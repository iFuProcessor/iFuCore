
package iFu.backend

import chisel3._
import chisel3.util._

import iFu.common._
import iFu.common.Consts._
import iFu.util._


class MetaLine extends CoreBundle with HasDcacheParameters {
    val valid = Bool()
    val dirty = Bool()
    val readOnly = Bool()
    val fixed = Bool()
    val tag   = UInt(nTagBits.W)
    // No age here, it is preserved at MetaLogic , because we need 
    // hit or miss to decide how to update age
}


class DcacheMetaReq extends CoreBundle with HasDcacheParameters{
    val idx    = UInt(nIdxBits.W)
    // 读的时候用于匹配
    val tag    = UInt(nTagBits.W)
    // 读的时候的路号,写回时候的路号
    val pos = UInt(log2Ceil(nWays).W)

    // 要写回的行首信息
    val setvalid = Valid(Bool())
    val setdirty = Valid(Bool())
    val setfixed = Valid(Bool())
    val setreadOnly = Valid(Bool())
    // 写的时候作为新行的tag
    val setTag = Valid(UInt(nTagBits.W))


    // 用于判断是否是lsu的store指令,判断readOnly
    val isStore = Bool()
    // 对于fence指令需要自己找脏行
    val isFence = Bool()

    val cacop_type = UInt(2.W)

}

// different with MetaResp, it return a single line
class DcacheMetaResp extends CoreBundle with HasDcacheParameters{
    val rmeta  = new MetaLine
    // 如果一个store命中了一个readOnly，就认为是miss
    val hit = Bool()
    // 指定接下来传到Data的idx , pos信息
    val idx = UInt(nIdxBits.W)
    val pos = UInt(log2Ceil(nWays).W)

}

class DcacheMetaLogic extends Module with HasDcacheParameters{
    val io = IO(new CoreBundle{ 
        val lsuRead     = Input(Vec( memWidth ,Valid(new DcacheMetaReq)))
        val lsuWrite    = Input(Valid(new DcacheMetaReq))

        // 计算并拿到被替换的行，紧接着会发给总线那边
        val missReplace  = Input(Valid(new DcacheMetaReq))

        // axi的写回新行请求
        val wfuWrite    = Input(Valid(new DcacheMetaReq))
        // mshr 送给wfu一行去wb，期间该行不能被修改
        val replaceLogout = Input(Valid(new DcacheMetaReq))

        // replay的读请求,用于mshr的replay
        val replayRead  = Input(Valid(new DcacheMetaReq))
        // replay的写请求,用于mshr的replay
        val replayWrite = Input(Valid(new DcacheMetaReq))

        // fence
        val fetchDirty   = Input(Valid(new DcacheMetaReq))
        val lineClear  = Input(Valid(new DcacheMetaReq))

        val cacopRead   = Input(Valid(new DcacheMetaReq))
        val cacopWrite  = Input(Valid(new DcacheMetaReq))

        val readResp = Output(Vec(memWidth, Valid(new DcacheMetaResp)))
        val writeResp = Output(Valid(new DcacheMetaResp))

        val hasDirty = Output(Bool())
    })

    
    for(w <- 0 until memWidth){
        io.readResp(w) := 0.U.asTypeOf(Valid(new DcacheMetaResp))
    }
    io.writeResp := 0.U.asTypeOf(Valid(new DcacheMetaResp))


    val meta = Module(new DcacheMeta)
    // age在此维护
    val ages = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(0.U(nAgeBits.W))))))

    // LRU
    val lru_pos = RegInit(VecInit(Seq.fill(nSets)((0.U(log2Ceil(nWays).W)))))

    val debug_clock = RegInit(0.U(32.W))
    if(!FPGAPlatform)dontTouch(io)
    debug_clock := debug_clock + 1.U

    val lsu_R :: lsu_W :: miss_R  :: axi_W :: line_F :: replay_R :: replay_W :: fence_R :: fence_W :: cacop_R :: cacop_W :: none :: Nil = Enum(12)
    
    //read
    val haslsuRead = io.lsuRead.map(_.valid).reduce(_|_)

    val readType = Wire(UInt(4.W))
    val readValid = Wire(Vec(memWidth, Bool()))
    val readReq = Wire(Vec(memWidth, new DcacheMetaReq))
    val readIdx = Wire(Vec(memWidth, UInt(nIdxBits.W)))
    val readTag = Wire(Vec(memWidth, UInt(nTagBits.W)))
    val readPos = Wire(Vec(memWidth, UInt(log2Ceil(nWays).W)))

    // lsureadinfo
    val hitoh = Wire(Vec(memWidth, UInt(nWays.W)))

    if(!FPGAPlatform)dontTouch(readType)
    if(!FPGAPlatform)dontTouch(readIdx)
    if(!FPGAPlatform)dontTouch(readPos)
    if(!FPGAPlatform)dontTouch(readTag)
    if(!FPGAPlatform)dontTouch(hitoh)

    for(w <- 0 until memWidth){
        readType := Mux(io.missReplace.valid, miss_R,
                    Mux(io.replayRead.valid, replay_R,
                    Mux(io.fetchDirty.valid, fence_R,
                    Mux(io.cacopRead.valid, cacop_R,
                                                lsu_R))))

        readValid(w) := io.lsuRead(w).valid ||
                        io.missReplace.valid ||
                        io.replayRead.valid ||
                        io.fetchDirty.valid ||
                        io.cacopRead.valid

        readReq(w) :=   Mux(io.missReplace.valid, io.missReplace.bits,
                        Mux(io.fetchDirty.valid, io.fetchDirty.bits,
                                                    io.lsuRead(w).bits))

        readIdx(w) :=   Mux(io.missReplace.valid, io.missReplace.bits.idx,
                        Mux(io.replayRead.valid, io.replayRead.bits.idx,
                                                    io.lsuRead(w).bits.idx))

       // only replayRead need pos set dirty (nessary ?)
        readPos(w) :=    io.replayRead.bits.pos

        // only lsuRead and cacopRead need tag 
        readTag(w) := Mux(io.lsuRead(w).valid, io.lsuRead(w).bits.tag,
                        Mux(io.cacopRead.valid, io.cacopRead.bits.tag,
                                                    0.U(nTagBits.W)))
                        
        
    }

    for(w<- 0 until memWidth){

        meta.io.read(w).req.valid := readValid(w)
        meta.io.read(w).req.bits := readReq(w)

        // lsu read
        hitoh(w) := 0.U
    }


    // 一些信息的s2内部转发
    val s2Dirval = RegInit(false.B)
    val s2Diridx = RegInit(0.U(nIdxBits.W))
    val s2Dirpos = RegInit(0.U(log2Ceil(nWays).W))

    dontTouch(s2Dirval)
    dontTouch(s2Diridx)
    dontTouch(s2Dirpos)

    s2Dirval := false.B
    s2Diridx := 0.U
    s2Dirpos := 0.U

    val lsu_readResp =  0.U.asTypeOf(Vec(memWidth, Valid(new DcacheMetaResp)))
    val miss_readResp = 0.U.asTypeOf(Vec(memWidth, Valid(new DcacheMetaResp)))
    val fence_readResp = 0.U.asTypeOf(Vec(memWidth, Valid(new DcacheMetaResp)))
    
    for(w <- 0 until memWidth){
        val rmetaSet = meta.io.read(w).resp.bits.rmetaSet
        val dirtyIdx = meta.io.read(w).resp.bits.dirtyIdx
        val dirtyPos = meta.io.read(w).resp.bits.dirtyPos
        val rpos = RegNext(readPos(w))
        val ridx = RegNext(readIdx(w))
        
        val isLsuStore = RegNext(io.lsuRead(w).bits.isStore && io.lsuRead(w).valid)
        val isReplayStore = RegNext(io.replayRead.bits.isStore && io.replayRead.valid)
//-----------------lsu_R-----------------
            // isLsuStore用来触发Store的命中机制
            val rtag = RegNext(readTag(w))// no need RegNext(readTag(w)) , because  the tag is from io.lsu.s1_paddr
            hitoh(w) := VecInit(rmetaSet.map((x: MetaLine) => x.valid && x.tag === rtag)).asUInt
            assert(PopCount(hitoh(w)) <= 1.U,"At most one hit")
            val hitpos = PriorityEncoder(hitoh(w))
            //  如果是被refill第一个字报废的行，此时算未命中(由于现在在mshr_replace就一定会报废掉这一行，之后没必要wb refill 报废了)
            lsu_readResp(w).bits.hit := hitoh(w).orR
            lsu_readResp(w).bits.pos := hitpos
            lsu_readResp(w).bits.rmeta := rmetaSet(hitpos)

            when(hitoh(w).orR && isLsuStore){
                // 如果是一个命中的store指令，存s2Dir信息，以防来不及告诉下个周期的missReplace这里脏了
                s2Dirval := true.B
                s2Diridx := ridx
                s2Dirpos := hitpos
            }
// -----------------replay_R-----------------
            // isLsuStore用来触发Store的命中机制
            when(isReplayStore){
                // 如果是一个replay的store指令，存s2Dir信息，以防来不及告诉下个周期的missReplace这里脏了
                s2Dirval := true.B
                s2Diridx := ridx
                s2Dirpos := rpos
            }

//-----------------miss_R-----------------
            val isfull = rmetaSet.map(_.valid).reduce(_ && _)
            // 读到的是一个set，里面有nWays个age
            val rAgeSet = ages(ridx)

            // 当前的替换策略暂时设置为，寻找valid为假且fixed为假的行
            val invalidPos =  PriorityEncoder(VecInit(rmetaSet.map((x: MetaLine) => !x.valid && !x.fixed)).asUInt)
            // 否则从fixed为假的行中挑一个
            // (暂时没设置真的youngest)
            // val rand_clk_pos = PriorityEncoder(VecInit(rmetaSet.map((x: MetaLine) => !x.fixed)).asUInt)

            // 这是随机替换策略
            val rand_clk_pos = debug_clock(log2Ceil(nWays)-1,0)
            // LRU 替换策略
            val lru_repl_pos = lru_pos(ridx)
            //满了取替换，没满取无效
            val replacePos = Mux(isfull, rand_clk_pos , invalidPos)

            // 得以替换的路
            miss_readResp(w).bits.pos := replacePos
            // 可能读到的是有效的，也可能是无效的行 ,送到axi那边自行判断要不要先写回总线
            miss_readResp(w).bits.rmeta := rmetaSet(replacePos)
            // 做快速dirty的转发
            when(s2Dirval && s2Diridx === ridx && s2Dirpos === replacePos){
                miss_readResp(w).bits.rmeta.dirty := true.B
            }
//-----------------fence_R-----------------
            // fence 从返回对应脏行的meta和位置信息
            fence_readResp(w).bits.rmeta := rmetaSet(dirtyPos)
            fence_readResp(w).bits.hit := DontCare
            fence_readResp(w).bits.idx := dirtyIdx
            fence_readResp(w).bits.pos := dirtyPos
        }

    for(w <- 0 until memWidth){
        io.readResp(w).valid := meta.io.read(w).resp.valid
        // idx怎么进来的，就怎么出去 
        io.readResp(w).bits.idx := RegNext(readIdx(w))
        // 其他项根据情况在下面配置
        switch(RegNext(readType)){
            is(lsu_R){
                io.readResp(w).bits.hit := lsu_readResp(w).bits.hit
                io.readResp(w).bits.pos := lsu_readResp(w).bits.pos
                io.readResp(w).bits.rmeta := lsu_readResp(w).bits.rmeta

                // 更新lru
                when(io.readResp(w).valid && io.readResp(w).bits.hit){
                    lru_pos(readIdx(w)) := io.readResp(w).bits.pos + 1.U
                }
            }
            is(miss_R){
                io.readResp(w).bits.pos := miss_readResp(w).bits.pos
                io.readResp(w).bits.rmeta := miss_readResp(w).bits.rmeta
            }
            is(replay_R){
                // do nothing
            }
            is(fence_R){
                io.readResp(w).bits.idx := fence_readResp(w).bits.idx
                io.readResp(w).bits.pos := fence_readResp(w).bits.pos
                io.readResp(w).bits.rmeta := fence_readResp(w).bits.rmeta
            }
        }
    }


    // 返回hasDirty给外界
    io.hasDirty := meta.io.hasDirty


    //write
    //这个resp和meta数据的resp类型有区别
    val writeResp = Wire(Valid(new DcacheMetaResp))
    writeResp := 0.U.asTypeOf(Valid(new DcacheMetaResp))

    val writeValid = io.lsuWrite.valid ||
                     io.wfuWrite.valid ||
                     io.replaceLogout.valid ||
                     io.replayWrite.valid ||
                     io.lineClear.valid ||
                     io.cacopWrite.valid

    val writeType   = Mux(io.lsuWrite.valid,lsu_W,
                    Mux(io.wfuWrite.valid,axi_W,
                    Mux(io.replaceLogout.valid,line_F,
                    Mux(io.replayWrite.valid,replay_W,
                    Mux(io.lineClear.valid,fence_W,
                    Mux(io.cacopWrite.valid,cacop_W,
                                                none))))))
    val writeReq    = Mux(io.lsuWrite.valid, io.lsuWrite.bits,
                    Mux(io.wfuWrite.valid, io.wfuWrite.bits,
                    Mux(io.replaceLogout.valid, io.replaceLogout.bits,
                    Mux(io.replayWrite.valid, io.replayWrite.bits,
                    Mux(io.lineClear.valid, io.lineClear.bits,
                    Mux(io.cacopWrite.valid, io.cacopWrite.bits,
                                                0.U.asTypeOf(new DcacheMetaReq)))))))

    // write 的行为由传入方定义好了,因此此处统一正常读写就好
    meta.io.write.req.valid := writeValid
    meta.io.write.req.bits := writeReq
    // 写meta没什么好返回的,返回一个valid就好
    io.writeResp.valid := meta.io.write.resp.valid

}
