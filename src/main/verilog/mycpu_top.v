module core_top (
    input           aclk            ,
    input           aresetn         ,
    input    [ 7:0] intrpt          ,
    //AXI interface
    //read reqest
    output   [ 3:0] arid            ,
    output   [31:0] araddr          ,
    output   [ 7:0] arlen           ,
    output   [ 2:0] arsize          ,
    output   [ 1:0] arburst         ,
    output   [ 1:0] arlock          ,
    output   [ 3:0] arcache         ,
    output   [ 2:0] arprot          ,
    output          arvalid         ,
    input           arready         ,
    //read back
    input    [ 3:0] rid             ,
    input    [31:0] rdata           ,
    input    [ 1:0] rresp           ,
    input           rlast           ,
    input           rvalid          ,
    output          rready          ,
    //write request
    output   [ 3:0] awid            ,
    output   [31:0] awaddr          ,
    output   [ 7:0] awlen           ,
    output   [ 2:0] awsize          ,
    output   [ 1:0] awburst         ,
    output   [ 1:0] awlock          ,
    output   [ 3:0] awcache         ,
    output   [ 2:0] awprot          ,
    output          awvalid         ,
    input           awready         ,
    //write data
    output   [ 3:0] wid             ,
    output   [31:0] wdata           ,
    output   [ 3:0] wstrb           ,
    output          wlast           ,
    output          wvalid          ,
    input           wready          ,
    //write back
    input    [ 3:0] bid             ,
    input    [ 1:0] bresp           ,
    input           bvalid          ,
    output          bready          ,

    // //debug
    input           break_point     ,
    input           infor_flag      ,
    input  [ 4:0]   reg_num         ,
    output          ws_valid        ,
    output [31:0]   rf_rdata        ,

    output [31:0] debug0_wb_pc      ,
    output [ 3:0] debug0_wb_rf_wen  ,
    output [ 4:0] debug0_wb_rf_wnum ,
    output [31:0] debug0_wb_rf_wdata
);
    reg reset;
    always @(posedge aclk) begin
        reset <= ~aresetn;
    end

    iFuCore core(
        .clock(aclk)                   ,
        .reset(reset)                  ,
        .io_ext_int(intrpt)            ,
        .io_axi3_ar_ready(arready)     ,
        .io_axi3_ar_valid(arvalid)     ,
        .io_axi3_ar_bits_id(arid)      ,
        .io_axi3_ar_bits_addr(araddr)  ,
        .io_axi3_ar_bits_len(arlen)    ,
        .io_axi3_ar_bits_size(arsize)  ,
        .io_axi3_ar_bits_burst(arburst),
        .io_axi3_ar_bits_lock(arlock)  ,
        .io_axi3_ar_bits_cache(arcache),
        .io_axi3_ar_bits_prot(arprot)  ,
        .io_axi3_r_ready(rready)       ,
        .io_axi3_r_valid(rvalid)       ,
        .io_axi3_r_bits_id(rid)        ,
        .io_axi3_r_bits_resp(rresp)    ,
        .io_axi3_r_bits_data(rdata)    ,
        .io_axi3_r_bits_last(rlast)    ,
        .io_axi3_aw_ready(awready)     ,
        .io_axi3_aw_valid(awvalid)     ,
        .io_axi3_aw_bits_id(awid)      ,
        .io_axi3_aw_bits_addr(awaddr)  ,
        .io_axi3_aw_bits_len(awlen)    ,
        .io_axi3_aw_bits_size(awsize)  ,
        .io_axi3_aw_bits_burst(awburst),
        .io_axi3_aw_bits_lock(awlock)  ,
        .io_axi3_aw_bits_cache(awcache),
        .io_axi3_aw_bits_prot(awprot)  ,
        .io_axi3_w_ready(wready)       ,
        .io_axi3_w_valid(wvalid)       ,
        .io_axi3_w_bits_id(wid)        ,
        .io_axi3_w_bits_data(wdata)    ,
        .io_axi3_w_bits_last(wlast)    ,
        .io_axi3_w_bits_strb(wstrb)    ,
        .io_axi3_b_ready(bready)       ,
        .io_axi3_b_valid(bvalid)       ,
        .io_axi3_b_bits_id(bid)        ,
        .io_axi3_b_bits_resp(bresp)
    );

endmodule
