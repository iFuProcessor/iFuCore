package ram

import chisel3._

class xpm_memory_sdpram(ADDR_WIDTH: Int, DATA_WIDTH: Int, BYTE_WIDTH: Int) extends BlackBox (Map(
    "ADDR_WIDTH_A" -> ADDR_WIDTH,
    "ADDR_WIDTH_B" -> ADDR_WIDTH,
    "AUTO_SLEEP_TIME" -> 0,
    "BYTE_WRITE_WIDTH_A" -> BYTE_WIDTH,
    "ECC_MODE" -> "no_ecc",
    "MEMORY_INIT_FILE" -> "none",
    "MEMORY_INIT_PARAM" -> "0",
    "MEMORY_OPTIMIZATION" -> "true",
    "MEMORY_PRIMITIVE" -> "auto",
    "MEMORY_SIZE" -> (math.pow(2, ADDR_WIDTH) * DATA_WIDTH).toInt,
    "MESSAGE_CONTROL" -> 0,
    "READ_DATA_WIDTH_B" -> DATA_WIDTH,
    "READ_LATENCY_B" -> 1,
    "READ_RESET_VALUE_B" -> "0",
    "RST_MODE_A" -> "SYNC",
    "RST_MODE_B" -> "SYNC",
    "USE_MEM_INIT" -> 0,
    "WAKEUP_TIME" -> "disable_sleep",
    "WRITE_DATA_WIDTH_A" -> DATA_WIDTH,
    "WRITE_MODE_B" -> "no_change"
)) {
    val io = IO(new Bundle {
        val clka = Input(Bool())
        val clkb = Input(Bool())
        val ena = Input(Bool())
        val enb = Input(Bool())
        val addra = Input(UInt(ADDR_WIDTH.W))
        val addrb = Input(UInt(ADDR_WIDTH.W))
        val wea = Input(UInt((DATA_WIDTH / BYTE_WIDTH).W))
        val dina = Input(UInt(DATA_WIDTH.W))
        val doutb = Output(UInt(DATA_WIDTH.W))
        val regceb = Input(Bool())
        val rstb = Input(Bool())
        val sleep = Input(Bool())
        val injectdbiterra = Input(Bool())
        val injectsbiterra = Input(Bool())
    })
}
