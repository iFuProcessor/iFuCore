import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestTemplate extends AnyFlatSpec with ChiselScalatestTester {
    "DUT" should "pass" in {
        test(new DeviceUnderTest) { dut =>
            println("Testing DUT")
        }
    }
}
