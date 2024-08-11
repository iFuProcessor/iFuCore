package iFu.common

import chisel3._

abstract class CoreModule extends Module with HasCoreParameters

abstract class CoreBundle extends Bundle with HasCoreParameters
