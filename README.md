# iFu

你说得对，但是《 **iFu** 》是由 **???** 自主编写的一款全新超标量乱序CPU。CPU运行在一个被称作「 **FPGA** 」的物理世界，在这里被 **SBT** 选中的 **Chisel Codes** 将被授予「 **编译运行** 」，引导 **仿真与综合** 之力。你将扮演一位名为「 **/\*TODO\*/** 」的神秘角色，在自由的 **LoongArch32** 中邂逅性格各异、能力独特的 **Instructions** ，和它们一起击败 **Func/Perf/Sys Test** ，找回 **性能** 的同时，逐步发掘「 **B+ = 3.3** 」的真相。

## 项目结构
```shell
repo
├── build.sbt
├── doc
├── LICENSE
├── Makefile
├── README.md
└── src
    ├── main
    │   ├── scala
    │   └── verilog
    └── test
        └── scala
```

## 环境配置
1. 参考`https://www.chisel-lang.org/docs/installation`安装`Javac`以及`SBT`。

## 编译运行
1. 修改`repo/Makfile`中的`gen_dir`为`system verilog`代码生成目录
2. 运行`make`命令，即可在`gen_dir`目录下生成`iFuCore.sv`文件
3. 将`repo/src/main/verilog/mycpu_top.v`复制到`gen_dir`目录下
4. 此时，整个项目对外暴露出一个`core_top`模块，其接口使用`AXI3`协议，另外还需传入`intrpt`信号，用于中断处理

## 仿真环境
1. 本项目仿真依赖于[chiplab](https://gitee.com/loongson-edu/chiplab)
2. 将`gen_dir`设置为`chiplab/IP/myCPU`
3. 将`repo/src/main/verilog/mycpu_top.v`复制到`chiplab/IP/myCPU`目录下
4. 完成上述步骤后，即可使用`chiplab`提供的仿真环境进行仿真

## 更多信息
[网站主页](https://ys.mihoyo.com/)
[资料下载](https://ys-api.mihoyo.com/event/download_porter/link/ys_cn/official/pc_default)
