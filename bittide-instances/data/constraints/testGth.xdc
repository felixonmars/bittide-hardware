# SPDX-FileCopyrightText: 2022-2023 Google LLC
#
# SPDX-License-Identifier: Apache-2.0



set_property BOARD_PART_PIN sysclk_300_n [get_ports SYSCLK_300_N]
set_property BOARD_PART_PIN sysclk_300_p [get_ports SYSCLK_300_P]


set_property BOARD_PART_PIN CPU_RESET [get_ports CPU_RESET]

set_property BOARD_PART_PIN sma_mgt_refclk_n [get_ports SMA_MGT_REFCLK_C_N]
set_property BOARD_PART_PIN sma_mgt_refclk_p [get_ports SMA_MGT_REFCLK_C_P]

set_property BOARD_PART_PIN GPIO_SW_E [get_ports GPIO_SW_E]
set_property BOARD_PART_PIN GPIO_SW_N [get_ports GPIO_SW_N]
set_property BOARD_PART_PIN GPIO_SW_C [get_ports GPIO_SW_C]


set_property PACKAGE_PIN H27 [get_ports USER_SMA_GPIO_P]
set_property IOSTANDARD LVCMOS18 [get_ports USER_SMA_GPIO_P]

set_property PACKAGE_PIN G27 [get_ports USER_SMA_GPIO_N]
set_property IOSTANDARD LVCMOS18 [get_ports USER_SMA_GPIO_N]


set_property BOARD_PART_PIN GPIO_LED_1_LS [get_ports rx_data_good[6]]
set_property BOARD_PART_PIN GPIO_LED_2_LS [get_ports rx_data_good[5]]
set_property BOARD_PART_PIN GPIO_LED_3_LS [get_ports rx_data_good[4]]
set_property BOARD_PART_PIN GPIO_LED_4_LS [get_ports rx_data_good[3]]
set_property BOARD_PART_PIN GPIO_LED_5_LS [get_ports rx_data_good[2]]
set_property BOARD_PART_PIN GPIO_LED_6_LS [get_ports rx_data_good[1]]
set_property BOARD_PART_PIN GPIO_LED_7_LS [get_ports rx_data_good[0]]






#  Color   | FPGA pin      | Connection
#  --------|---------------|-----------
#  Orange  | PMOD1_0       | MISO/SDO
#  Green   | PMOD1_1       | CSb
#  Purple  | PMOD1_2       | FDEC
#  White   | PMOD1_3       | SWCLK
#  Red     | PMOD1_4       | SCLK
#  Yellow  | PMOD1_5       | MOSI/SDIO
#  Blue    | PMOD1_6       | FINC
#  Grey    | PMOD1_7       | SWDIO
#  Black   | GND           | GND (SWD)
#  Brown   | GND           | GND (SPI)
#
# The data wire of the external reset button is connected to PMOD0_3.

# PMOD1_[0..7]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AL14} [get_ports {CLK_MISO}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM14} [get_ports {CLK_CSB}]
#set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AP16} [get_ports {FDEC}]
#set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AP15} [get_ports {SWCLK}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM16} [get_ports {CLK_SCLK}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM15} [get_ports {CLK_MOSI}]
#set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AN18} [get_ports {FINC}]
#set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AN17} [get_ports {SWDIO}]


# PMOD0_3
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM19} [get_ports {shared_reset_btn}]





# set_property PACKAGE_PIN AL14      [get_ports "PMOD1_0_LS"]
# set_property PACKAGE_PIN AM14      [get_ports "PMOD1_1_LS"]
# set_property PACKAGE_PIN AP16      [get_ports "PMOD1_2_LS"]
# set_property PACKAGE_PIN AP15      [get_ports "PMOD1_3_LS"]
# set_property PACKAGE_PIN AM16      [get_ports "PMOD1_4_LS"]
# set_property PACKAGE_PIN AM15      [get_ports "PMOD1_5_LS"]
# set_property PACKAGE_PIN AN18      [get_ports "PMOD1_6_LS"]
# set_property PACKAGE_PIN AN17      [get_ports "PMOD1_7_LS"]
#