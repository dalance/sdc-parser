# Copyright (c) 2016 Intel Corporation
# SPDX-License-Identifier: MIT

# 50MHz board input clock
create_clock -period 20 [get_ports FPGA_CLK1_50]
create_clock -period 20 [get_ports FPGA_CLK2_50]
create_clock -period 20 [get_ports FPGA_CLK3_50]

# for enhancing USB BlasterII to be reliable, 25MHz
create_clock -name {altera_reserved_tck} -period 40 {altera_reserved_tck}
set_input_delay -clock altera_reserved_tck -clock_fall 3 [get_ports altera_reserved_tdi]
set_input_delay -clock altera_reserved_tck -clock_fall 3 [get_ports altera_reserved_tms]
set_output_delay -clock altera_reserved_tck 3 [get_ports altera_reserved_tdo]

# FPGA IO port constraints
derive_pll_clocks
derive_clock_uncertainty

set_false_path -from [get_ports {SW[0]}] -to *
set_false_path -from [get_ports {SW[1]}] -to *
set_false_path -from [get_ports {SW[2]}] -to *
set_false_path -from [get_ports {SW[3]}] -to *
set_false_path -from * -to [get_ports {LED[0]}]
set_false_path -from * -to [get_ports {LED[1]}]
set_false_path -from * -to [get_ports {LED[2]}]
set_false_path -from * -to [get_ports {LED[3]}]
set_false_path -from * -to [get_ports {LED[4]}]
set_false_path -from * -to [get_ports {LED[5]}]
set_false_path -from * -to [get_ports {LED[6]}]
set_false_path -from * -to [get_ports {LED[7]}]
set_false_path -from [get_ports {KEY[0]}] -to *
set_false_path -from [get_ports {KEY[1]}] -to *
set_false_path -from [get_ports {GPIO_0[*]}] -to *
set_false_path -from [get_ports {GPIO_1[*]}] -to *
set_false_path -from * -to [get_ports {GPIO_0[*]}]
set_false_path -from * -to [get_ports {GPIO_1[*]}]

# ADC
set_false_path -from [get_ports {ADC_CONVST}] -to *
set_false_path -from [get_ports {ADC_SCK}] -to *
set_false_path -from [get_ports {ADC_SDI}] -to *
set_false_path -from [get_ports {ADC_SDO}] -to *
set_false_path -from * -to [get_ports {ADC_CONVST}] 
set_false_path -from * -to [get_ports {ADC_SCK}] 
set_false_path -from * -to [get_ports {ADC_SDI}] 
set_false_path -from * -to [get_ports {ADC_SDO}] 

# HPS peripherals port false path setting to workaround the
# unconstraint path (setting false_path for hps_0 ports will
# not affect the routing as it is hard silicon)
set_false_path -from * -to [get_ports {HPS_ENET_GTX_CLK}]
set_false_path -from * -to [get_ports {HPS_ENET_TX_DATA[0]}]
set_false_path -from * -to [get_ports {HPS_ENET_TX_DATA[1]}]
set_false_path -from * -to [get_ports {HPS_ENET_TX_DATA[2]}]
set_false_path -from * -to [get_ports {HPS_ENET_TX_DATA[3]}]
set_false_path -from * -to [get_ports {HPS_ENET_MDC}]
set_false_path -from * -to [get_ports {HPS_ENET_TX_EN}]
set_false_path -from * -to [get_ports {HPS_SD_CLK}]
set_false_path -from * -to [get_ports {HPS_USB_STP}]
set_false_path -from * -to [get_ports {HPS_SPIM_CLK}]
set_false_path -from * -to [get_ports {HPS_SPIM_MOSI}]
set_false_path -from * -to [get_ports {HPS_SPIM_SS}]
set_false_path -from * -to [get_ports {HPS_UART_TX}]

set_false_path -from * -to [get_ports {HPS_ENET_MDIO}]
set_false_path -from * -to [get_ports {HPS_SD_CMD}]
set_false_path -from * -to [get_ports {HPS_SD_DATA[0]}]
set_false_path -from * -to [get_ports {HPS_SD_DATA[1]}]
set_false_path -from * -to [get_ports {HPS_SD_DATA[2]}]
set_false_path -from * -to [get_ports {HPS_SD_DATA[3]}]
set_false_path -from * -to [get_ports {HPS_USB_DATA[0]}]
set_false_path -from * -to [get_ports {HPS_USB_DATA[1]}]
set_false_path -from * -to [get_ports {HPS_USB_DATA[2]}]
set_false_path -from * -to [get_ports {HPS_USB_DATA[3]}]
set_false_path -from * -to [get_ports {HPS_USB_DATA[4]}]
set_false_path -from * -to [get_ports {HPS_USB_DATA[5]}]
set_false_path -from * -to [get_ports {HPS_USB_DATA[6]}]
set_false_path -from * -to [get_ports {HPS_USB_DATA[7]}]
set_false_path -from * -to [get_ports {HPS_I2C0_SDAT}]
set_false_path -from * -to [get_ports {HPS_I2C0_SCLK}]
set_false_path -from * -to [get_ports {HPS_I2C1_SDAT}]
set_false_path -from * -to [get_ports {HPS_I2C1_SCLK}]
set_false_path -from * -to [get_ports {HPS_CONV_USB_N}]
set_false_path -from * -to [get_ports {HPS_ENET_INT_N}]
set_false_path -from * -to [get_ports {HPS_LTC_GPIO}]
set_false_path -from * -to [get_ports {HPS_LED}]
set_false_path -from * -to [get_ports {HPS_KEY}]
set_false_path -from * -to [get_ports {HPS_GSENSOR_INT}]

set_false_path -from [get_ports {HPS_ENET_MDIO}] -to *
set_false_path -from [get_ports {HPS_SD_CMD}] -to *
set_false_path -from [get_ports {HPS_SD_DATA[0]}] -to *
set_false_path -from [get_ports {HPS_SD_DATA[1]}] -to *
set_false_path -from [get_ports {HPS_SD_DATA[2]}] -to *
set_false_path -from [get_ports {HPS_SD_DATA[3]}] -to *
set_false_path -from [get_ports {HPS_USB_DATA[0]}] -to *
set_false_path -from [get_ports {HPS_USB_DATA[1]}] -to *
set_false_path -from [get_ports {HPS_USB_DATA[2]}] -to *
set_false_path -from [get_ports {HPS_USB_DATA[3]}] -to *
set_false_path -from [get_ports {HPS_USB_DATA[4]}] -to *
set_false_path -from [get_ports {HPS_USB_DATA[5]}] -to *
set_false_path -from [get_ports {HPS_USB_DATA[6]}] -to *
set_false_path -from [get_ports {HPS_USB_DATA[7]}] -to *
set_false_path -from [get_ports {HPS_I2C0_SDAT}] -to *
set_false_path -from [get_ports {HPS_I2C0_SCLK}] -to *
set_false_path -from [get_ports {HPS_I2C1_SDAT}] -to *
set_false_path -from [get_ports {HPS_I2C1_SCLK}] -to *
set_false_path -from [get_ports {HPS_CONV_USB_N}] -to *
set_false_path -from [get_ports {HPS_ENET_INT_N}] -to *
set_false_path -from [get_ports {HPS_LTC_GPIO}] -to *
set_false_path -from [get_ports {HPS_LED}] -to *
set_false_path -from [get_ports {HPS_KEY}] -to *
set_false_path -from [get_ports {HPS_GSENSOR_INT}] -to *

set_false_path -from [get_ports {HPS_ENET_RX_DV}] -to *
set_false_path -from [get_ports {HPS_ENET_RX_CLK}] -to *
set_false_path -from [get_ports {HPS_ENET_RX_DATA[0]}] -to *
set_false_path -from [get_ports {HPS_ENET_RX_DATA[1]}] -to *
set_false_path -from [get_ports {HPS_ENET_RX_DATA[2]}] -to *
set_false_path -from [get_ports {HPS_ENET_RX_DATA[3]}] -to *
set_false_path -from [get_ports {HPS_USB_CLKOUT}] -to *
set_false_path -from [get_ports {HPS_USB_DIR}] -to *
set_false_path -from [get_ports {HPS_USB_NXT}] -to *
set_false_path -from [get_ports {HPS_SPIM_MISO}] -to *
set_false_path -from [get_ports {HPS_SPIM_SS}] -to *
set_false_path -from [get_ports {HPS_UART_RX}] -to *

# create unused clock constraint for HPS I2C and usb1
# to avoid misleading unconstraint clock reporting in TimeQuest
create_clock -period "1 MHz" [get_ports HPS_I2C0_SCLK]
create_clock -period "1 MHz" [get_ports HPS_I2C1_SCLK]
create_clock -period "48 MHz" [get_ports HPS_USB_CLKOUT]


#
# Arduino IO
#

# ALT_IOBUF arduino_scl_iobuf (.i(1'b0), .oe(arduino_internal_scl_o_e), .o(arduino_internal_scl_o), .io(arduino_io[15]));
# ALT_IOBUF arduino_sda_iobuf (.i(1'b0), .oe(arduino_internal_sda_o_e), .o(arduino_internal_sda_o), .io(arduino_io[14]));
set_false_path -from [get_ports ARDUINO_IO[15]] -to *
set_false_path -from * -to [get_ports ARDUINO_IO[15]]
set_false_path -from [get_ports ARDUINO_IO[14]] -to *
set_false_path -from * -to [get_ports ARDUINO_IO[14]]

# ALT_IOBUF arduino_uart_rx_iobuf (.i(1'b0), .oe(1'b0), .o(arduino_hps_0_uart1_rxd), .io(arduino_io[0]));
# ALT_IOBUF arduino_uart_tx_iobuf (.i(arduino_hps_0_uart1_txd), .oe(1'b1), .o(), .io(arduino_io[1]));
set_false_path -from [get_ports ARDUINO_IO[0]] -to *
set_false_path -from * -to [get_ports ARDUINO_IO[1]]

# ALT_IOBUF arduino_ss_iobuf (.i(arduino_hps_0_spim0_ss_0_n), .oe(!arduino_hps_0_spim0_ssi_oe_n), .o(arduino_hps_0_spim0_ss_in_n), .io(arduino_io[10]));
# ALT_IOBUF arduino_mosi_iobuf (.i(arduino_hps_0_spim0_txd), .oe(1'b1), .o(), .io(arduino_io[11]));
# ALT_IOBUF arduino_miso_iobuf (.i(1'b0), .oe(1'b0), .o(arduino_hps_0_spim0_rxd), .io(arduino_io[12]));
# ALT_IOBUF arduino_sck_iobuf (.i(arduino_hps_0_spim0_sclk_out_clk), .oe(1'b1), .o(), .io(arduino_io[13]));
set_false_path -from [get_ports ARDUINO_IO[10]] -to *
set_false_path -from * -to [get_ports ARDUINO_IO[10]]
set_false_path -from * -to [get_ports ARDUINO_IO[11]]
set_false_path -from [get_ports ARDUINO_IO[12]] -to *
set_false_path -from * -to [get_ports ARDUINO_IO[13]]

# .arduino_gpio_export              (arduino_io[9:2]),
set_false_path -from [get_ports ARDUINO_IO[2]] -to *
set_false_path -from * -to [get_ports ARDUINO_IO[2]]
set_false_path -from [get_ports ARDUINO_IO[3]] -to *
set_false_path -from * -to [get_ports ARDUINO_IO[3]]
set_false_path -from [get_ports ARDUINO_IO[4]] -to *
set_false_path -from * -to [get_ports ARDUINO_IO[4]]
set_false_path -from [get_ports ARDUINO_IO[5]] -to *
set_false_path -from * -to [get_ports ARDUINO_IO[5]]
set_false_path -from [get_ports ARDUINO_IO[6]] -to *
set_false_path -from * -to [get_ports ARDUINO_IO[6]]
set_false_path -from [get_ports ARDUINO_IO[7]] -to *
set_false_path -from * -to [get_ports ARDUINO_IO[7]]
set_false_path -from [get_ports ARDUINO_IO[8]] -to *
set_false_path -from * -to [get_ports ARDUINO_IO[8]]
set_false_path -from [get_ports ARDUINO_IO[9]] -to *
set_false_path -from * -to [get_ports ARDUINO_IO[9]]

# assign arduino_reset_n = hps_fpga_reset_n;
set_false_path -from * -to [get_ports ARDUINO_RESET_N]

