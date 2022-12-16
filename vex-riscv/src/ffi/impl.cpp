// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "VVexRiscv.h"
#include "verilated.h"
#include "interface.h"
#include <stdio.h>

extern "C" {
  VVexRiscv* vexr_init();
  void vexr_shutdown(VVexRiscv *top);
  void vexr_step_first(VVexRiscv *top, const INPUT *input, OUTPUT *output);
  void vexr_step_second(VVexRiscv *top, const INPUT *input, OUTPUT *output);
}


VVexRiscv* vexr_init()
{
  return new VVexRiscv();
}

void vexr_shutdown(VVexRiscv *top)
{
  delete top;
}

static void print_input(const INPUT *input)
{
  printf("reset = %d\n", input->reset);
  printf("timerInterrupt = %d\n", input->timerInterrupt);
  printf("externalInterrupt = %d\n", input->externalInterrupt);
  printf("softwareInterrupt = %d\n", input->softwareInterrupt);
  printf("iBusWishbone_ACK = %d\n", input->iBusWishbone_ACK);
  printf("iBusWishbone_DAT_MISO = %X\n", input->iBusWishbone_DAT_MISO);
  printf("iBusWishbone_ERR = %d\n", input->iBusWishbone_ERR);
  printf("dBusWishbone_ACK = %d\n", input->dBusWishbone_ACK);
  printf("dBusWishbone_DAT_MISO = %X\n", input->dBusWishbone_DAT_MISO);
  printf("dBusWishbone_ERR = %d\n", input->dBusWishbone_ERR);
}

void vexr_step_first(VVexRiscv *top, const INPUT *input, OUTPUT *output)
{
  // set inputs
  top->reset = input->reset;
  top->timerInterrupt = input->timerInterrupt;
  top->externalInterrupt = input->externalInterrupt;
  top->softwareInterrupt = input->softwareInterrupt;
  top->iBusWishbone_ACK = input->iBusWishbone_ACK;
  top->iBusWishbone_DAT_MISO = input->iBusWishbone_DAT_MISO;
  top->iBusWishbone_ERR = input->iBusWishbone_ERR;
  top->dBusWishbone_ACK = input->dBusWishbone_ACK;
  top->dBusWishbone_DAT_MISO = input->dBusWishbone_DAT_MISO;
  top->dBusWishbone_ERR = input->dBusWishbone_ERR;

  // print_input(input);

  // rising edge
  top->clk = true;
  top->eval();

  // update outputs
  output->iBusWishbone_CYC = top->iBusWishbone_CYC;
  output->iBusWishbone_STB = top->iBusWishbone_STB;
  output->iBusWishbone_WE = top->iBusWishbone_WE;
  output->iBusWishbone_ADR = top->iBusWishbone_ADR;
  output->iBusWishbone_DAT_MOSI = top->iBusWishbone_DAT_MOSI;
  output->iBusWishbone_SEL = top->iBusWishbone_SEL;
  output->iBusWishbone_CTI = top->iBusWishbone_CTI;
  output->iBusWishbone_BTE = top->iBusWishbone_BTE;
  output->dBusWishbone_CYC = top->dBusWishbone_CYC;
  output->dBusWishbone_STB = top->dBusWishbone_STB;
  output->dBusWishbone_WE = top->dBusWishbone_WE;
  output->dBusWishbone_ADR = top->dBusWishbone_ADR;
  output->dBusWishbone_DAT_MOSI = top->dBusWishbone_DAT_MOSI;
  output->dBusWishbone_SEL = top->dBusWishbone_SEL;
  output->dBusWishbone_CTI = top->dBusWishbone_CTI;
  output->dBusWishbone_BTE = top->dBusWishbone_BTE;
}

void vexr_step_second(VVexRiscv *top, const INPUT *input, OUTPUT *output)
{
  // set inputs
  top->reset = input->reset;
  top->timerInterrupt = input->timerInterrupt;
  top->externalInterrupt = input->externalInterrupt;
  top->softwareInterrupt = input->softwareInterrupt;
  top->iBusWishbone_ACK = input->iBusWishbone_ACK;
  top->iBusWishbone_DAT_MISO = input->iBusWishbone_DAT_MISO;
  top->iBusWishbone_ERR = input->iBusWishbone_ERR;
  top->dBusWishbone_ACK = input->dBusWishbone_ACK;
  top->dBusWishbone_DAT_MISO = input->dBusWishbone_DAT_MISO;
  top->dBusWishbone_ERR = input->dBusWishbone_ERR;

  // print_input(input);

  top->clk = false;
  top->eval();

  // update outputs
  output->iBusWishbone_CYC = top->iBusWishbone_CYC;
  output->iBusWishbone_STB = top->iBusWishbone_STB;
  output->iBusWishbone_WE = top->iBusWishbone_WE;
  output->iBusWishbone_ADR = top->iBusWishbone_ADR;
  output->iBusWishbone_DAT_MOSI = top->iBusWishbone_DAT_MOSI;
  output->iBusWishbone_SEL = top->iBusWishbone_SEL;
  output->iBusWishbone_CTI = top->iBusWishbone_CTI;
  output->iBusWishbone_BTE = top->iBusWishbone_BTE;
  output->dBusWishbone_CYC = top->dBusWishbone_CYC;
  output->dBusWishbone_STB = top->dBusWishbone_STB;
  output->dBusWishbone_WE = top->dBusWishbone_WE;
  output->dBusWishbone_ADR = top->dBusWishbone_ADR;
  output->dBusWishbone_DAT_MOSI = top->dBusWishbone_DAT_MOSI;
  output->dBusWishbone_SEL = top->dBusWishbone_SEL;
  output->dBusWishbone_CTI = top->dBusWishbone_CTI;
  output->dBusWishbone_BTE = top->dBusWishbone_BTE;
}
