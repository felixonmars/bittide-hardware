/*
SPDX-FileCopyrightText: 2022 Google LLC

SPDX-License-Identifier: CC0-1.0
*/

MEMORY
{
  DATA : ORIGIN = 0x40000000, LENGTH = 64K
  INSTR : ORIGIN = 0x20000000, LENGTH = 64K
}

REGION_ALIAS("REGION_TEXT", INSTR);
REGION_ALIAS("REGION_RODATA", DATA);
REGION_ALIAS("REGION_DATA", DATA);
REGION_ALIAS("REGION_BSS", DATA);
REGION_ALIAS("REGION_HEAP", DATA);
REGION_ALIAS("REGION_STACK", DATA);
