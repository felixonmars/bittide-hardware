/*
SPDX-FileCopyrightText: 2022 Google LLC

SPDX-License-Identifier: CC0-1.0
*/

MEMORY
{
  INSTRUCTION_MEMORY : ORIGIN = 0x00000000, LENGTH = 16M
  DATA_MEMORY : ORIGIN = 0x10001000, LENGTH = 1024K
}

REGION_ALIAS("REGION_TEXT", INSTRUCTION_MEMORY);
REGION_ALIAS("REGION_RODATA", DATA_MEMORY);
REGION_ALIAS("REGION_DATA", DATA_MEMORY);
REGION_ALIAS("REGION_BSS", DATA_MEMORY);
REGION_ALIAS("REGION_HEAP", DATA_MEMORY);
REGION_ALIAS("REGION_STACK", DATA_MEMORY);
