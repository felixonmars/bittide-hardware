MEMORY
{
  INSTR : ORIGIN = 0x30000000, LENGTH = 512M
  DATA  : ORIGIN = 0x50000000, LENGTH = 768M
}

REGION_ALIAS("REGION_TEXT", INSTR);
REGION_ALIAS("REGION_RODATA", DATA);
REGION_ALIAS("REGION_DATA", DATA);
REGION_ALIAS("REGION_BSS", DATA);
REGION_ALIAS("REGION_HEAP", DATA);
REGION_ALIAS("REGION_STACK", DATA);
