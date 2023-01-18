`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company:
// Engineer:
//
// Create Date: 01/09/2023 11:03:31 AM
// Design Name:
// Module Name: dna_test
// Project Name:
// Target Devices:
// Tool Versions:
// Description:
//
// Dependencies:
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////


module dna_test(
    input wire CLK_125MHZ,
    input wire CPU_RESET,
    output wire [95:0] DNA,
    output wire DNA_DONE
    );

    wire CLK_125MHZ;
    wire SHIFT;
    wire READ;
    wire DNA;
    wire DOUT;

   //  <-----Cut code below this line---->

   // DNA_PORTE2: Device DNA Access Port
   //             Kintex UltraScale
   // Xilinx HDL Language Template, version 2022.1


   dnaControl dnaControl (
    .clk(CLK_125MHZ),
    .rst(CPU_RESET),
    .ena(1),
    .dout(DOUT),
    .read(READ),
    .shift(SHIFT),
    .dna(DNA),
    .done(DNA_DONE)
    );

   DNA_PORTE2 #(
      .SIM_DNA_VALUE(96'h000000000000000000000000)  // Specifies a sample 96-bit DNA value for simulation.
   )
   DNA_PORTE2_inst (
      .DOUT(DOUT),   // 1-bit output: DNA output data.
      .CLK(CLK_125MHZ),     // 1-bit input: Clock input.
      .DIN(DOUT),     // 1-bit input: User data input pin.
      .READ(READ),   // 1-bit input: Active-High load DNA, active-Low read input.
      .SHIFT(SHIFT)  // 1-bit input: Active-High shift enable input.
   );

   // End of DNA_PORTE2_inst instantiation


endmodule
