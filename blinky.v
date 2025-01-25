module top (
  input clk,
//  input key,
  output [3:0] led
);

// Sequential code (flip-flop)
// always @(posedge clk) begin
//   if (key) begin
//     ctr_q <= ctr_d;
//   end
// end

// Combinational code (boolean logic)
assign led[0] = 1'b1;
assign led[1] = 1'b0;

endmodule
