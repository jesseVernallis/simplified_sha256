module expand_message_to_w(
	input logic [31:0] w[64],
	input logic [7:0] t,
	input logic enable,
	output logic [31:0] wt);
	
	//right rotate function
	 function logic [31:0] ror(input logic [31:0] in,
										 input logic [7:0] s);
	   begin
		   ror = (in >> s) | (in << (32-s));
	   end
	   endfunction
		
	//sha256_op function
	 function logic [31:0] expand_message(input logic[31:0] w[64],
		   input logic [7:0] t);
		   logic [31:0] s1, s0; // internal signals
		   begin
			   //S0 = (Wt-15 rightrotate 7) xor (Wt-15 rightrotate 18) xor (Wt-15 rightshift 3)
			   s0 = ror(w[t-15], 7) ^ ror(w[t-15], 18) ^ (w[t-15] >> 3); 
			   //S1 = (Wt-2 rightrotate 17) xor (Wt-2 rightrotate 19) xor (Wt-2 rightshift 10)
			   s1 = ror(w[t-2],17) ^ ror(w[t-2],19) ^ (w[t-2] >> 10); 
			   //Wt = Wt-16 + s0 + Wt-7 + s1
			   expand_message = w[t-16] + s0 + w[t-7] + s1; 
			   
		   end
	   endfunction
		always_comb begin
			if(enable) begin
				 wt = expand_message(w, t);
			end
			else begin
				wt = 32'bx;
			end
		end
	endmodule : expand_message_to_w