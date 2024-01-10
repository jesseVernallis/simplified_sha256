module pad_block#(parameter logic[31:0] SIZE=1)(
input logic [31:0] input_message[16],
input logic [4:0] word_size,
output logic [31:0] output_message[16]);

int m;

always_comb begin
	if(word_size>=14) begin
		output_message = input_message; // there is no space for padding
	end else begin
//		for(m=0;m<16;m++)begin
//			if(m<word_size) begin
//			output_message[m] = input_message[m];
//			end else if(m==word_size) begin
//			output_message[m] = 32'h80000000;
//			end else if(m<15) begin
//			output_message[m] = 32'b0;
//			end
//		end
//		
//		output_message[15] = SIZE;
		
	end
end
	
endmodule : pad_block