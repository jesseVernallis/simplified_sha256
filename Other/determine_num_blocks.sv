module determine_num_blocks(
	input logic [31:0] size,
	output logic [7:0] blocks);
	function logic [7:0] determine_num_blocks(input logic [31:0] size);
			//size + 3 due to 64bit size and at least 1 bit padding 
		   if((size[3:0] + 3)>0) begin
			   determine_num_blocks = ((size+3)>>4) + 1;
		   end
		   else begin
			   determine_num_blocks = ((size+3)>>4);
		   end
	endfunction
	 
	assign blocks = determine_num_blocks(size);
		
	endmodule : determine_num_blocks