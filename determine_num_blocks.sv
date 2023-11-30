module determine_num_blocks(
input logic [11:0] size,
output logic [7:0] blocks);

function logic [7:0] determine_num_blocks(input logic [11:0] size);
   
	   //rounding
	   //logic [7:0] quotient = (size >> 4); //quotient = size/16, size = 101000
	   //logic [11:0] n = quotient << 4; //n = quotient * 16, = 100000 quotient=000010
   
	   if(size[3:0]>0) begin
		   determine_num_blocks = (size>>4) + 1;
	   end
	   else begin
		   determine_num_blocks = (size>>4);
	   end
endfunction
 
assign blocks = determine_num_blocks(size);
	
endmodule : determine_num_blocks