module pad_message#(parameter int NUM_OF_WORDS=20)(
input logic [11:0] message);

function logic [7:0] pad_message(input logic [11:0] size);
    //
	dpsram_tb[NUM_OF_WORDS] = 32'h80000000;
    for (m = NUM_OF_WORDS + 1; m < blocks*16 -1; m++) begin
        dpsram_tb[m] = 32'h00000000;
    end
endfunction
 
assign blocks = determine_num_blocks(size);
	
endmodule : determine_num_blocks