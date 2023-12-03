module simplified_sha256 #(parameter integer NUM_OF_WORDS = 40)(
	input logic  clk, rst_n, start, // standard inputs
	input logic  [15:0] input_addr, hash_addr, // CONSTANTS = address values
	output logic done, memory_clk, enable_write, // memory
	output logic [15:0] memory_addr, // current memory address for READ and WRITE
	output logic [31:0] memory_write_data,
	input logic [31:0] memory_read_data // data that is read from memory
	); // data to write to memory
	
   // FSM state variables 
   //                 0     1      2        3         4         5          6
   enum logic [3:0] {IDLE, READ, BLOCK, PAD_BLOCK, COMPUTE_W,COMPRESSION, WRITE} next_state;
   
   
   // NOTE : Below mentioned frame work is for reference purpose.
   // Local variables might not be complete and you might have to add more variables
   // or modify these variables. Code below is more as a reference.
   
   // Local variables
   logic [31:0] w[64]; // hash computation temporary variable
   logic [31:0] message[16]; //temporary BLOCK storage (16 WORDS)

   logic [31:0] hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7; //INITIAL hash and MIDDLE hash values
   logic [31:0] A, B, C, D, E, F, G, H; //OUTPUT hash and MIDDLE hash values

   logic        enable_write_reg;
   logic [15:0] present_addr;
   logic [31:0] present_write_data;

   
   logic [7:0] round_index;
   logic [7:0] block_offset;
   logic [4:0] word_offset;

	logic [4:0] words_in_current_block;
   
	logic[7:0] num_blocks; //output
	
	parameter longint SIZE = NUM_OF_WORDS * 32; 
	
   // Generate request to memory
   // for reading from memory to get original message
   // for writing final computed has value
   assign memory_clk = clk;
   assign memory_addr = present_addr;
		   
   assign enable_write = enable_write_reg;				// word_offset = offset in WORDS
   assign memory_write_data = present_write_data;
	
	assign words_in_current_block = determine_words_in_current_block(block_offset);

	
  
   
	/****************************************************************
	
					MODULE AND FUCNTION DECLARATIONS
								
	*****************************************************************/
	//determines # of words in current block
	function logic[4:0] determine_words_in_current_block(input logic [11:0] block_offset_);
		automatic logic [31:0] words_left = NUM_OF_WORDS - 16 * block_offset;
		if((words_left >= 16) && (block_offset - (num_blocks - 1))) begin
			determine_words_in_current_block = 16;
		end
		else begin
			if((words_left > 0) && (words_left <= 15)) begin
				determine_words_in_current_block = NUM_OF_WORDS % 16;
			end
			else begin
				determine_words_in_current_block = 0;
			end
		end
	endfunction

	//pad message module declaration
	logic [31:0] message_out[16]; //output
	logic pad_block_enable;
	pad_block #(.SIZE(SIZE))pad_block_inst(
		.input_message(message), 
		.word_size(words_in_current_block), 
		.block_offset(block_offset), 
		.num_blocks(num_blocks), 
		.enable(pad_block_enable),
		.output_message(message_out)
	);
	
	
	//sha256_op module declaration
	logic[255:0] sha256_op_out; //output
	logic sha256_op_enable;
	sha256_op sha_op_inst(
		.a(A), 
		.b(B), 
		.c(C), 
		.d(D), 
		.e(E), 
		.f(F), 
		.g(G), 
		.h(H), 
		.w(w[round_index]),
		.t(round_index),
		.enable(sha256_op_enable),
		.sha256_out(sha256_op_out)
	);
	
   //expand_message_to_w module declaration
	logic[31:0] wt; //output
	logic expand_message_enable;
	expand_message_to_w expand_inst(
		.w(w),
		.t(round_index),
		.enable(expand_message_enable),
		.wt(wt)
	);
	
	//determine_num_blocks declaration

	determine_num_blocks det_block_inst(
	.size(NUM_OF_WORDS),
	.blocks(num_blocks)
	);	

	/************************************************************************
	
						State Machine Logic and Implementation
	
	*************************************************************************/
   always_ff @(posedge clk, negedge rst_n)
   begin
	 if (!rst_n) begin
	   next_state <= IDLE;
	 end 
	 else begin 
		 case (next_state)
		   //Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
		   IDLE: begin
			   if(start) begin 

				   present_addr <= input_addr; //set address for input message
				   word_offset <= 0;
				   block_offset <= 0;
				   enable_write_reg <= 0;
					
				   {A, B, C, D, E, F, G, H} <= {32'h6a09e667, 32'hbb67ae85, 32'h3c6ef372,
	32'ha54ff53a, 32'h510e527f, 32'h9b05688c, 32'h1f83d9ab, 32'h5be0cd19};
					{hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7} <= {32'h6a09e667, 32'hbb67ae85, 32'h3c6ef372,
	32'ha54ff53a, 32'h510e527f, 32'h9b05688c, 32'h1f83d9ab, 32'h5be0cd19};
					next_state <= READ;
			   end
				else begin
					next_state <= IDLE;
				end
		   end
			
			//read a BLOCK from memory 
		   READ: begin 
				if(word_offset == 0) begin //1 cycle of no output
					present_addr <= present_addr + 1;
					word_offset <= word_offset + 1;
					next_state <= READ;
				end else if(word_offset <= 15 /*&& word_offset<=words_in_current_block*/) begin //15 cycles with output
				   present_addr <= present_addr + 1;
				   message[word_offset-1] <= memory_read_data; 
				   word_offset <= word_offset + 1; 
				   next_state <= READ;
			   end else if(word_offset == 16 /*&& word_offset<=words_in_current_block*/) begin // 1 additional cycle with output
					message[word_offset-1] <= memory_read_data; 
				   word_offset <= word_offset + 1; 
				   next_state <= READ;
				end
				else begin
					//SKip padding if not needed
					if(words_in_current_block == 16) begin 
						//compute_w setup
						round_index <= 0;
						expand_message_enable <= 1;
						next_state <= COMPUTE_W;
					end
					else begin
						//pad_block setup
						pad_block_enable <= 1;
						next_state <= PAD_BLOCK;
					end
			   end
		   end
			
			//ADD OPERATION
		   BLOCK: begin 
			   //ADD OPERATION
			   hash0 <= hash0+A;
				hash1 <= hash1+B;
				hash2 <= hash2+C;
				hash3 <= hash3+D;
				hash4 <= hash4+E;
				hash5 <= hash5+F;
				hash6 <= hash6+G;
				hash7 <= hash7+H;
			   
				// if this is the last block, WRITE
			   if(block_offset == num_blocks)begin 
					present_addr <= hash_addr;
					present_write_data <= hash0+A;
					enable_write_reg <= 1;
					word_offset <= 0;
					next_state <= WRITE;
			   end
				//if it is not the last block, begin READ
				else begin 
					//TODO .
				   A <= hash0+A;
					B <= hash1+B;
					C <= hash2+C;
					D <= hash3+D;
					E <= hash4+E;
					F <= hash5+F;
					G <= hash6+G;
					H <= hash7+H;
					word_offset <= 0;
				   next_state <= READ;
					
			   end
		   end
			
			//Pad the message if needed
			PAD_BLOCK: begin
				//message set to message out reg from module 
				message <= message_out;
				//setup for compute_w
				round_index <= 0;	
				pad_block_enable <= 0;
				expand_message_enable <= 1;
				next_state <= COMPUTE_W;
			end
			
		   //Compute W[0] -> W[63] from input padded message
		   COMPUTE_W: begin
			   if(round_index<=63) begin
				
				   if(round_index<=15) begin
					   w[round_index] <= message[round_index];
				   end else begin
					   w[round_index] <= wt;//expand_message(w, round_index);
					   
				   end
				   round_index <= round_index + 1;
				   next_state <= COMPUTE_W;
			   end
			   //final round 
			   else begin 
					//compression setup
					expand_message_enable <= 0;
					sha256_op_enable <= 1;
					round_index <= 0;
				   next_state <= COMPRESSION;
				   
			   end
		   end
			
			// 64 round of COMPRESSION ALGORITHM
		   COMPRESSION: begin 
		   	   // if there are still rounds left
			   if(round_index<=63) begin 
				   {A, B, C, D, E, F, G, H} <= sha256_op_out;//sha256_op(A, B, C, D, E, F, G, H, w[round_index], round_index);
				   round_index <= round_index + 1;
				   next_state <= COMPRESSION;
			   end
			   //final round
			   else begin
					//block setup
					sha256_op_enable <= 0;
					block_offset <= block_offset+1;
					next_state <= BLOCK;
			   end
		   end
			//Write has values back to memory
		   WRITE: begin
				
			   if(word_offset<=7) begin
				   case(word_offset+1)
					   0: present_write_data <= hash0;
					   1: present_write_data <= hash1;
					   2: present_write_data <= hash2;
					   3: present_write_data <= hash3;
					   4: present_write_data <= hash4;
					   5: present_write_data <= hash5;
					   6: present_write_data <= hash6;
					   7: present_write_data <= hash7;
					   default : present_write_data <= hash0;
				   endcase
				   word_offset <= word_offset+1;
					present_addr <= present_addr+1;
				   next_state <= WRITE; //do another read while block is not full
			   end 
				else begin
				   next_state <= IDLE;
			   end
			end
		 endcase
	  end
   end
   
   // Generate done when SHA256 hash computation has finished and moved to IDLE state
   
   assign done = (next_state == IDLE);
   
   endmodule
   