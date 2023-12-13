module simplified_sha256 #(parameter integer NUM_OF_WORDS = 40)(
	input logic  clk, rst_n, start, // standard inputs
	input logic  [15:0] input_addr, hash_addr, // CONSTANTS = address values
	output logic done, memory_clk, enable_write, // memory
	output logic [15:0] memory_addr, // current memory address for READ and WRITE
	output logic [31:0] memory_write_data,
	input logic [31:0] memory_read_data // data that is read from memory
	); // data to write to memory
	
   // FSM state variables 
   //                 0     1      2        3         4         5          
   enum logic [3:0] {IDLE, READ, BLOCK, PAD_BLOCK,COMPRESSION, WRITE} next_state;
   
   
   // NOTE : Below mentioned frame work is for reference purpose.
   // Local variables might not be complete and you might have to add more variables
   // or modify these variables. Code below is more as a reference.
   
   // Local variables
   logic [31:0] w[16]; // hash computation temporary variable
	logic [31:0] kt;
   logic [31:0] message[16]; //temporary BLOCK storage (16 WORDS)

   logic [31:0] hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7; //INITIAL hash and MIDDLE hash values
   logic [31:0] A, B, C, D, E, F, G, H; //OUTPUT hash and MIDDLE hash values

   logic        enable_write_reg;
   logic [15:0] present_addr;
   logic [31:0] present_write_data;

   
   logic [6:0] round_index;
   logic [7:0] block_offset;
   logic [4:0] word_offset;

	logic [4:0] words_in_current_block;
   
	logic[7:0] num_blocks; //output
	
	parameter longint SIZE = NUM_OF_WORDS * 32; 
	
	parameter int k[0:63] = '{
	  32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
	  32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
	  32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
	  32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
	  32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
	  32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
	  32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
	  32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
   };
	
   // Generate request to memory
   // for reading from memory to get original message
   // for writing final computed has value
   assign memory_clk = clk;
   assign memory_addr = present_addr;
		   
   assign enable_write = enable_write_reg;				// word_offset = offset in WORDS
   assign memory_write_data = present_write_data;
	
	assign words_in_current_block = determine_words_in_current_block(block_offset);
	assign num_blocks = determine_num_blocks(NUM_OF_WORDS);

	
  
   
	/****************************************************************
	
							FUCNTION DECLARATIONS
								
	*****************************************************************/
	//right rotate function
	function logic [31:0] ror(input logic [31:0] in,input logic [7:0] s);
		begin
			ror = (in >> s) | (in << (32-s));
		end
	endfunction
	
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
	
	//determine_num_blocks function
	function logic [7:0] determine_num_blocks(input logic [11:0] size);
   
	   if((size[3:0] + 3)>0) begin
		   determine_num_blocks = ((size+3)>>4) + 1;
	   end
	   else begin
		   determine_num_blocks = ((size+3)>>4);
	   end
	endfunction
	
	//sha256_op function
	 function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
										kt);
			logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
		begin
			S0 = ror(a, 2) ^ ror(a, 13) ^ ror(a, 22);
			maj = (a & b) ^ (a & c) ^ (b & c);
			t2 = S0 + maj;
			S1 = ror(e, 6) ^ ror(e, 11) ^ ror(e, 25);
			ch = (e & f) ^ ((~e) & g);
			t1 = h + S1 + ch + kt + w;
			sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
			//            A       B  C  D  E       F  G  H
		end
	 endfunction
	 
	 //pad_message function
	 function void pad_message(input logic [31:0] input_message[16], output logic [31:0] output_message[16]);

			// If not add in message words and padding
			for(int m=0;m<16;m++)begin 
				//if a word is present add it to the block
				if(m<words_in_current_block) begin 
				output_message[m] = input_message[m];
				// if this is the first padded block and the paddding end indicator word 
				end else if((m==words_in_current_block) && (words_in_current_block != 0)) begin 
				output_message[m] = 32'h80000000;
				//else add the padding zeros to the rest of the words
				end else begin
				output_message[m] = 32'b0;
				end
			end
			//if this is the last block add the size to the two words
			if(block_offset == (num_blocks -1)) begin
				output_message[14] = SIZE[63:32];
				output_message[15] = SIZE[31:0];
			end
	 endfunction
	 
	 //expand_message_to_w function
	 function logic [31:0] expand_message(input logic[31:0] w15,w2,w16,w7);
	   logic [31:0] s1, s0; // internal signals
	   begin
		   //S0 = (Wt-15 rightrotate 7) xor (Wt-15 rightrotate 18) xor (Wt-15 rightshift 3)
		   s0 = ror(w15, 7) ^ ror(w15, 18) ^ (w15 >> 3); 
		   //S1 = (Wt-2 rightrotate 17) xor (Wt-2 rightrotate 19) xor (Wt-2 rightshift 10)
		   s1 = ror(w2,17) ^ ror(w2,19) ^ (w2 >> 10); 
		   //Wt = Wt-16 + s0 + Wt-7 + s1
		   expand_message = w16 + s0 + w7 + s1; 
		   
	    end
     endfunction
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
						next_state <=  COMPRESSION;
					end
					else begin
						//pad_block setup
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
				logic [31:0] message_out [16];
				pad_message(message, message_out);
				message <= message_out;
				//setup for compute_w
				round_index <= 0;	
				next_state <=  COMPRESSION;
			end
			
			// 64 round of COMPRESSION ALGORITHM
		   COMPRESSION: begin 
				kt<= k[round_index];	
				//Compute W[0] -> W[63] from input padded message
				if(round_index<=15) begin
					w[15] <= message[round_index];
				end else begin
					w[15] <= expand_message(w[1],w[14],w[0],w[9]); 
				end	
				if(round_index != 0) begin
				// 64 round of COMPRESSION ALGORITHM
					{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, w[15], kt);
				end
				
				round_index <= round_index + 7'b1;
				if(round_index == 64) begin
					//block setup
					block_offset <= block_offset+1;
					next_state <= BLOCK;
				end
				//final round
				else begin
					w[0] <= w[1];
					w[1] <= w[2];
					w[2] <= w[3];
					w[3] <= w[4];
					w[4] <= w[5];
					w[5] <= w[6];
					w[6] <= w[7];
					w[7] <= w[8];
					w[8] <= w[9];
					w[9] <= w[10];
					w[10] <= w[11];
					w[11] <= w[12];
					w[12] <= w[13];
					w[13] <= w[14];
					w[14] <= w[15];
					next_state <= COMPRESSION;
						
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
   


