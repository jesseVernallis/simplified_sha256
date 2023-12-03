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
   enum logic [3:0] {IDLE, READ, BLOCK, PAD_BLOCK, COMPUTE_W,COMPRESSION, WRITE} next_state;
   
   
   // NOTE : Below mentioned frame work is for reference purpose.
   // Local variables might not be complete and you might have to add more variables
   // or modify these variables. Code below is more as a reference.
   
   // Local variables
   logic [31:0] w[64]; // hash computation temporary variable
   logic [31:0] message[16]; //temporary BLOCK storage (16 WORDS)
   //logic [31:0] wt;
   //logic [31:0] S0,S1; // wires in hash computation. Don't touch.
   logic [31:0] hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7; //INITIAL hash and MIDDLE hash values
   logic [31:0] A, B, C, D, E, F, G, H; //OUTPUT hash and MIDDLE hash values
   //logic [ 7:0] i, j; // i = block index in message, j = word index in block
   //logic [15:0] offset; // in word address
   //logic [ 7:0] num_blocks; moved to right above det_num_blocks module
   logic        enable_write_reg;
   logic [15:0] present_addr;
   logic [31:0] present_write_data;
   //logic [512:0] data_read;
   //logic [ 7:0] tstep;
   
   logic [7:0] round_index;
   logic [31:0] block_offset;
   logic [4:0] word_offset;

	logic [4:0] words_in_current_block;
   
   // Initialize after index
   // start loop ------
   // READ FROM MEMORY / INITIALIZE A
   // START COMPUTE OPERATION
   // ADD OPERATION
   // goto start ------
	
	//determine_num_blocks declaration
	logic[7:0] num_blocks; //output
	determine_num_blocks det_block_inst(
	.size(NUM_OF_WORDS),
	.blocks(num_blocks)
	);	
	
	
	parameter integer SIZE = NUM_OF_WORDS * 32; 
   
   //Places where memory is accessed
   // - READ
   // - 
   
   //TODO Figure out where the initial hash values come from
   // SHA256 K constants
//    parameter int k[0:63] = '{
// 	  32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
// 	  32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
// 	  32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
// 	  32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
// 	  32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
// 	  32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
// 	  32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
// 	  32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
//    };
   
   
   // Generate request to memory
   // for reading from memory to get original message
   // for writing final computed has value
   assign memory_clk = clk;
   assign memory_addr = present_addr;
		   
   assign enable_write = enable_write_reg;				// word_offset = offset in WORDS
   assign memory_write_data = present_write_data;
	
	assign words_in_current_block = determine_words_in_current_block(block_offset);

	function logic[4:0] determine_words_in_current_block(input logic [11:0] block_offset_);
		automatic logic [31:0] words_left = NUM_OF_WORDS - 16 * block_offset;
		$display("wl : %d", words_left);
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
   
   //assign num_blocks = determine_num_blocks(NUM_OF_WORDS); 
   //assign tstep = (i - 1);
   
   
   // Note : Function defined are for reference purpose. Feel free to add more functions or modify below.
   // Function to determine number of blocks in memory to fetch
   /********************************************************************
										FUNCTIONS
   //TODO Check if the input is the raw message or message with padding
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
   
   // SHA256 hash round
   function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
									input logic [7:0] t);
	   logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
   begin
	   S0 = ror(a, 2) ^ ror(a, 13) ^ ror(a, 22);
	   maj = (a & b) ^ (a & c) ^ (b & c);
	   t2 = S0 + maj;
	   S1 = ror(e, 6) ^ ror(e, 11) ^ ror(e, 25);
	   ch = (e & f) ^ ((~e) & g);
	   t1 = h + S1 + ch + k[t] + w;
	   sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
		//            A       B  C  D  E       F  G  H
   end
   endfunction
   
   // expand message to w[64]
   function logic [31:0] expand_message(input logic[31:0] w[64],
	   input logic [7:0] t);
	   logic [31:0] s1, s0; // internal signals
	   begin
		   //S0 = (Wt-15 rightrotate 7) xor (Wt-15 rightrotate 18) xor (Wt-15 rightshift 3)
		   s0 = ror(w[t-15], 7) ^ ror(w[t-15], 18) ^ ror(w[t-15], 3); 
		   //S1 = (Wt-2 rightrotate 17) xor (Wt-2 rightrotate 19) xor (Wt-2 rightshift 10)
		   s1 = ror(w[t-2],17) ^ ror(w[t-2],19) ^ ror(w[t-2],10); 
		   //Wt = Wt-16 + s0 + Wt-7 + s1
		   expand_message = w[t-16] + s0 + w[t-7] + s1; 
		   
	   end
   endfunction
   
   
   
   
   // Right Rotation Example : right rotate input x by r
   // Lets say input x = 1111 ffff 2222 3333 4444 6666 7777 8888
   // lets say r = 4
   // x >> r  will result in : 0000 1111 ffff 2222 3333 4444 6666 7777 
   // x << (32-r) will result in : 8888 0000 0000 0000 0000 0000 0000 0000
   // final right rotate expression is = (x >> r) | (x << (32-r));
   // (0000 1111 ffff 2222 3333 4444 6666 7777) | (8888 0000 0000 0000 0000 0000 0000 0000)
   // final value after right rotate = 8888 1111 ffff 2222 3333 4444 6666 7777
   // Right rotation function
   
   function logic [31:0] ror(input logic [31:0] in,
									 input logic [7:0] s);
   begin
	   ror = (in >> s) | (in << (32-s));
   end
   endfunction
	****************************************************************/


   int n;
	/****************************************************************
	
					MODULE DECLARATIONS(REPLACE FUNCTIONS)
								
	*****************************************************************/
	
	int m;
	function void pad_message(input logic [31:0] input_message[16], output logic [31:0] output_message[16]);
		if(words_in_current_block>=16) begin
			output_message = input_message; // there is no space for padding (padding overlapps between blocks, so there is space)
		end else begin
			for(m=0;m<16;m++)begin /// only have to do to m = 15
				if(m<words_in_current_block) begin
				output_message[m] = input_message[m];
				end else if((m==words_in_current_block) && (words_in_current_block != 0)) begin
				output_message[m] = 32'h80000000;
				end else if(m<=15) begin
				output_message[m] = 32'b0;
				end
				$display("m : %h", output_message[m]);
			end
			if(block_offset == (num_blocks -1)) begin
				output_message[15] = SIZE;
			end
			
		end
	endfunction

	//sha256_op module declaration
	logic[255:0] sha256_op_out; //output
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
	.sha256_out(sha256_op_out)
	);
	
   //expand_message_to_w module declaration
	logic[31:0] wt; //output
	expand_message_to_w expand_inst(
	.w(w),
	.t(round_index),
	.wt(wt)
	);


   always_ff @(posedge clk, negedge rst_n)
   begin
	 if (!rst_n) begin
	   next_state <= IDLE;
	 end 
	 else begin 
		 case (next_state)
		   // Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
		   IDLE: begin
			   if(start) begin 

				   present_addr <= input_addr; //set address for input message
				   word_offset <= 0;
				   block_offset <= 0;
				   enable_write_reg <= 0;
					
				   {A, B, C, D, E, F, G, H} <= {32'h6a09e667, 32'hbb67ae85, 32'h3c6ef372, 32'ha54ff53a, 32'h510e527f, 32'h9b05688c, 32'h1f83d9ab, 32'h5be0cd19};
				   {hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7} <= {32'h6a09e667, 32'hbb67ae85, 32'h3c6ef372, 32'ha54ff53a, 32'h510e527f, 32'h9b05688c, 32'h1f83d9ab, 32'h5be0cd19};
				   next_state <= READ;
			   end
				else begin
					next_state <= IDLE;
				end
		   end
   
		   BLOCK: begin //ADD OPERATION
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
   
		   READ: begin //read a BLOCK from memory 
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
				   next_state <= PAD_BLOCK;
					
					//for(n=0;n<=15;n++) begin
					//	$display("CODE [word=%d]=%S", block_offset*16+n, message[n]);
					//end
			   end
			   
		   end
			
			PAD_BLOCK: begin
				logic [31:0] message_out[16];
				pad_message(message, message_out);
				message <= message_out;
				round_index <= 0;
					
				next_state <= COMPUTE_W;
			end
		  
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
				   next_state <= COMPRESSION;
				   round_index <= 0;
			   end
		   end
   
   
		   COMPRESSION: begin // COMPRESSION ALGORITHM
		   	   // if there are still rounds left
			   if(round_index<=63) begin 
				   {A, B, C, D, E, F, G, H} <= sha256_op_out;//sha256_op(A, B, C, D, E, F, G, H, w[round_index], round_index);
				   round_index <= round_index + 1;
				   next_state <= COMPRESSION;
			   end
			   //final round
			   else begin
				   next_state <= BLOCK;
					block_offset <= block_offset+1;
			   end
		   end
   
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
   