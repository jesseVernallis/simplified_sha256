module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] header_addr, hash_out_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] memory_addr,
                    output logic [31:0] memory_write_data,
                     input logic [31:0] memory_read_data);

parameter num_nonces = 16;
parameter INSTANCES = 4;
logic [31:0] hash_out[num_nonces];


   // FSM state variables 
   //                 0     1       2        3        4     5          
   enum logic [2:0] {IDLE, READ, COMPUTE_ST0, ADD_ST0, PAD_BLOCK, COMPUTE, ADD,WRITE} next_state;
   
   
   // NOTE : Below mentioned frame work is for reference purpose.
   // Local variables might not be complete and you might have to add more variables
   // or modify these variables. Code below is more as a reference.
   
   // Local variables
   logic [31:0] w[64]; // hash computation temporary variable
	logic [31:0] wt,w15,w2,w16,w7,kt;
	logic [31:0] wt_p[INSTANCES],w15_p[INSTANCES],w2_p[INSTANCES],
	w16_p[INSTANCES],w7_p[INSTANCES],kt_p[INSTANCES];
   logic [31:0] message[16]; //temporary BLOCK storage (16 WORDS)

   logic [31:0] hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7; //INITIAL hash and MIDDLE hash values
   logic [31:0] A, B, C, D, E, F, G, H; //OUTPUT hash and MIDDLE hash values
	
   logic        enable_write_reg;
   logic [15:0] present_addr;
   logic [31:0] present_write_data;

   
   logic [6:0] round_index;
	logic [6:0] round_index_p;
   logic [2:0] stage;
	logic [4:0] nonce;
   logic [4:0] word_offset;
	logic [31:0] saved_message[3];
	logic [31:0] message_out [16];
	
	logic [31:0] w_p[INSTANCES][64]; // hash computation temporary variable
	logic [31:0] messages_p[INSTANCES][16]; //temporary BLOCK storage (16 WORDS)

	logic [31:0] A_[INSTANCES];
	logic [31:0] B_[INSTANCES];
	logic [31:0] C_[INSTANCES];
	logic [31:0] D_[INSTANCES];
	logic [31:0] E_[INSTANCES];
	logic [31:0] F_[INSTANCES];
	logic [31:0] G_[INSTANCES];
	logic [31:0] H_[INSTANCES];
	
	logic [31:0] hash0_[INSTANCES];
	logic [31:0] hash1_[INSTANCES];
	logic [31:0] hash2_[INSTANCES];
	logic [31:0] hash3_[INSTANCES];
	logic [31:0] hash4_[INSTANCES];
	logic [31:0] hash5_[INSTANCES];
	logic [31:0] hash6_[INSTANCES];
	logic [31:0] hash7_[INSTANCES];	//OUTPUT hash and MIDDLE hash values
	logic[31:0]  message_out_p[INSTANCES][16];

	
	parameter longint SIZE = 640; 
	
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
   assign mem_clk = clk;
   assign memory_addr = present_addr;
		   
   assign mem_we = enable_write_reg;				// word_offset = offset in WORDS
   assign memory_write_data = present_write_data;
	

	
  
   
	/****************************************************************
	
							FUCNTION DECLARATIONS
								
	*****************************************************************/
	//right rotate function
	function logic [31:0] ror(input logic [31:0] in,input logic [7:0] s);
		begin
			ror = (in >> s) | (in << (32-s));
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
	 
	 //pads the message for the 2nd stage
     function void pad_message_nonce(input logic [31:0] input_message[3], input logic[31:0] nonce, output logic [31:0] output_message[16]);
            for(int m=0;m<3;m++) begin
                output_message[m] = input_message[m];
            end

            output_message[3] = nonce;

            output_message[4] = 32'h80000000;

            for(int m=5;m<14;m++) begin
                output_message[m] = 32'b0;
            end

            output_message[14] = SIZE[63:32];
            output_message[15] = SIZE[31:0];
     endfunction
	 // pads the message for the 3rd stage
	  function void pad_message_h(input logic [31:0] hash0,hash1,hash2,hash3,hash4,hash5,hash6,hash7, output logic [31:0] output_message[16]);
        output_message[0]=hash0;
		  output_message[1]=hash1;
		  output_message[2]=hash2;
		  output_message[3]=hash3;
		  output_message[4]=hash4;
		  output_message[5]=hash5;
		  output_message[6]=hash6;
		  output_message[7]=hash7;
		  
        output_message[8] = 32'h80000000;
		  
        for(int n = 9;n<14;n++) begin
            output_message[n] = 32'b0;
        end
		  output_message[14] = 0;
        output_message[15] = 256;  
	  endfunction

	 //expand_message_to_w function
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
	
   always_ff @(posedge clk, negedge reset_n)
   begin
	 if (!reset_n) begin
	   next_state <= IDLE;
	 end 
	 else begin 
		 case (next_state)
		   //Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
		   IDLE: begin
			   if(start) begin 

				   present_addr <= header_addr; //set address for input message
				   word_offset <= 0;
				   enable_write_reg <= 0;
					nonce <= 0;
					stage <= 0;
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
			/*********************DONE******************/
		   READ: begin 
				if(word_offset == 0) begin //1 cycle of no output
					present_addr <= present_addr + 16'b1;
					word_offset <= word_offset + 5'b1;
					next_state <= READ;
				end else if(word_offset <= 15) begin //15 cycles with output
				   present_addr <= present_addr + 16'b1;
				   message[word_offset-1] <= memory_read_data; 
				   word_offset <= word_offset + 5'b1; 
				   next_state <= READ;
			   end else if(word_offset == 16) begin // 1 additional cycle with output
					message[word_offset-1] <= memory_read_data; 
				   word_offset <= word_offset + 5'b1; 
				   next_state <= READ;
				end
				else begin
					//SKip padding if not needed
					if(stage == 0) begin 
						//compute_w setup
						round_index <= 0;
						next_state <=  COMPUTE_ST0;
					end
					else begin
						//pad_block setup
						saved_message[0] <= message[0];
						saved_message[1] <= message[1];
						saved_message[2] <= message[2];
						next_state <= PAD_BLOCK;
					end
			   end
		   end
			
			//ADD OPERATION
		   ADD_ST0: begin 
			   //ADD OPERATION
			   hash0 <= hash0+A;
				hash1 <= hash1+B;
				hash2 <= hash2+C;
				hash3 <= hash3+D;
				hash4 <= hash4+E;
				hash5 <= hash5+F;
				hash6 <= hash6+G;
				hash7 <= hash7+H;
			   
			   A <= hash0+A;
				B <= hash1+B;
				C <= hash2+C;
				D <= hash3+D;
				E <= hash4+E;
				F <= hash5+F;
				G <= hash6+G;
				H <= hash7+H;
					
				stage <= stage + 3'b1;
				word_offset <= 0;
				next_state <= READ;
		   end
			// 64 round of COMPRESSION ALGORITHM 
			/*********DONE***********/
		   COMPUTE_ST0: begin 
		   	   // if there are still rounds left
			   if(round_index<=64) begin 
					//Compute W[0] -> W[63] from input padded message
					kt <= k[round_index];
				   if(round_index<=15) begin
					   w[round_index] <= message[round_index];
						wt <= message[round_index];
						if(round_index == 15) begin
							//save inputs for first expand message
							w15 <= w[1];
							w2 <= w[14];
							w16 <= w[0];
							w7 <= w[9];
						end
				   end else begin
					   w[round_index] <= expand_message(w15,w2,w16,w7); 
						wt <= expand_message(w15,w2,w16,w7); 
						//save inputs for next expand message
						w15 <= w[round_index - 14];
						w2 <= w[round_index - 1];
						w16 <= w[round_index - 15];
						w7 <= w[round_index - 6];
				   end
					if(round_index != 0) begin
					// 64 round of COMPRESSION ALGORITHM
						{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, wt, kt);
						round_index <= round_index + 7'b1;
					end
					else begin
						round_index <= round_index + 7'b1;
					end
				next_state <= COMPUTE_ST0;
			   end
			   //final round
			   else begin
					//block setup
					next_state <= ADD_ST0;
			   end
		   end
			//Pad the message if needed
			PAD_BLOCK: begin
				//message set to message out reg from module
				if(stage == 1) begin
					for(int inst=0;inst<INSTANCES;inst++) begin
						pad_message_nonce(saved_message,nonce + inst,message_out_p[inst]);
						A_[inst] <= hash0;
						B_[inst] <= hash1;
						C_[inst] <= hash2;
						D_[inst] <= hash3;
						E_[inst] <= hash4;
						F_[inst] <= hash5;
						G_[inst] <= hash6;
						H_[inst] <= hash7;
						hash0_[inst] <= hash0;
						hash1_[inst] <= hash1;
						hash2_[inst] <= hash2;
						hash3_[inst] <= hash3;
						hash4_[inst] <= hash4;
						hash5_[inst] <= hash5;
						hash6_[inst] <= hash6;
						hash7_[inst] <= hash7;
					end
				end
				else if(stage == 2) begin
					for(int inst=0;inst<INSTANCES;inst++) begin
						pad_message_h(hash0_[inst],hash1_[inst],hash2_[inst],hash3_[inst],hash4_[inst],hash5_[inst],hash6_[inst],hash7_[inst],message_out_p[inst]);
					{A_[inst], B_[inst], C_[inst], D_[inst], E_[inst], F_[inst], G_[inst], H_[inst]} <= {32'h6a09e667, 32'hbb67ae85, 32'h3c6ef372,
					32'ha54ff53a, 32'h510e527f, 32'h9b05688c, 32'h1f83d9ab, 32'h5be0cd19};
					{hash0_[inst], hash1_[inst], hash2_[inst], hash3_[inst], hash4_[inst], hash5_[inst], hash6_[inst], hash7_[inst]} <= {32'h6a09e667, 32'hbb67ae85, 32'h3c6ef372,
					32'ha54ff53a, 32'h510e527f, 32'h9b05688c, 32'h1f83d9ab, 32'h5be0cd19};	
					end
				end
				for(int inst=0;inst<INSTANCES;inst++) begin
					messages_p[inst] <= message_out_p[inst];
				end
				//setup for compute_w
				round_index_p <= 0;	
				next_state <=  COMPUTE;
			end
				//ADD OPERATION
		   ADD: begin 
				for(int inst=0;inst<INSTANCES;inst++) begin
					//ADD OPERATION
					hash0_[inst] <= hash0_[inst]+A_[inst];
					hash1_[inst] <= hash1_[inst]+B_[inst];
					hash2_[inst] <= hash2_[inst]+C_[inst];
					hash3_[inst] <= hash3_[inst]+D_[inst];
					hash4_[inst] <= hash4_[inst]+E_[inst];
					hash5_[inst] <= hash5_[inst]+F_[inst];
					hash6_[inst] <= hash6_[inst]+G_[inst];
					hash7_[inst] <= hash7_[inst]+H_[inst];
					
					// if this is the last block, WRITE
					//if(nonce == 16)begin 
					//for parallel change
						
						//next_state <= WRITE;
					//end
					//if it is not the last block, begin READ
					//else begin 
						//TODO .
					A_[inst] <= hash0_[inst]+A_[inst];
					B_[inst] <= hash1_[inst]+B_[inst];
					C_[inst] <= hash2_[inst]+C_[inst];
					D_[inst] <= hash3_[inst]+D_[inst];
					E_[inst] <= hash4_[inst]+E_[inst];
					F_[inst] <= hash5_[inst]+F_[inst];
					G_[inst] <= hash6_[inst]+G_[inst];
					H_[inst] <= hash7_[inst]+H_[inst];
				end	
				stage <= stage + 3'b1;
				if(stage == 1)begin
					next_state <= PAD_BLOCK;
				end
				else if(stage == 2)	begin
					present_addr <= hash_out_addr + nonce;
					for(int inst=0;inst<INSTANCES;inst++) begin
						hash_out[inst+nonce]<= hash0_[inst]+A_[inst];
					end
					present_write_data <= hash0_[0]+A_[0];
					enable_write_reg <= 1;
					word_offset <= 1;
					next_state <= WRITE;
				end
			end
			
			
			// 64 round of COMPRESSION ALGORITHM 
			/*********DONE***********/
		   COMPUTE: begin 
	
		   	   // if there are still rounds left
			   if(round_index_p<=64) begin 
					kt<= k[round_index_p];	
					//Compute W[0] -> W[63] from input padded message
					for(int inst = 0;inst<INSTANCES;inst++) begin
						if(round_index_p<=15) begin
							w_p[inst][round_index_p] <= messages_p[inst][round_index_p];
							wt_p[inst] <= messages_p[inst][round_index_p];
							if(round_index_p == 15) begin
							//save inputs for first expand message
							w15_p[inst] <= w_p[inst][1];
							w2_p[inst] <= w_p[inst][14];
							w16_p[inst] <= w_p[inst][0];
							w7_p[inst] <= w_p[inst][9];
						end
						end else begin
							w_p[inst][round_index_p] <= expand_message(w15_p[inst],w2_p[inst],w16_p[inst],w7_p[inst]); 
							wt_p[inst] <= expand_message(w15_p[inst],w2_p[inst],w16_p[inst],w7_p[inst]); 
							w15_p[inst] <= w_p[inst][round_index_p - 14];
							w2_p[inst] <= w_p[inst][round_index_p - 1];
							w16_p[inst] <= w_p[inst][round_index_p - 15];
							w7_p[inst] <= w_p[inst][round_index_p - 6];
						end
						
						if(round_index_p != 0) begin
						// 64 round of COMPRESSION ALGORITHM
							{A_[inst], B_[inst], C_[inst], D_[inst], E_[inst], F_[inst], G_[inst], H_[inst]} <= sha256_op(A_[inst], 
							B_[inst], C_[inst], D_[inst], E_[inst], F_[inst], G_[inst], H_[inst], wt_p[inst], kt);
							round_index_p <= round_index_p + 7'b1;
						end
						else begin
							round_index_p <= round_index_p + 7'b1;
						end
					end		
					next_state <= COMPUTE;
				end
				//final round
				else begin
					//block setup
					next_state <= ADD;
				end
		   end
			
			
			//Write has values back to memory
		   WRITE: begin
				if(word_offset < INSTANCES && nonce < 15) begin
					present_write_data <= hash_out[word_offset+nonce];
					present_addr <= hash_out_addr + nonce + word_offset;
					word_offset <= word_offset + 5'b1;
					next_state <= WRITE;
				end
				else begin
					if(nonce >= 12) begin
						next_state <= IDLE;
					end
					else begin
						stage <= 1;
						nonce <= nonce + INSTANCES;
						next_state <= PAD_BLOCK;
					end
				end
			end
		endcase
   end
 end
   // Generate done when SHA256 hash computation has finished and moved to IDLE state
   
   assign done = (next_state == IDLE);
endmodule