module simplified_sha256 #(parameter integer NUM_OF_WORDS = 40)(
 input logic  clk, rst_n, start, // standard inputs
 input logic  [15:0] input_addr, hash_addr, // CONSTANTS = address values
 input logic [31:0] memory_read_data, // data that is read from memory
 output logic done, memory_clk, enable_write, // memory
 output logic [15:0] memory_addr, // current memory address for READ and WRITE
 output logic [31:0] memory_write_data); // data to write to memory

// FSM state variables 
enum logic [2:0] {IDLE, READ, BLOCK, COMPUTE, WRITE} state,next_state;

//TODO ??
parameter integer SIZE = ??; 


// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables
logic [31:0] w[64]; // hash computation temporary variable
logic [31:0] message[16]; //temporary BLOCK storage (16 WORDS)
logic [31:0] wt;
logic [31:0] S0,S1; // wires in hash computation. Don't touch.
logic [31:0] hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7; //INITIAL hash and MIDDLE hash values
logic [31:0] A, B, C, D, E, F, G, H; //OUTPUT hash and MIDDLE hash values
//logic [ 7:0] i, j; // i = block index in message, j = word index in block
//logic [15:0] offset; // in word address
logic [ 7:0] num_blocks;
logic        enable_write;
logic [15:0] present_addr;
logic [31:0] present_write_data;
logic [512:0] data_read;
//logic [ 7:0] tstep;

logic [6:0] round_index;
logic [$clog2(NUM_OF_WORDS/16+1):0] block_offset;
logic [4:0] word_offset;


// Initialize after index
// start loop ------
// READ FROM MEMORY / INITIALIZE A
// START COMPUTE OPERATION
// ADD OPERATION
// goto start ------

//TODO Figure out where the initial hash values come from
parameter int [31:0] h_initial = 32'h6a09e667;

// SHA256 K constants
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
assign memory_addr = present_addr + word_offset; // present_addr = in WORDS, word_offset in WORDS, block_offset in BLOCKS        //assign memory_addr = present_addr + word_offset;
												 // word_offset = offset in WORDS
assign memory_we = enable_write;
assign memory_write_data = present_write_data;


assign num_blocks = determine_num_blocks(NUM_OF_WORDS); 
//assign tstep = (i - 1);


// Note : Function defined are for reference purpose. Feel free to add more functions or modify below.
// Function to determine number of blocks in memory to fetch

//TODO Check if the input is the raw message or message with padding
function logic [15:0] determine_num_blocks(input logic [31:0] size);

	//rounding
	logic [31:0] quotient = (size >> 4); //quotient = size/16, size = 101000
	logic [31:0] n = quotient << 4; //n = quotient * 16, = 100000 quotient=000010

	if(size ^ n) begin
		determine_num_blocks = quotient + 1;
	end
	else begin
		determine_num_blocks = quotient;
	end
endfunction

// SHA256 hash round
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    //TODO Check that it is written as such that it looks like it isn't copied
	S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
	maj = (a & b) ^ (a & c) ^ (b & c);
	t2 = S0 + maj;
	S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    ch = (e & f) ^ ((~e) & g);
    t1 = h + S1 + ch + k[t] + w;
    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
	 //            A       B  C  D  E       F  G  H
end
endfunction

// expand message to w[64]
function logic [31:0] expand_message(input logic[31:0] W[64],
	input logic [6:0] t);
	logic [31:0] S1, S0; // internal signals
	begin
		//S0 = (Wt-15 rightrotate 7) xor (Wt-15 rightrotate 18) xor (Wt-15 rightshift 3)
		S0 = rightrotate(W[t-15], 7) xor rightrotate(W[t-15], 18) xor rightrotate(W[t-15], 3); 
		//S1 = (Wt-2 rightrotate 17) xor (Wt-2 rightrotate 19) xor (Wt-2 rightshift 10)
		S1 = rightrotate(WAIT[t-2],17) xor rightrotate(W[t-2],19) xor rightrotate(W[t-2],10); 
		//Wt = Wt-16 + s0 + Wt-7 + s1
		expand_message = W[t-16] + S0 + W[t-7] + S1; 
		
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

always_ff @(posedge clk, negedge rst_n)
begin
  if (!rst_n) begin
    state <= IDLE;

  end 
  else begin 
	state <= next_state;
	
  end
end



// SHA-256 FSM 
// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
// and write back hash value back to memory

always_comb begin
  if (!rst_n) begin
	next_state = IDLE;
  end
  else begin 
	  case (state)
		// Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
		IDLE: begin 
			if(start) begin 
				present_addr = input_addr; //set address for input message
				word_offset = 0;
				block_offset = 0;
				next_state = BLOCK;
				A=B=C=D=E=F=G=H = 32'b0;
				{hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash6, hash7} = h_initial;
		   end
		end

		BLOCK: begin //ADD OPERATION
			
			//ADD OPERATION
			{hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash6, hash7} = {hash0+A, hash1+B, hash2+C, hash3+D, hash4+E, hash5+F, hash6+G, hash7+H};
			
			if(block_offset == num_blocks-1)begin // if this is the last block, WRITE
				
				word_offset = 0;
				data_read = {hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7}

				next_state = WRITE;
			end else begin //if it is not the last block, begin READ
				
				word_offset = 0;
				{A, B, C, D, E, F, G, H} = {hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7};

				next_state = READ;
			end
		end

		READ: begin //read a BLOCK from memory 
			if(word_offset<=15) begin //read more because entire block is not read yet [blocks 0 to 15]
				message[word_offset] = memory_read_data; 
				word_offset += 1; 
				
				next_state = READ; //do another read while block is not full
			end else begin // finished reading
				present_addr = present_addr + 16;//pre-emplively increment memory
				block_offset += 1; //monitor which block we are on in the message

				next_state = COMPUTE_W;
			end
			
		end
		
		COMPUTE_W: begin
			if(round_index<=63) begin
				if(round_index<=15) begin
					w[round_index] = message[round_index];
				end else begin
					w[round_index] = expand_message(w, round_index)
				end

				round_index = round_index + 1;
				next_state = COMPUTE_W;
			end else begin //final round
				next_state = COMPRESSION;
				round_index = 0;
			end
		end


		COMPRESSION: begin // COMPRESSION ALGORITHM
			if(round_index<=63) begin // if there are still rounds left
				
				{A, B, C, D, E, F, G, H} = sha256_op(A, B, C, D, E, F, G, H, w[t], round_index);

				round_index = round_index + 1;
				next_state = COMPRESSION;
			end else begin //final round
				next_state = BLOCK;
			end

		end

		// h0 to h7 each are 32 bit hashes, which makes up total 256 bit value
		// h0 to h7 after compute stage has final computed hash value
		// write back these h0 to h7 to memory starting from output_addr
		WRITE: begin
		
			enable_write = 1;
			
			if(word_offset<=15) begin
				memory_write_data = data_read[(word_offset+1)*32-1:(word_offset)*32]
				word_offset += 1; 
				next_state = WRITE; //do another read while block is not full
			end else begin
				next_state = IDLE;
			end
		
		end
      endcase
	end
end

// Generate done when SHA256 hash computation has finished and moved to IDLE state

assign done = (state == IDLE);

endmodule
