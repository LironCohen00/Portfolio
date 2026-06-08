// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.


(LOOP)
	@SCREEN
	D=A
	@R0
	M=D 	//stores value of first register of pixels
	@KBD
	D=M		//store key press 
	@WHITE
	D;JEQ
	@BLACK
	0;JMP 

//turns screen to white
(WHITE) 
	@R0
	A=M  
	M=0 	// sets current register of pixels to white
	@R0	
	M=M+1	// increment current register 
	@SCREEN
	D=A
	@8191
	D=D+A   // D stores the address of the last register of pixels that needs to be changed
	@R0
	D=D-M  // If the address of the current register equals the address of the last register of pixels, then D=0
	@WHITE
	D;JGE	// if the current register is last register  register, then goto WHITE 
	@LOOP	
	0;JMP	// Goto LOOP once finsihed setting the screen to white

//turns screen to black
(BLACK) 
	@R0
	A=M
	M=-1 	// sets current register of pixels to white
	@R0	
	M=M+1	// increment current register
	@SCREEN
	D=A
	@8191
	D=D+A   // D stores the address of the last register of pixels that needs to be changed
	@R0
	D=D-M	// If the address of the current register equals the address of the last register of pixels, then D=0
	@BLACK
	D;JGE	// if the current register is NOT the 8191st register, then goto WHITE 
	@LOOP	
	0;JMP	// Goto LOOP once finsihed setting the screen to white

