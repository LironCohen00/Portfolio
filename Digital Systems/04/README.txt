Program		Description
	
Mult.asm	Multiplication: in the Hack computer, the top 16 RAM words (RAM[0]...RAM[15]) are also referred 		to as R0...R15.

		With this terminology in mind, this program computes the value R0*R1 and stores the result in 			R2.The program assumes that R0>=0, R1>=0, and R0*R1<32768. Your program need not test these 			conditions, but rather assume that they hold.

Fill.asm	I/O handling: this program illustrates low-level handling of the screen and keyboard devices, as 		follows.


		The program runs an infinite loop that listens to the keyboard input. When a key is pressed (any 		key), the program blackens the screen, i.e. writes "black" in every pixel; the screen should remain 		fully black as long as the key is pressed.

		When no key is pressed, the program clears the screen, i.e. writes "white" in every pixel; the 			screen should remain fully clear as long as no key is pressed.