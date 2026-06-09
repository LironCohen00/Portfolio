Hack Computer Architecture
===========================

Construction of the full Hack hardware platform in HDL, integrating all
previously built chips into a working von Neumann computer.

Design:

Memory.hdl — unified 32K address space:
  - RAM16K (addresses 0–16383): general-purpose data memory
  - Screen (addresses 16384–24575): memory-mapped display; writing to these
    addresses directly sets pixels on the screen
  - Keyboard (address 24576): memory-mapped input; reading this address returns
    the currently pressed key's ASCII code

CPU.hdl — the Hack CPU:
  - Decodes each 16-bit instruction into control signals (A-instruction or
    C-instruction)
  - A-instruction: loads a 15-bit constant into the A register
  - C-instruction: selects ALU inputs (A or M), ALU operation, destination
    registers (A, D, M), and jump condition
  - Contains: A register, D register, ALU (from previous project), and the
    program counter; produces outM (data to write), writeM (write enable),
    addressM (memory address), and pc (next instruction address)

Computer.hdl — top-level chip:
  - Wires CPU, Memory, and ROM together into a complete computer
  - ROM holds the program (loaded externally); the CPU fetches instructions
    from ROM[PC] each clock cycle

Language: HDL (Hack Hardware Description Language)
