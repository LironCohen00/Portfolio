Objective:
Build a basic VM translator, focusing on the implementation of the VM language's stack arithmetic and memory access commands.

Use your VM translator to translate the VM files, yielding corresponding programs written in the Hack assembly language. The VM translator should be invoked using something like "VMTranslator fileName.vm", where the string fileName.vm is the translator's input, i.e. the name of a text file containing VM commands. The translator creates an output file named fileName.asm, which is stored in the same directory of the input file.