RAM — Registers and Memory
===========================

Implementation of the Hack computer's sequential memory chips in HDL, building
from a single-bit storage element up to a 16K-word RAM.

Design:
- DFF (Data Flip-Flop): primitive sequential element — outputs its input from
  the previous clock cycle. All memory in the system is ultimately built from DFFs.
- Bit: 1-bit register using a DFF and a Mux to decide whether to load a new
  value or hold the current one (controlled by the load bit).
- Register: 16-bit register built from 16 Bit chips, all sharing one load bit.
- RAM8/64/512/4K/16K: hierarchical RAM built by composing smaller banks.
  RAM8 holds 8 registers; each larger bank contains 8 instances of the previous
  level. A DMux routes the load signal to the correct sub-bank; a Mux selects
  the correct sub-bank's output. Address width grows by 3 bits at each level.
- PC (Program Counter): 16-bit counter that supports load (jump to address),
  inc (advance by 1 each clock cycle), and reset (set to 0). Drives the CPU's
  instruction fetch.

Language: HDL (Hack Hardware Description Language)
