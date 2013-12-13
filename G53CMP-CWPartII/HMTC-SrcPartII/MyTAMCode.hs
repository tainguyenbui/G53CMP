
module MyTAMCode where

import TAMCode
import TAMInterpreter

print1ton = [
	     GETINT,
	     LOAD (SB 0),
	     LOADL 0,
	     GTR,
	     JUMPIFZ "negInput",
	     LOADL 1,
	     Label "loop",
	     LOAD (ST (-1)),
	     PUTINT,
	     LOADL 1,
	     ADD,
	     LOAD (SB 0),
	     LOAD (SB 1),
	     EQL,
	     JUMPIFZ "loop",
	     JUMP "end",
	     Label "negInput",
	     LOADL 1,
	     LSS,
	     Label "end",
	     PUTINT,
	     HALT
	     ]

fac = 	     [
    	     GETINT,	 
	  LOAD (SB 0),
	  LOADL (0),
	  GTR,
	  JUMPIFZ "negInput",
	  LOAD (SB 0),
	  Label "loop",
	  LOADL 1,
	  SUB,
	  LOAD (SB 1),
	  JUMPIFZ "end",
	  LOAD (SB 0),
	  LOAD (SB 1),
	  MUL,
	  STORE (SB 0),
	  LOAD (SB 1),
	  JUMPIFNZ "loop",
	  Label "negInput",
	  LOADL 0,
	  LOAD (SB 0),
	  EQL,
	  Label "end",
	  ADD,
	  PUTINT,
	  HALT]

	  

