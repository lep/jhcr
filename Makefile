RUNTIME := runtime/table.j runtime/instruction-parser.j
RUNTIME += runtime/modified.j runtime/wrap-around.j
RUNTIME += runtime/convert.j runtime/context.j runtime/types.j
RUNTIME += runtime/instruction.j runtime/interpreter.j runtime/init.j

SRC := Jass/Parser.hs Jass/Ast.hs Jass/Printer.hs
SRC += Hot/Ast.hs Hot/Instruction.hs Hot/Interpreter.hs Hot/Types.hs Hot/Var.hs
SRC += Hot/Jass/Init.hs Hot/Instruction/Compiler.hs
SRC += Data/Composeable.hs
SRC += Main.hs

PROCESSED := $(patsubst runtime/%.j, out/%.j, $(RUNTIME))

HS_O := $(patsubst %.hs, %.o, $(SRC))
HS_HI := $(patsubst %.hs, %.hi, $(SRC))

.PHONY: process check clean run

process: $(PROCESSED)

run: tmp.w3x
	cp tmp.w3x /cygdrive/c/Users/lep/Documents/Warcraft\ III/Maps/jhcr/tmp.w3x
	/cygdrive/e/Battle.Net/Warcraft\ III.exe -nativefullscr -loadfile 'C:\Users\lep\Documents\Warcraft III\Maps\jhcr\tmp.w3x'

war3map.j: test.w3x
	cp test.w3x tmp.w3x
	./MPQEditor /extract tmp.w3x war3map.j
	rm tmp.w3x

jhcr.j: war3map.j Main $(PROCESSED)
	./Main init common.j Blizzard.j war3map.j

tmp.w3x: jhcr.j test.w3x
	cp test.w3x tmp.w3x
	./MPQEditor /add tmp.w3x jhcr.j war3map.j

convert: convert.hs
	cabal exec -- ghc convert

runtime/convert.j Hot/Types.hs runtime/types.j: convert common.j
	./convert

Main: $(SRC)
	cabal exec -- ghc Main

out/%.j: runtime/%.j
	bash process.sh $^ $@ JHCR_

check: $(GEN) $(PROCESSED)
	./pjass.exe common.j Blizzard.j out/table.j out/convert.j \
    out/wrap-around.j out/modified.j generated/stubs.j generated/i2code.j \
    generated/setget.j generated/call_predefined.j out/context.j \
    out/instruction.j out/instruction-parser.j out/interpreter.j out/init.j

clean:
	rm -f $(PROCESSED) $(HS_O) $(HS_HI) runtime/convert.j runtime/types.j 
	rm -f Hot/Types.hs Main convert tmp.w3x jhcr.j war3map.j jhcr.state jhcr.txt