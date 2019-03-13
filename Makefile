RUNTIME := runtime/table.j runtime/instruction-parser.j
RUNTIME += runtime/modified.j runtime/scopes.j runtime/wrap-around.j
RUNTIME += runtime/convert.j runtime/context.j
RUNTIME += runtime/instruction.j runtime/interpreter.j runtime/init.j

GEN := generated/stubs.j generated/i2code.j
GEN += generated/setget.j generated/call_predefined.j

SRC := Jass/Parser.hs Jass/Ast.hs Jass/Printer.hs
SRC += Hot/Ast.hs Hot/Instruction.hs Hot/Interpreter.hs Hot/Types.hs Hot/Var.hs
SRC += Hot/Jass/Init.hs Hot/Instruction/Compiler.hs
SRC += Data/Composeable.hs
SRC += Main.hs

PROCESSED := $(patsubst runtime/%.j, out/%.j, $(RUNTIME))

HS_O := $(patsubst %.hs, %.o, $(SRC))
HS_HI := $(patsubst %.hs, %.hi, $(SRC))

.PHONY: process check clean

process: $(PROCESSED)

convert: convert.hs
	cabal exec -- ghc convert

runtime/convert.j Hot/Types.hs: convert common.j
	./convert

Main: $(SRC)
	cabal exec -- ghc Main

out/%.j: runtime/%.j
	bash process.sh $^ $@ JHCR_

check: $(GEN) $(PROCESSED)
	./pjass.exe common.j Blizzard.j out/table.j out/scopes.j out/convert.j \
    out/wrap-around.j out/modified.j generated/stubs.j generated/i2code.j \
    generated/setget.j generated/call_predefined.j out/context.j \
    out/instruction.j out/instruction-parser.j out/interpreter.j out/init.j

clean:
	rm -f $(PROCESSED) $(HS_O) $(HS_HI) runtime/convert.j Main convert