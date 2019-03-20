RUNTIME := runtime/table.j runtime/parser.j runtime/list.j
RUNTIME += runtime/modified.j runtime/wrap-around.j runtime/print.j
RUNTIME += runtime/convert.j runtime/context.j runtime/types.j
RUNTIME += runtime/instruction.j runtime/interpreter.j runtime/init.j

SRC := Jass/Parser.hs Jass/Ast.hs Jass/Printer.hs
SRC += Hot/Ast.hs Hot/Instruction.hs Hot/Interpreter.hs Hot/Types.hs Hot/Var.hs
SRC += Hot/Jass/Init.hs Hot/Instruction/Compiler.hs Hot/HandleCode.hs
SRC += Hot/JassHelper.hs
SRC += Data/Composeable.hs
SRC += Main.hs

PROCESSED := $(patsubst runtime/%.j, out/%.j, $(RUNTIME))

HS_O := $(patsubst %.hs, %.o, $(SRC))
HS_HI := $(patsubst %.hs, %.hi, $(SRC))

UPX := ./upx
UPXFLAGS ?= --best

.PHONY: clean process

jhcr: HSFLAGS=-O
jhcr: Main
	cp Main jhcr
	strip jhcr
	$(UPX) $(UPXFLAGS) jhcr.exe

convert: convert.hs
	cabal exec -- ghc $(HSFLAGS) convert

runtime/convert.j Hot/Types.hs runtime/types.j: convert common.j
	./convert

Main: $(SRC) $(PROCESSED)
	cabal exec -- ghc $(HSFLAGS) Main

process: $(PROCESSED)

out/%.j: runtime/%.j runtime/alloc.j runtime/alloc-globals.j
	bash process.sh $< $@ JHCR_

clean:
	rm -f $(PROCESSED) $(HS_O) $(HS_HI) runtime/convert.j runtime/types.j 
	rm -f Hot/Types.hs Main convert tmp.w3x jhcr_war3map.j war3map.j jhcr.bin