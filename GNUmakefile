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

UPX := ./upx
UPXFLAGS ?= --best

.PHONY: clean process release all

all: jhcr


release: HSFLAGS=-O -osuf .oo -hisuf .hio
release: $(SRC) $(PROCESSED)
	cabal exec -- ghc $(HSFLAGS) Main.hs -o jhcr
	strip jhcr
	$(UPX) $(UPXFLAGS) jhcr.exe

convert: convert.hs
	cabal exec -- ghc $(HSFLAGS) convert

runtime/convert.j Hot/Types.hs runtime/types.j runtime/g-type-bin.j: convert common.j
	./convert

jhcr: $(SRC) $(PROCESSED)
	cabal exec -- ghc $(HSFLAGS) Main.hs -o jhcr

process: $(PROCESSED)

out/%.j: runtime/%.j runtime/alloc.j runtime/alloc-globals.j
	bash process.sh $< $@ JHCR_

clean:
	rm -f $(PROCESSED) runtime/convert.j runtime/types.j Hot/Types.hs 
	rm -f jhcr convert 