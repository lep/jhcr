RUNTIME := runtime/table.j runtime/parser.j runtime/list.j runtime/stringtable.j
RUNTIME += runtime/modified.j runtime/wrap-around.j runtime/print.j
RUNTIME += runtime/convert.j runtime/context.j runtime/types.j
RUNTIME += runtime/instruction.j runtime/interpreter.j runtime/init.j

SRC := Jass/Parser.hs Jass/Ast.hs Jass/Printer.hs
SRC += Hot/Ast.hs Hot/Types.hs Hot/Var.hs
SRC += Hot/Instruction/Compiler.hs Hot/Instruction.hs 
SRC += Hot/Init/Auto.hs Hot/Init/Stubs.hs Hot/Init/Rename.hs
SRC += Hot/JassHelper.hs Hot/HandleCode.hs
SRC += Data/Composeable.hs
SRC += Main.hs

HS_O := $(patsubst %.hs, %.o, $(SRC))
HS_HI := $(patsubst %.hs, %.hi, $(SRC))

PROCESSED := $(patsubst runtime/%.j, out/%.j, $(RUNTIME))

UPX := ./upx
UPXFLAGS := --best

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
	rm -f $(HS_O) $(HS_HI)
	rm -f jhcr convert 