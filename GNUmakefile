RUNTIME := runtime/table.j runtime/parser.j runtime/list.j runtime/stringtable.j
RUNTIME += runtime/modified.j runtime/wrap-around.j runtime/print.j
RUNTIME += runtime/convert.j runtime/context.j runtime/types.j
RUNTIME += runtime/instruction.j runtime/interpreter.j runtime/init.j

SRC := Jass/Parser.hs Jass/Ast.hs Jass/Printer.hs Jass/Opt/Rewrite.hs
SRC += Jass/Opt/Rewrite/SomeRules.hs Hot/Instruction/Opt/Rewrite/SomeRules.hs
SRC += Jass/LCA.hs
SRC += Hot/Ast.hs Hot/Types.hs Hot/Var.hs Hot/Instruction.hs
SRC += Hot/Instruction/Compiler.hs Hot/Instruction/Opt/Rewrite.hs
SRC += Hot/Init/Auto.hs Hot/Init/Stubs.hs Hot/Init/Rename.hs
SRC += Hot/JassHelper.hs Hot/HandleCode.hs
SRC += Data/Composeable.hs
SRC += Main.hs

HS_O := $(patsubst %.hs, %.o, $(SRC))
HS_HI := $(patsubst %.hs, %.hi, $(SRC))

PROCESSED := $(patsubst runtime/%.j, out/%.j, $(RUNTIME))

UPX := ./upx
UPXFLAGS := --best

Z := /cygdrive/c/Program\ Files/7-Zip/7z

.PHONY: clean process release all
.PHONY: patch126 patch129 patch130 patch131 patch132

all: patch132

# 1.26b to 1.28
patch128: COMMONJ=common-1.28.j
patch128: PATCH_LVL=128
patch128: jhcr

patch129: COMMONJ=common-1.29.j
patch129: PATCH_LVL=129
patch129: jhcr

patch130: COMMONJ=common-1.30.j
patch130: PATCH_LVL=130
patch130: jhcr

patch131: COMMONJ=common-1.31.j
patch131: PATCH_LVL=131
patch131: jhcr

patch132: COMMONJ=common-1.32.j
patch132: PATCH_LVL=132
patch132: jhcr


release: HSFLAGS=-O -osuf .oo -hisuf .hio
release: jhcr.zip


jhcr.exe: HSFLAGS=-O -osuf .oo -hisuf .hio
jhcr.exe: jhcr
	strip jhcr
	$(UPX) $(UPXFLAGS) jhcr.exe
    
jhcr.zip: jhcr.exe
	$(Z) u jhcr.zip jhcr.exe

convert: convert.hs
	cabal v1-exec -- ghc $(HSFLAGS) convert

runtime/convert.j Hot/Types.hs runtime/types.j runtime/g-type-bin.j: convert
	./convert $(COMMONJ)

jhcr: $(SRC) $(PROCESSED)
	cabal v1-exec -- ghc $(HSFLAGS) Main.hs -o jhcr

process: $(PROCESSED)

out/%.j: runtime/%.j runtime/alloc.j runtime/alloc-globals.j
	@mkdir -p $(@D)
	PATCH_LVL=$(PATCH_LVL) bash process.sh $< $@ JHCR_

init:
	cabal v1-sandbox init
	cabal v1-install megaparsec lens optparse-applicative file-embed gitrev \
                     lca hashable dlist

clean:
	rm -f $(PROCESSED) runtime/convert.j runtime/types.j Hot/Types.hs 
	rm -f $(HS_O) $(HS_HI)
	rm -f Main.o Main.hi
	rm -f convert convert.o convert.hi
	rm -f jhcr.zip jhcr 