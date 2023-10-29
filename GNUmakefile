RUNTIME := src/runtime/table.j src/runtime/parser.j src/runtime/list.j src/runtime/stringtable.j
RUNTIME += src/runtime/modified.j src/runtime/wrap-around.j src/runtime/print.j
RUNTIME += src/runtime/convert.j src/runtime/context.j src/runtime/types.j
RUNTIME += src/runtime/instruction.j src/runtime/interpreter.j src/runtime/init.j

PROCESSED := $(patsubst src/runtime/%.j, out/%.j, $(RUNTIME))


.PHONY: clean process all build
.PHONY: patch128 patch133

all: patch133

# 1.26b to 1.28
patch128: COMMONJ=common-1.28.j
patch128: PATCH_LVL=128
patch128: clean build

patch133: COMMONJ=common-1.33.j
patch133: PATCH_LVL=133
patch133: build

nix: COMMONJ=common-1.33.j
nix: PATCH_LVL=133
nix: $(PROCESSED) src/Hot/Types.hs src/Hot/CommonJHash.hs
	ghc -DPATCH_LVL=$(PATCH_LVL) -isrc src/Main.hs -o jhcr

build: CABAL_FLAGS+=--ghc-options=-DPATCH_LVL=$(PATCH_LVL)
build: $(PROCESSED) src/Hot/Types.hs src/Hot/CommonJHash.hs
	cabal build $(CABAL_FLAGS) jhcr

jhcr.exe: build
	rm -f $@
	strip $$(cabal list-bin $(CABAL_FLAGS) jhcr)
	upx -qq $$(cabal list-bin $(CABAL_FLAGS) jhcr) -o $@

src/runtime/convert.j src/Hot/Types.hs src/Hot/CommonJHash.hs src/runtime/types.j src/runtime/g-type-bin.j: $(COMMONJ)
	cabal run $(CABAL_FLAGS) convert -- $(COMMONJ)

process: $(PROCESSED)

out/%.j: src/runtime/%.j src/runtime/alloc.j src/runtime/alloc-globals.j
	@mkdir -p $(@D)
	PATCH_LVL=$(PATCH_LVL) bash src/process.sh $< $@ JHCR_

clean:
	rm -f $(PROCESSED)
	rm -f src/runtime/convert.j src/runtime/types.j src/Hot/Types.hs src/Hot/CommonJHash.hs
	rm -f jhcr.exe
