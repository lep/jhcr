RUNTIME := runtime/table.j runtime/parser.j runtime/list.j runtime/stringtable.j
RUNTIME += runtime/modified.j runtime/wrap-around.j runtime/print.j
RUNTIME += runtime/convert.j runtime/context.j runtime/types.j
RUNTIME += runtime/instruction.j runtime/interpreter.j runtime/init.j

PROCESSED := $(patsubst runtime/%.j, out/%.j, $(RUNTIME))


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
nix: $(PROCESSED) Hot/Types.hs Hot/CommonJHash.hs
	ghc -DPATCH_LVL=$(PATCH_LVL) Main.hs -o jhcr

build: CABAL_FLAGS+=--ghc-options=-DPATCH_LVL=$(PATCH_LVL)
build: $(PROCESSED) Hot/Types.hs Hot/CommonJHash.hs
	cabal build $(CABAL_FLAGS) jhcr

jhcr.exe: build
	rm -f $@
	strip $$(cabal list-bin $(CABAL_FLAGS) jhcr)
	upx -qq $$(cabal list-bin $(CABAL_FLAGS) jhcr) -o $@

runtime/convert.j Hot/Types.hs Hot/CommonJHash.hs runtime/types.j runtime/g-type-bin.j: $(COMMONJ)
	cabal run $(CABAL_FLAGS) convert -- $(COMMONJ)

process: $(PROCESSED)

out/%.j: runtime/%.j runtime/alloc.j runtime/alloc-globals.j
	@mkdir -p $(@D)
	PATCH_LVL=$(PATCH_LVL) bash process.sh $< $@ JHCR_

clean:
	rm -f $(PROCESSED) runtime/convert.j runtime/types.j Hot/Types.hs Hot/CommonJHash.hs
