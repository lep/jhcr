RUNTIME := runtime/table.j runtime/parser.j runtime/list.j runtime/stringtable.j
RUNTIME += runtime/modified.j runtime/wrap-around.j runtime/print.j
RUNTIME += runtime/convert.j runtime/context.j runtime/types.j
RUNTIME += runtime/instruction.j runtime/interpreter.j runtime/init.j

PROCESSED := $(patsubst runtime/%.j, out/%.j, $(RUNTIME))


.PHONY: clean process all build
.PHONY: patch128 patch132
.PHONY: configure-old-patch configure-new-patch

all: patch132

# 1.26b to 1.28
patch128: COMMONJ=common-1.28.j
patch128: PATCH_LVL=128
patch128: clean configure-old-patch build

patch132: COMMONJ=common-1.32.9.j
patch132: PATCH_LVL=132
patch132: clean configure-new-patch build

configure-old-patch:
	cabal configure -f+old-patch

configure-new-patch:
	cabal configure -f-old-patch

build: $(PROCESSED) Hot/Types.hs
	cabal build jhcr

runtime/convert.j Hot/Types.hs runtime/types.j runtime/g-type-bin.j: $(COMMONJ)
	cabal run convert -- $(COMMONJ)

process: $(PROCESSED)

out/%.j: runtime/%.j runtime/alloc.j runtime/alloc-globals.j
	@mkdir -p $(@D)
	PATCH_LVL=$(PATCH_LVL) bash process.sh $< $@ JHCR_

clean:
	rm -f $(PROCESSED) runtime/convert.j runtime/types.j Hot/Types.hs 
