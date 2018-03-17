RUNTIME:= runtime/table.j runtime/stringtable.j runtime/instruction-parser.j
RUNTIME+= runtime/modified.j runtime/scopes.j runtime/wrap-around.j
RUNTIME+= runtime/heap.j runtime/convert.j runtime/context.j
RUNTIME+= runtime/instruction.j runtime/interpreter.j runtime/names.j

PROCESSED:=$(patsubst runtime/%.j, out/%.j, $(RUNTIME))

.PHONY: all

all: ${PROCESSED}

out/%.j: runtime/%.j
	bash process.sh $^ $@ JHCR_

Jass/Tokenizer.hs: Jass/jass.x
	alex -o $@ $^
