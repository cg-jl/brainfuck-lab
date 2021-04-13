bin/phase%: src/phase%.c
	@mkdir -p bin && \
		gcc -o $@ $^


clean:
	rm -rf bin phases

tests: $(BINARIES)
	@for phase in $(patsubst bin/phase%,%,$(BINARIES)); do \
		sh test-phase.sh $$phase || exit 1;\
		done

BINARIES := $(patsubst %,bin/phase%,$(shell find src/ -maxdepth 1 -name 'phase*.c' | sed -e 's/src\/phase//;s/\.c//' | sort -n ))

bin: $(BINARIES)

all: bin

