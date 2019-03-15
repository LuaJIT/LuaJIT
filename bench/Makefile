CC=gcc

LUA_FILE_ARG = -a
LUA_ARG = $(patsubst %.lua,%,$^)

ifndef LUA_BIN
BENCH_BIN = luajit-bench
LUA_BIN_NAME = ./$(BENCH_BIN)
else
ifdef BENCH_DURATION
BENCH_ARG = -t $(BENCH_DURATION)
endif
LUA_BIN_NAME = $(LUA_BIN) luajit-bench.lua $(BENCH_ARG)
endif

BENCHMARKS = array3d binary-trees chameneos coroutine-ring euler14-bit \
	     fannkuch fasta k-nucleotide life mandelbrot mandelbrot-bit md5 \
	     meteor nbody nsieve nsieve-bit nsieve-bit-fp partialsums \
	     pidigits-nogmp ray recursive-ack recursive-fib revcomp \
	     scimark-2010-12-20 scimark-fft scimark-lu series scimark-sparse \
	     scimark-sor spectral-norm sum-file

# Specify benchmarks that need input files.
INPUT-k-nucleotide = FASTA_1000000
INPUT-revcomp = FASTA_1000000
INPUT-sum-file = SUMCOL_1000

input-file = $(INPUT-$(patsubst %.result,%,$@))
input-file-arg = $(if $(input-file),$(LUA_FILE_ARG) $(input-file),)
LUA_CMD = $(LUA_BIN_NAME) $(LUA_ARG) $(input-file-arg)

bench_targets = $(patsubst %,%.result,$(BENCHMARKS))

all: $(BENCH_BIN) $(bench_targets)

%.result: %.lua
	@$(LUA_CMD)

ifndef LUA_BIN
ifdef BENCH_DURATION
DURATION = -DBENCH_DURATION=$(BENCH_DURATION)
endif

LUAJIT_A = ../src/$(FILE_A)

$(BENCH_BIN): $(LUAJIT_A) $(BENCH_BIN).c Makefile
	$(CC) $@.c -std=gnu11 $(DURATION) -g -O3 -c -o $@.o -I ../src
	$(CC) $@.o -lpthread $< -lm -ldl -o $@

# Build the luajit static library if it doesn't exist.
$(LUAJIT_A):
	make -C ..

clean:
	rm -f $(BENCH_BIN) main.o
endif
