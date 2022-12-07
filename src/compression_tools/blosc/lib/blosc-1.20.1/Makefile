EXTRA_FLAGS+=-I../lz4-1.9.2 -I../snappy-1.1.1 -I../zstd-1.4.5 -msse2 -mavx2
FLAGS=$(PKG_CFLAGS) $(PKG_CPICFLAGS) $(PKG_CPPFLAGS) $(EXTRA_FLAGS)

BLOSC_FILES := $(sort $(wildcard *.c))

BLOSC_OBJS=	bitshuffle-avx2.o shuffle-avx2.o bitshuffle-sse2.o shuffle-sse2.o bitshuffle-generic.o shuffle-generic.o shuffle.o blosclz.o blosc.o fastcopy.o


all: $(BLOSC_OBJS)

bitshuffle-avx2.o: bitshuffle-avx2.c
	$(CC) $(FLAGS) -c bitshuffle-avx2.c
shuffle-avx2.o: shuffle-avx2.c
	$(CC) $(FLAGS) -c shuffle-avx2.c
bitshuffle-sse2.o: bitshuffle-sse2.c
	$(CC) $(FLAGS) -c bitshuffle-sse2.c
shuffle-sse2.o: shuffle-sse2.c
	$(CC) $(FLAGS) -c shuffle-sse2.c
bitshuffle-generic.o: bitshuffle-generic.c
	$(CC) $(FLAGS) -c bitshuffle-generic.c
shuffle-generic.o: shuffle-generic.c
	$(CC) $(FLAGS) -c shuffle-generic.c
shuffle.o: shuffle.c
	$(CC) $(FLAGS) -DSHUFFLE_SSE2_ENABLED -DSHUFFLE_AVX2_ENABLED -c shuffle.c
blosclz.o: blosclz.c
	$(CC) $(FLAGS) -c blosclz.c
blosc.o: blosc.c
	$(CC) $(FLAGS) -c blosc.c
fastcopy.o: fastcopy.c
	$(CC) $(FLAGS) -c fastcopy.c

clean:
	rm -f *.o
