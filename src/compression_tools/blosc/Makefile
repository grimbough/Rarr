BLOSC_OBJS=	lib/blosc-1.20.1/bitshuffle-avx2.o \
			lib/blosc-1.20.1/blosc.o \
			lib/blosc-1.20.1/shuffle-avx2.o \
			lib/blosc-1.20.1/shuffle-sse2.o \
			lib/blosc-1.20.1/bitshuffle-generic.o \
			lib/blosc-1.20.1/blosclz.o \
			lib/blosc-1.20.1/shuffle.o \
			lib/blosc-1.20.1/bitshuffle-sse2.o \
			lib/blosc-1.20.1/fastcopy.o \
			lib/blosc-1.20.1/shuffle-generic.o

LZ4_OBJS=	lib/lz4-1.9.2/lz4.o \
			lib/lz4-1.9.2/lz4hc.o

SNAPPY_OBJS=lib/snappy-1.1.1/snappy.o \
			lib/snappy-1.1.1/snappy-c.o \
			lib/snappy-1.1.1/snappy-sinksource.o \
			lib/snappy-1.1.1/snappy-stubs-internal.o

ZSTDCOMMON_FILES := $(sort $(wildcard lib/zstd-1.4.5/common/*.c))
ZSTDCOMP_FILES := $(sort $(wildcard lib/zstd-1.4.5/compress/*.c))
ZSTDDECOMP_FILES := $(sort $(wildcard lib/zstd-1.4.5/decompress/*.c))
ZSTD_FILES := $(ZSTDCOMMON_FILES) $(ZSTDCOMP_FILES) $(ZSTDDECOMP_FILES)
ZSTD_OBJS := $(patsubst %.c,%.o,$(ZSTD_FILES))


all: libblosc.a

libblosc.a: blosc
	rm -f libblosc.a
	ar cq libblosc.a $(BLOSC_OBJS) $(SNAPPY_OBJS) $(LZ4_OBJS) $(ZSTD_OBJS)
	ranlib libblosc.a

.PHONY: blosc

blosc: subdirs
	$(MAKE) -C lib/blosc-1.20.1 all

SUBDIRS=lib/snappy-1.1.1 lib/lz4-1.9.2 lib/zstd-1.4.5

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	rm -f libH5Zblosc.so
	rm -f libblosc.a
	rm -f lib/blosc-1.20.1/*.o
	rm -f lib/snappy-1.1.1/*.o
	rm -f lib/lz4-1.9.2/*.o
	$(MAKE) -C lib/zstd-1.4.5/ clean
