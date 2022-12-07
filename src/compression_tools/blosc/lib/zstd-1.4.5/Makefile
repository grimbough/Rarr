
EXTRA_FLAGS=-I. -I./common -DXXH_NAMESPACE=ZSTD_
FLAGS=$(PKG_CFLAGS) $(PKG_CPICFLAGS) $(EXTRA_FLAGS)

ZSTDCOMMON_FILES := $(sort $(wildcard common/*.c))
ZSTDCOMP_FILES := $(sort $(wildcard compress/*.c))
ZSTDDECOMP_FILES := $(sort $(wildcard decompress/*.c))
ZSTD_FILES := $(ZSTDCOMMON_FILES) $(ZSTDCOMP_FILES) $(ZSTDDECOMP_FILES)

ZSTD_OBJ   := $(patsubst %.c,%.o,$(ZSTD_FILES))

.PHONY: clean 

all: $(ZSTD_OBJ)

common/debug.o: common/debug.c
	$(CC) $(FLAGS) -c common/debug.c -o common/debug.o
common/error_private.o: common/error_private.c
	$(CC) $(FLAGS) -c common/error_private.c -o common/error_private.o
common/pool.o: common/pool.c
	$(CC) $(FLAGS) -I. -c common/pool.c -o common/pool.o -o common/pool.o
common/xxhash.o: common/xxhash.c
	$(CC) $(FLAGS) -c common/xxhash.c -o common/xxhash.o
common/entropy_common.o: common/entropy_common.c
	$(CC) $(FLAGS) -c common/entropy_common.c -o common/entropy_common.o
common/fse_decompress.o: common/fse_decompress.c
	$(CC) $(FLAGS) -c common/fse_decompress.c -o common/fse_decompress.o
common/threading.o: common/threading.c
	$(CC) $(FLAGS) -c common/threading.c -o common/threading.o
common/zstd_common.o: common/zstd_common.c
	$(CC) $(FLAGS) -c common/zstd_common.c -o common/zstd_common.o

compress/fse_compress.o: compress/fse_compress.c
	$(CC) $(FLAGS) -c compress/fse_compress.c -o compress/fse_compress.o
compress/huf_compress.o: compress/huf_compress.c
	$(CC) $(FLAGS) -c compress/huf_compress.c -o compress/huf_compress.o
compress/zstd_double_fast.o: compress/zstd_double_fast.c
	$(CC) $(FLAGS) -c compress/zstd_double_fast.c -o compress/zstd_double_fast.o
compress/zstd_lazy.o: compress/zstd_lazy.c
	$(CC) $(FLAGS) -c compress/zstd_lazy.c -o compress/zstd_lazy.o
compress/zstdmt_compress.o: compress/zstdmt_compress.c
	$(CC) $(FLAGS) -c compress/zstdmt_compress.c -o compress/zstdmt_compress.o
compress/hist.o: compress/hist.c
	$(CC) $(FLAGS) -c compress/hist.c -o compress/hist.o
compress/zstd_compress.o: compress/zstd_compress.c
	$(CC) $(FLAGS) -c compress/zstd_compress.c -o compress/zstd_compress.o
compress/zstd_compress_literals.o: compress/zstd_compress_literals.c	
	$(CC) $(FLAGS) -c compress/zstd_compress_literals.c -o compress/zstd_compress_literals.o
compress/zstd_compress_sequences.o: compress/zstd_compress_sequences.c	
	$(CC) $(FLAGS) -c compress/zstd_compress_sequences.c -o compress/zstd_compress_sequences.o
compress/zstd_compress_superblock.o: compress/zstd_compress_superblock.c	
	$(CC) $(FLAGS) -c compress/zstd_compress_superblock.c -o compress/zstd_compress_superblock.o
compress/zstd_fast.o: compress/zstd_fast.c
	$(CC) $(FLAGS) -c compress/zstd_fast.c -o compress/zstd_fast.o
compress/zstd_ldm.o: compress/zstd_ldm.c
	$(CC) $(FLAGS) -c compress/zstd_ldm.c -o compress/zstd_ldm.o
compress/zstd_opt.o: compress/zstd_opt.c	
	$(CC) $(FLAGS) -c compress/zstd_opt.c -o compress/zstd_opt.o

decompress/huf_decompress.o: decompress/huf_decompress.c
	$(CC) $(FLAGS) -c decompress/huf_decompress.c -o decompress/huf_decompress.o
decompress/zstd_ddict.o: decompress/zstd_ddict.c
	$(CC) $(FLAGS) -c decompress/zstd_ddict.c -o decompress/zstd_ddict.o
decompress/zstd_decompress_block.o: decompress/zstd_decompress_block.c
	$(CC) $(FLAGS) -c decompress/zstd_decompress_block.c -o decompress/zstd_decompress_block.o
decompress/zstd_decompress.o: decompress/zstd_decompress.c
	$(CC) $(FLAGS) -c decompress/zstd_decompress.c -o decompress/zstd_decompress.o


clean:
	@$(RM) *.o common/*.o compress/*.o decompress/*.o 


