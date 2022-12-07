FLAGS=$(PKG_CFLAGS) $(PKG_CPICFLAGS)

LZ4_OBJS=lz4.o lz4hc.o

all: $(LZ4_OBJS)

lz4.o: lz4.c lz4.h
	$(CC) $(FLAGS) -c lz4.c
lz4hc.o: lz4hc.c lz4hc.h lz4.c
	$(CC) $(FLAGS) -c lz4hc.c

clean:
	rm -f *.o
