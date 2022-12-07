#SHELL=/bin/sh

FLAGS = $(PKG_CXXFLAGS) $(PKG_CXXPICFLAGS) $(PKG_CPPFLAGS)

SNAPPY_OBJS=snappy.o \
			snappy-c.o \
			snappy-sinksource.o \
			snappy-stubs-internal.o

all: $(SNAPPY_OBJS)

snappy.o: snappy.cc
	$(CXX) $(FLAGS) -c snappy.cc
snappy-c.o: snappy-c.cc
	$(CXX) $(FLAGS) -c snappy-c.cc
snappy-sinksource.o: snappy-sinksource.cc
	$(CXX) $(FLAGS) -c snappy-sinksource.cc
snappy-stubs-internal.o: snappy-stubs-internal.cc
	$(CXX) $(FLAGS) -c snappy-stubs-internal.cc

clean:
	rm -f *.o