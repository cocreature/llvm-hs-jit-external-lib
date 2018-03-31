.PHONY: clean
all: libexternalstatic.a libexternaldynamic.so
libexternalstatic.a: lib.o
	ar rcs $@ $^
libexternaldynamic.so: lib.o
	gcc -shared -o $@ $^
lib.o: lib.c
	gcc -fPIC -c -o $@ $^
clean:
	rm -f *.a *.so *.o
