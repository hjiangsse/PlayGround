CC=gcc

bar: bar.c foo.o
	$(CC) -o bar bar.c foo.o

foo.o: foo.c foo.h
	$(CC) -c foo.c

run:
	./bar

clean:
	rm -f bar foo.o
