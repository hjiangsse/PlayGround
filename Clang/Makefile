#This make file compile TwoPass.c
#base on BitVec.h and BitVec.c
CC=gcc
#CFLAGS will be the options passed to the compiler
CFLAGS=-c -Wall

all: TwoPass

BitVec.o: BitVec.c
	$(CC) $(CFALGS) BitVec.c BitVec.h

TwoPass.o: TwoPass.c
	$(CC) $(CFALGS) TwoPass.c BitVec.h

TwoPass: TwoPass.o BitVec.o
	$(CC) TwoPass.o BitVec.o -o TwoPass

clean:
	rm -rf *.o
