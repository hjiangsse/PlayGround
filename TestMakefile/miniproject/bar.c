#include "foo.h"

int main(int argc, char **argv) {
  printf("Print out the message from foo: \n");
  tell_foo();
  printf("Now in the main function in bar.c\n");
  return 0;
}
