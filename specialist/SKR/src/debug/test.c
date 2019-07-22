#include "debug.h"

int
main(int argc, char **argv)
{
  int status = cfg_read("test.cfg");
  printf("status = %d\n", status);
}
