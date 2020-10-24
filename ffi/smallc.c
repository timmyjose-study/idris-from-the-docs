#include <stdio.h>

int add(int x, int y)
{
  return x + y;
}

int add_with_message(const char* msg, int x, int y)
{
  printf("%s: %d + %d = %d\n", msg, x, y, x + y);
  return x + y;
}