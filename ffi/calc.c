#include <stdio.h>

int op_with_message(const char* msg, int (*op)(int, int), int x, int y)
{
  printf("%s: %d\n", msg, op(x, y));
}

int add(int x, int y)
{
  return x + y;
}

int sub (int x, int y)
{
  return x - y;
}

int mul(int x, int y)
{
  return x * y;
}

int div(int x, int y)
{
  if (y == 0)
    return 0;
  return x / y;
}