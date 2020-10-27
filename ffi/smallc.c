#include <stdio.h>
#include <stdlib.h>

int add(int x, int y)
{
  return x + y;
}

int sub(int x, int y)
{
  return x - y;
}

int mul(int x, int y)
{
  return x * y;
}

int divide(int x, int y)
{
  if (y == 0) return 0;
  return x / y;
}

int add_with_message(const char* msg, int x, int y)
{
  printf("%s: %d + %d = %d\n", msg, x, y, add(x, y));
}

int sub_with_message(const char* msg, int x, int y)
{
  printf("%s: %d - %d = %d\n", msg, x, y, sub(x, y));
}

int mul_with_message(const char* msg, int x, int y)
{
  printf("%s: %d * %d = %d\n", msg, x, y, mul(x, y));
}

int div_with_message(const char* msg, int x, int y)
{
  printf("%s: %d / %d = %d\n", msg, x, y, divide(x, y));
}

typedef char*(stringFn)(char*, int);

char* apply_fn(char* s, int x, stringFn f)
{
  printf("Applying callback to %s and %d\n", s, x);
  return f(s, x);
}

typedef struct {
  int x;
  int y;
} point;

point* mk_point(int x, int y)
{
  point* p = malloc(sizeof(point));
  p->x = x;
  p->y = y;
  return p;
}

void free_point(point* p)
{
  free(p);
}
