#include <stdio.h>

extern int add(int, int);
extern int sub(int, int);
extern int mul(int, int);
extern int div(int, int);
extern int op_with_message(const char* msg, int (*)(int, int), int, int);

int main(int argc, char *argv[])
{
  int x, y;
  scanf("%d%d", &x, &y);

  op_with_message("sum", add, x, y);
  op_with_message("diff", sub, x, y);
  op_with_message("prod", mul, x, y);
  op_with_message("quot", div, x, y);

  return 0;
}
