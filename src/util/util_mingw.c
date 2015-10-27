/*  $Id: util_mingw.c 27004 2015-04-22 18:37:52Z edo $*/
/* stubs for functions missing in MinGW */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
void bzero(void *ptr, size_t n)
{
  memset(ptr, 0, n);
}
void nice(int n)
{
/* do nothing */
}
void bcopy(const void *src, void *dest, size_t n)
{
memcpy(dest, src, n);
}
int pause(void)
{
  fprintf(stderr, " pause() function not available \n");
  return -1;
}
