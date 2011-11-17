#include <stdio.h>
#include <stdlib.h>

#define SIZE (1<<24)

int cmp(const void *a,const void * b)
{
  return(*(int *)a - *(int *)b );
}

int main(int argc, char* argv[])
{
  int *values; 

  values = (int*)malloc(SIZE*sizeof(int));
  
  for (int i = 0; i < SIZE; ++i) {
    values[i] = rand ();
  }
  
  
  qsort(values,SIZE,sizeof(int),cmp);



  for (int i = 0; i < 10 ;++i)
    printf("Number = %d\n",values[i]);
  return 0;
}
