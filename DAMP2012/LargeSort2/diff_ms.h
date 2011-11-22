
#include <sys/time.h>
#include <time.h>


// struct timeval t_start,t_stop;
// gettimeofday( &t_start, 0);

int diff_ms(struct timeval *t_1,struct timeval *t_2) 
{
  return  (float) (1000.0 * ( t_2->tv_sec - t_1->tv_sec) 
                + (0.001 * (t_2->tv_usec - t_1->tv_usec)) );
}
