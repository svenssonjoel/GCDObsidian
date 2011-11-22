
#include <stdio.h>
#include <stdlib.h>


#include <assert.h>

#include <sys/time.h>
#include <time.h>
#include "diff_ms.h"


// #define TIME_MEM_TRANS 1

/* 2048 20*/
/* 4096 21*/
/* 8192 22*/
/* 16384 23*/
/* 32768 24*/


#define BLOCKS 32768
#define LARGE_SIZE BLOCKS*SMALL_SIZE
#define SMALL_SIZE 512
#define THREADS 256
#define LOG_SMALL_SIZE 9
#define LOG_LARGE_SIZE 24

//* kernel for sorting small fixed size arrays all on the GPU

#define NUM    512

__device__ inline void swap(int & a, int & b)
{
	// Alternative swap doesn't use a temporary register:
	// a ^= b;
	// b ^= a;
	// a ^= b;
	
    int tmp = a;
    a = b;
    b = tmp;
}

__global__ static void bitonicSort(int * values, int *results)
{
    extern __shared__ int shared[];

    const unsigned int tid = threadIdx.x;
    const unsigned int bid = blockIdx.x;

    // Copy input to shared mem.
    shared[tid] = values[(bid*NUM) + tid];

    __syncthreads();

    // Parallel bitonic sort.
    for (unsigned int k = 2; k <= NUM; k *= 2)
    {
        // Bitonic merge:
        for (unsigned int j = k / 2; j>0; j /= 2)
        {
            unsigned int ixj = tid ^ j;
            
            if (ixj > tid)
            {
                if ((tid & k) == 0)
                {
                    if (shared[tid] > shared[ixj])
                    {
                        swap(shared[tid], shared[ixj]);
                    }
                }
                else
                {
                    if (shared[tid] < shared[ixj])
                    {
                        swap(shared[tid], shared[ixj]);
                    }
                }
            }
            
            __syncthreads();
        }
    }

    // Write result.
    results[(bid*NUM) + tid] = shared[tid];
}
/* --------------------------------------------------------------------------
   tsortSmall 
   -------------------------------------------------------------------------- */
 
__global__ void tsortSmall(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__  unsigned char sbase[];
  (( int *)sbase)[(tid<<1)] = min(input0[((bid*512)+(tid<<1))],input0[((bid*512)+((tid<<1)^1))]);
  (( int *)sbase)[((tid<<1)^1)] = max(input0[((bid*512)+(tid<<1))],input0[((bid*512)+((tid<<1)^1))]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967294))] = min((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^3)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967294))^3)] = max((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^3)]);
  __syncthreads();
  (( int *)sbase)[(tid<<1)] = min((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  (( int *)sbase)[((tid<<1)^1)] = max((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967292))] = min((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^7)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967292))^7)] = max((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^7)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967294))] = min((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  (( int *)sbase)[((tid+(tid&4294967294))^2)] = max((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid<<1)] = min((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  (( int *)(sbase + 2048))[((tid<<1)^1)] = max((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967288))] = min((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^15)]);
  (( int *)sbase)[((tid+(tid&4294967288))^15)] = max((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^15)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967292))] = min((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967292))^4)] = max((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967294))] = min((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  (( int *)sbase)[((tid+(tid&4294967294))^2)] = max((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid<<1)] = min((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  (( int *)(sbase + 2048))[((tid<<1)^1)] = max((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967280))] = min((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^31)]);
  (( int *)sbase)[((tid+(tid&4294967280))^31)] = max((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^31)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967288))] = min((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^8)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967288))^8)] = max((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^8)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967292))] = min((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^4)]);
  (( int *)sbase)[((tid+(tid&4294967292))^4)] = max((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^4)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967294))] = min((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^2)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967294))^2)] = max((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)sbase)[(tid<<1)] = min((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  (( int *)sbase)[((tid<<1)^1)] = max((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967264))] = min((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^63)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967264))^63)] = max((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^63)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967280))] = min((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^16)]);
  (( int *)sbase)[((tid+(tid&4294967280))^16)] = max((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^16)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967288))] = min((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^8)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967288))^8)] = max((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^8)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967292))] = min((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^4)]);
  (( int *)sbase)[((tid+(tid&4294967292))^4)] = max((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^4)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967294))] = min((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^2)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967294))^2)] = max((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)sbase)[(tid<<1)] = min((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  (( int *)sbase)[((tid<<1)^1)] = max((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967232))] = min((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^127)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967232))^127)] = max((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^127)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967264))] = min((( int *)(sbase+2048))[(tid+(tid&4294967264))],(( int *)(sbase+2048))[((tid+(tid&4294967264))^32)]);
  (( int *)sbase)[((tid+(tid&4294967264))^32)] = max((( int *)(sbase+2048))[(tid+(tid&4294967264))],(( int *)(sbase+2048))[((tid+(tid&4294967264))^32)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967280))] = min((( int *)sbase)[(tid+(tid&4294967280))],(( int *)sbase)[((tid+(tid&4294967280))^16)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967280))^16)] = max((( int *)sbase)[(tid+(tid&4294967280))],(( int *)sbase)[((tid+(tid&4294967280))^16)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967288))] = min((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^8)]);
  (( int *)sbase)[((tid+(tid&4294967288))^8)] = max((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^8)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967292))] = min((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967292))^4)] = max((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967294))] = min((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  (( int *)sbase)[((tid+(tid&4294967294))^2)] = max((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid<<1)] = min((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  (( int *)(sbase + 2048))[((tid<<1)^1)] = max((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967168))] = min((( int *)(sbase+2048))[(tid+(tid&4294967168))],(( int *)(sbase+2048))[((tid+(tid&4294967168))^255)]);
  (( int *)sbase)[((tid+(tid&4294967168))^255)] = max((( int *)(sbase+2048))[(tid+(tid&4294967168))],(( int *)(sbase+2048))[((tid+(tid&4294967168))^255)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967232))] = min((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^64)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967232))^64)] = max((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^64)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967264))] = min((( int *)(sbase+2048))[(tid+(tid&4294967264))],(( int *)(sbase+2048))[((tid+(tid&4294967264))^32)]);
  (( int *)sbase)[((tid+(tid&4294967264))^32)] = max((( int *)(sbase+2048))[(tid+(tid&4294967264))],(( int *)(sbase+2048))[((tid+(tid&4294967264))^32)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967280))] = min((( int *)sbase)[(tid+(tid&4294967280))],(( int *)sbase)[((tid+(tid&4294967280))^16)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967280))^16)] = max((( int *)sbase)[(tid+(tid&4294967280))],(( int *)sbase)[((tid+(tid&4294967280))^16)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967288))] = min((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^8)]);
  (( int *)sbase)[((tid+(tid&4294967288))^8)] = max((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^8)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967292))] = min((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967292))^4)] = max((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967294))] = min((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  (( int *)sbase)[((tid+(tid&4294967294))^2)] = max((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid<<1)] = min((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  (( int *)(sbase + 2048))[((tid<<1)^1)] = max((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967040))] = min((( int *)(sbase+2048))[(tid+(tid&4294967040))],(( int *)(sbase+2048))[((tid+(tid&4294967040))^511)]);
  (( int *)sbase)[((tid+(tid&4294967040))^511)] = max((( int *)(sbase+2048))[(tid+(tid&4294967040))],(( int *)(sbase+2048))[((tid+(tid&4294967040))^511)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967168))] = min((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^128)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967168))^128)] = max((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^128)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967232))] = min((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^64)]);
  (( int *)sbase)[((tid+(tid&4294967232))^64)] = max((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^64)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967264))] = min((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^32)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967264))^32)] = max((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^32)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967280))] = min((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^16)]);
  (( int *)sbase)[((tid+(tid&4294967280))^16)] = max((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^16)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967288))] = min((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^8)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967288))^8)] = max((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^8)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967292))] = min((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^4)]);
  (( int *)sbase)[((tid+(tid&4294967292))^4)] = max((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^4)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967294))] = min((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^2)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967294))^2)] = max((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)sbase)[(tid<<1)] = min((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  (( int *)sbase)[((tid<<1)^1)] = max((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  __syncthreads();
  result0[((bid*512)+tid)] = (( int *)sbase)[tid];
  result0[((bid*512)+(tid+256))] = (( int *)sbase)[(tid+256)];
  
}


/* --------------------------------------------------------------------------
   tsortSmall 
   -------------------------------------------------------------------------- */


__global__ void vsortSmall(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__  unsigned char sbase[];
  (( int *)sbase)[(tid+(tid&4294967040))] = min(input0[((bid*512)+(tid+(tid&4294967040)))],input0[((bid*512)+((tid+(tid&4294967040))^256))]);
  (( int *)sbase)[((tid+(tid&4294967040))^256)] = max(input0[((bid*512)+(tid+(tid&4294967040)))],input0[((bid*512)+((tid+(tid&4294967040))^256))]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967040))] = min((( int *)sbase)[(tid+(tid&4294967040))],(( int *)sbase)[((tid+(tid&4294967040))^384)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967040))^384)] = max((( int *)sbase)[(tid+(tid&4294967040))],(( int *)sbase)[((tid+(tid&4294967040))^384)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967168))] = min((( int *)(sbase+2048))[(tid+(tid&4294967168))],(( int *)(sbase+2048))[((tid+(tid&4294967168))^128)]);
  (( int *)sbase)[((tid+(tid&4294967168))^128)] = max((( int *)(sbase+2048))[(tid+(tid&4294967168))],(( int *)(sbase+2048))[((tid+(tid&4294967168))^128)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967040))] = min((( int *)sbase)[(tid+(tid&4294967040))],(( int *)sbase)[((tid+(tid&4294967040))^448)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967040))^448)] = max((( int *)sbase)[(tid+(tid&4294967040))],(( int *)sbase)[((tid+(tid&4294967040))^448)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967168))] = min((( int *)(sbase+2048))[(tid+(tid&4294967168))],(( int *)(sbase+2048))[((tid+(tid&4294967168))^192)]);
  (( int *)sbase)[((tid+(tid&4294967168))^192)] = max((( int *)(sbase+2048))[(tid+(tid&4294967168))],(( int *)(sbase+2048))[((tid+(tid&4294967168))^192)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967232))] = min((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^64)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967232))^64)] = max((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^64)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967040))] = min((( int *)(sbase+2048))[(tid+(tid&4294967040))],(( int *)(sbase+2048))[((tid+(tid&4294967040))^480)]);
  (( int *)sbase)[((tid+(tid&4294967040))^480)] = max((( int *)(sbase+2048))[(tid+(tid&4294967040))],(( int *)(sbase+2048))[((tid+(tid&4294967040))^480)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967168))] = min((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^224)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967168))^224)] = max((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^224)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967232))] = min((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^96)]);
  (( int *)sbase)[((tid+(tid&4294967232))^96)] = max((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^96)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967264))] = min((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^32)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967264))^32)] = max((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^32)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967040))] = min((( int *)(sbase+2048))[(tid+(tid&4294967040))],(( int *)(sbase+2048))[((tid+(tid&4294967040))^496)]);
  (( int *)sbase)[((tid+(tid&4294967040))^496)] = max((( int *)(sbase+2048))[(tid+(tid&4294967040))],(( int *)(sbase+2048))[((tid+(tid&4294967040))^496)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967168))] = min((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^240)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967168))^240)] = max((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^240)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967232))] = min((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^112)]);
  (( int *)sbase)[((tid+(tid&4294967232))^112)] = max((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^112)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967264))] = min((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^48)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967264))^48)] = max((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^48)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967280))] = min((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^16)]);
  (( int *)sbase)[((tid+(tid&4294967280))^16)] = max((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^16)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967040))] = min((( int *)sbase)[(tid+(tid&4294967040))],(( int *)sbase)[((tid+(tid&4294967040))^504)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967040))^504)] = max((( int *)sbase)[(tid+(tid&4294967040))],(( int *)sbase)[((tid+(tid&4294967040))^504)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967168))] = min((( int *)(sbase+2048))[(tid+(tid&4294967168))],(( int *)(sbase+2048))[((tid+(tid&4294967168))^248)]);
  (( int *)sbase)[((tid+(tid&4294967168))^248)] = max((( int *)(sbase+2048))[(tid+(tid&4294967168))],(( int *)(sbase+2048))[((tid+(tid&4294967168))^248)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967232))] = min((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^120)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967232))^120)] = max((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^120)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967264))] = min((( int *)(sbase+2048))[(tid+(tid&4294967264))],(( int *)(sbase+2048))[((tid+(tid&4294967264))^56)]);
  (( int *)sbase)[((tid+(tid&4294967264))^56)] = max((( int *)(sbase+2048))[(tid+(tid&4294967264))],(( int *)(sbase+2048))[((tid+(tid&4294967264))^56)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967280))] = min((( int *)sbase)[(tid+(tid&4294967280))],(( int *)sbase)[((tid+(tid&4294967280))^24)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967280))^24)] = max((( int *)sbase)[(tid+(tid&4294967280))],(( int *)sbase)[((tid+(tid&4294967280))^24)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967288))] = min((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^8)]);
  (( int *)sbase)[((tid+(tid&4294967288))^8)] = max((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^8)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967040))] = min((( int *)sbase)[(tid+(tid&4294967040))],(( int *)sbase)[((tid+(tid&4294967040))^508)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967040))^508)] = max((( int *)sbase)[(tid+(tid&4294967040))],(( int *)sbase)[((tid+(tid&4294967040))^508)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967168))] = min((( int *)(sbase+2048))[(tid+(tid&4294967168))],(( int *)(sbase+2048))[((tid+(tid&4294967168))^252)]);
  (( int *)sbase)[((tid+(tid&4294967168))^252)] = max((( int *)(sbase+2048))[(tid+(tid&4294967168))],(( int *)(sbase+2048))[((tid+(tid&4294967168))^252)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967232))] = min((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^124)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967232))^124)] = max((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^124)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967264))] = min((( int *)(sbase+2048))[(tid+(tid&4294967264))],(( int *)(sbase+2048))[((tid+(tid&4294967264))^60)]);
  (( int *)sbase)[((tid+(tid&4294967264))^60)] = max((( int *)(sbase+2048))[(tid+(tid&4294967264))],(( int *)(sbase+2048))[((tid+(tid&4294967264))^60)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967280))] = min((( int *)sbase)[(tid+(tid&4294967280))],(( int *)sbase)[((tid+(tid&4294967280))^28)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967280))^28)] = max((( int *)sbase)[(tid+(tid&4294967280))],(( int *)sbase)[((tid+(tid&4294967280))^28)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967288))] = min((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^12)]);
  (( int *)sbase)[((tid+(tid&4294967288))^12)] = max((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^12)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967292))] = min((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967292))^4)] = max((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967040))] = min((( int *)(sbase+2048))[(tid+(tid&4294967040))],(( int *)(sbase+2048))[((tid+(tid&4294967040))^510)]);
  (( int *)sbase)[((tid+(tid&4294967040))^510)] = max((( int *)(sbase+2048))[(tid+(tid&4294967040))],(( int *)(sbase+2048))[((tid+(tid&4294967040))^510)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967168))] = min((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^254)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967168))^254)] = max((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^254)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967232))] = min((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^126)]);
  (( int *)sbase)[((tid+(tid&4294967232))^126)] = max((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^126)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967264))] = min((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^62)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967264))^62)] = max((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^62)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967280))] = min((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^30)]);
  (( int *)sbase)[((tid+(tid&4294967280))^30)] = max((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^30)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967288))] = min((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^14)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967288))^14)] = max((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^14)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967292))] = min((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^6)]);
  (( int *)sbase)[((tid+(tid&4294967292))^6)] = max((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^6)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967294))] = min((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^2)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967294))^2)] = max((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967040))] = min((( int *)(sbase+2048))[(tid+(tid&4294967040))],(( int *)(sbase+2048))[((tid+(tid&4294967040))^511)]);
  (( int *)sbase)[((tid+(tid&4294967040))^511)] = max((( int *)(sbase+2048))[(tid+(tid&4294967040))],(( int *)(sbase+2048))[((tid+(tid&4294967040))^511)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967168))] = min((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^255)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967168))^255)] = max((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^255)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967232))] = min((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^127)]);
  (( int *)sbase)[((tid+(tid&4294967232))^127)] = max((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^127)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967264))] = min((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^63)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967264))^63)] = max((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^63)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967280))] = min((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^31)]);
  (( int *)sbase)[((tid+(tid&4294967280))^31)] = max((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^31)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967288))] = min((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^15)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967288))^15)] = max((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^15)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967292))] = min((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^7)]);
  (( int *)sbase)[((tid+(tid&4294967292))^7)] = max((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^7)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967294))] = min((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^3)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967294))^3)] = max((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^3)]);
  __syncthreads();
  (( int *)sbase)[(tid<<1)] = min((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  (( int *)sbase)[((tid<<1)^1)] = max((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  __syncthreads();
  result0[((bid*512)+tid)] = (( int *)sbase)[tid];
  result0[((bid*512)+(tid+256))] = (( int *)sbase)[(tid+256)];
  
}




//* kernel for merging small fixed size arrays all on the GPU
//* bitonic merge (on 512 elements), generated by Obsidian
__global__ void bmergeSmall(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__  unsigned char sbase[];
  (( int *)sbase)[(tid+(tid&4294967040))] = min(input0[((bid*512)+(tid+(tid&4294967040)))],input0[((bid*512)+((tid+(tid&4294967040))^256))]);
  (( int *)sbase)[((tid+(tid&4294967040))^256)] = max(input0[((bid*512)+(tid+(tid&4294967040)))],input0[((bid*512)+((tid+(tid&4294967040))^256))]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967168))] = min((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^128)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967168))^128)] = max((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^128)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967232))] = min((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^64)]);
  (( int *)sbase)[((tid+(tid&4294967232))^64)] = max((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^64)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967264))] = min((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^32)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967264))^32)] = max((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^32)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967280))] = min((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^16)]);
  (( int *)sbase)[((tid+(tid&4294967280))^16)] = max((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^16)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967288))] = min((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^8)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967288))^8)] = max((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^8)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967292))] = min((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^4)]);
  (( int *)sbase)[((tid+(tid&4294967292))^4)] = max((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^4)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967294))] = min((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^2)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967294))^2)] = max((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)sbase)[(tid<<1)] = min((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  (( int *)sbase)[((tid<<1)^1)] = max((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  __syncthreads();
  result0[((bid*512)+tid)] = (( int *)sbase)[tid];
  result0[((bid*512)+(tid+256))] = (( int *)sbase)[(tid+256)];
  
}


//* Compare and swap elements in a large array with a stride of 2^k 
//* stride > SMALL_SIZE and < array length (all a power of two)



__global__ void iSwap(
    int *d_input,
    int *d_output,
    unsigned int stride){

    unsigned int tid = blockIdx.x * blockDim.x + threadIdx.x;
   
    unsigned int ix = tid + (tid & ~(stride - 1));

    int v1 = d_input[ix];
    int v2 = d_input[ix + stride];

    d_output[ix] = min(v1,v2);
    d_output[ix + stride] = max(v1,v2);
    
}

__global__ void iSwap2(
    int *d_input,
    int *d_output,
    unsigned int stride){

    unsigned int tid = blockIdx.x * blockDim.x + threadIdx.x;
   
    unsigned int ix00 = tid + 3*(tid & ~((stride>>1) - 1));
    unsigned int ix01 = ix00 + (stride >> 1);

    int v1 = d_input[ix00];
    int v2 = d_input[ix01];
    int v3 = d_input[ix00 + stride];
    int v4 = d_input[ix01 + stride];

    int t1 = min(v1,v3);
    int t2 = max(v1,v3);
    int t3 = min(v2,v4);
    int t4 = max(v2,v4);
    d_output[ix00] = min(t1,t3);
    d_output[ix01] = max(t1,t3);
    d_output[ix00 + stride] = min(t2,t4);
    d_output[ix01 + stride] = max(t2,t4);
    
    
}

__global__ void iSwap3(
    int *d_input,
    int *d_output,
    unsigned int stride){

    unsigned int tid = blockIdx.x * blockDim.x + threadIdx.x;
    unsigned int s4 = stride >> 2;
    unsigned int ix00 = tid + 7*(tid & ~(s4 - 1));
    
    unsigned int ix01 = ix00 + (stride >> 1);

    int v1 = d_input[ix00];
    int v2 = d_input[ix00 + s4];
    int v3 = d_input[ix00 + 2*s4];
    int v4 = d_input[ix00 + 3*s4];
    int v5 = d_input[ix00 + stride];
    int v6 = d_input[ix00 + 5*s4];
    int v7 = d_input[ix00 + 6*s4];
    int v8 = d_input[ix00 + 7*s4];

    int t1 = min(v1,v5);
    int t2 = max(v1,v5);
    int t3 = min(v2,v6);
    int t4 = max(v2,v6);
    int t5 = min(v3,v7);
    int t6 = max(v3,v7);
    int t7 = min(v4,v8);
    int t8 = max(v4,v8);

    v1 = min(t1,t5);
    v2 = max(t1,t5);
    v3 = min(t3,t7);
    v4 = max(t3,t7);
    v5 = min(t2,t6);
    v6 = max(t2,t6);
    v7 = min(t4,t8);
    v8 = max(t4,t8);

    d_output[ix00] = min(v1,v3);
    d_output[ix00 + s4] = max(v1,v3);
    d_output[ix00 + 2*s4] = min(v2,v4);
    d_output[ix00 + 3*s4] = max(v2,v4);
    d_output[ix00 + stride] = min(v5,v7);
    d_output[ix00 + 5*s4] = max(v5,v7);
    d_output[ix00 + 6*s4] = min(v6,v8);
    d_output[ix00 + 7*s4] = max(v6,v8);

    
    
    
}

//* Compare and swap elements in a large array, as a series of
//* adjacent "vee" shaped patterns

__global__ void vSwap(
    int *d_input,
    int *d_output,
    unsigned int stride){

    unsigned int tid = blockIdx.x * blockDim.x + threadIdx.x;
   
    unsigned int ix = tid + (tid & ~(stride - 1));

    unsigned int ix2 = ix^((stride<<1)-1);
    
    int v1 = d_input[ix];
    int v2 = d_input[ix2];

    d_output[ix] = min(v1,v2);
    d_output[ix2] = max(v1,v2);
    
}


//* kernel for sorting large arrays, demanding repeated use of kernels
//* that sort or merge small fixed size arrays


//* Assume input and output arrays have length 2^lenLog 
//* Small kernels work on SMALL_SIZE inputs. Assume lenLog >= LOG_SMALL_SIZE > 0
void sort(int *d_data)
{
   
  unsigned int arrayLength = 1 << LOG_LARGE_SIZE;
  unsigned int diff = LOG_LARGE_SIZE - LOG_SMALL_SIZE;
  unsigned int blocks = arrayLength / SMALL_SIZE;
  unsigned int threads = SMALL_SIZE / 2;
  
  //bitonicSort<<<blocks,threads*2,2048>>>(d_data, d_data);
  //tsortSmall<<<blocks, threads,4096>>>(d_data, d_data);
  vsortSmall<<<blocks, threads,4096>>>(d_data, d_data);
 
  
  for(int i = 0 ; i < diff ; i += 1){ 
    vSwap<<<blocks/2,threads*2,0>>>(d_data, d_data,(1<<i)*SMALL_SIZE);
     
    for(int j = i-1; j >= 0; j -= 3){ 
      
       if (j==0) {
           iSwap<<<blocks/2,threads*2,0>>>(d_data, d_data,(1<<j)*SMALL_SIZE);
           }
       else {if (j==1) {
           
           iSwap2<<<blocks/4,threads*2,0>>>(d_data, d_data,(1<<j)*SMALL_SIZE);}
           
       else {iSwap3<<<blocks/8,threads*2,0>>>(d_data, d_data,(1<<j)*SMALL_SIZE);}}}

   
    bmergeSmall<<<blocks,threads,4096>>>(d_data, d_data);
   
     }
   }


/* comparator for cpusort ------------------------------------------------------ */

int cmp(const void *a,const void * b)
{
  return(*(int *)a - *(int *)b );
}



int main(int argc, char *argv[]){ 
  
  int *values;
  int *result; 

  int *dvalues;

  /*timing*/
  timeval gpustart,gpustop;
  timeval cpustart,cpustop;
  

  values = (int*)malloc(LARGE_SIZE*sizeof(int));
  result = (int*)malloc(LARGE_SIZE*sizeof(int));
  
  for (int i = 0; i < LARGE_SIZE; ++i) { 
    values[i] = rand ();
  }
  



  /* Allocate GPU arrays */   
  cudaMalloc((void**)&dvalues, sizeof(int) * LARGE_SIZE ); 

#ifdef TIME_MEM_TRANS
  gettimeofday(&gpustart,NULL);
#endif

  cudaMemcpy(dvalues, values, sizeof(int) * LARGE_SIZE, cudaMemcpyHostToDevice);

#ifndef TIME_MEM_TRANS
  gettimeofday(&gpustart,NULL);
#endif

  /* Launch sorting algorithm on gpu */
  sort(dvalues); 

#ifndef TIME_MEM_TRANS
  cudaThreadSynchronize();
  gettimeofday(&gpustop,NULL);
#endif
  cudaMemcpy(result, dvalues, sizeof(int) * LARGE_SIZE , cudaMemcpyDeviceToHost);

#ifdef TIME_MEM_TRANS
  gettimeofday(&gpustop,NULL);
#endif
  

  
  cudaFree(dvalues);
 
  /* Results ?*/
  int passed = 1;
  for (int i = 1; i < LARGE_SIZE; ++i) { 
    if (result[i] < result[i-1]) {
      // printf("[%d](%d, %d) ",i, result[i], result[i-1]); 
      passed = 0; 
    } 
  }
  printf("%s first test.\n",passed ? "Passed" : "Failed");  

  gettimeofday(&cpustart,NULL);
  qsort(values,LARGE_SIZE,sizeof(int),cmp);
  gettimeofday(&cpustop,NULL);


  for (int i = 0; i < LARGE_SIZE; ++i) { 
    if (result[i] != values[i]) {
      //printf("[%d](%d, %d) ",i, result[i], values[i]); 
      passed = 0; 
    } 
  }
  
  

  
  printf("%s second test.\n",passed ? "Passed" : "Failed");

#ifdef TIME_MEM_TRANS 
  printf("GPUTIME includes memory transfer time \n");
#endif 
  printf("GPUTIME %dms\n", diff_ms(&gpustart,&gpustop));
  printf("CPUTIME %dms\n", diff_ms(&cpustart,&cpustop));

  return 0;
}
