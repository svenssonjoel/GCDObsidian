
#include <stdio.h>
#include <stdlib.h>


#include <assert.h>


#define BLOCKS 32768
//32768 24
//16384 23
//8196  22
//4096  21
//2048  20  
#define LARGE_SIZE BLOCKS*SMALL_SIZE
#define SMALL_SIZE 512
#define THREADS 256
#define LOG_SMALL_SIZE 9
#define LOG_LARGE_SIZE 24

//* kernel for sorting small fixed size arrays all on the GPU

//__global__ void tsortSmall(int *d_input,int *d_output)
//{

//}
__global__ void tsortSmall(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
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

//* Will be generated from Obsidian for SMALL_SIZE inputs



//* kernel for merging small fixed size arrays all on the GPU

//__global__ void tmergeSmall(int *d_input,int *d_output)
//{

  //}
__global__ void tmergeSmall(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
  (( int *)sbase)[(tid+(tid&4294967040))] = min(input0[((bid*512)+(tid+(tid&4294967040)))],input0[((bid*512)+((tid+(tid&4294967040))^511))]);
  (( int *)sbase)[((tid+(tid&4294967040))^511)] = max(input0[((bid*512)+(tid+(tid&4294967040)))],input0[((bid*512)+((tid+(tid&4294967040))^511))]);
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

//* Will be generated from Obsidian for SMALL_SIZE inputs



//* kernel for sorting large arrays, demanding repeated use of kernels
//* that sort or merge small fixed size arrays




//* Compare and swap elements in a large array with a stride of 2^k 
//* stride > SMALL_SIZE and < array length (all a power of two)

__global__ void cSwap(
    int *d_input,
    int *d_output,
    uint stride){

    uint tid = blockIdx.x * blockDim.x + threadIdx.x;
   
    uint ix = tid + (tid & ~(stride - 1));

    int v1 = d_input[ix];
    int v2 = d_input[ix + stride];
    
    d_output[ix] = min(v1,v2);
    d_output[ix + stride] = max(v1,v2);
  
}

//* Assume input and output arrays have length 2^lenLog 
//* Small kernels work on SMALL_SIZE inputs. Assume lenLog >= ssLog > 0
void sort(int *d_data)
{
   
  uint arrayLength = 1 << LOG_LARGE_SIZE;
  uint diff = LOG_LARGE_SIZE - LOG_SMALL_SIZE;
  uint blocks = arrayLength / SMALL_SIZE;
  uint threads = SMALL_SIZE / 2;
   
  tsortSmall<<<blocks, threads,4096>>>(d_data, d_data);
  // cudaThreadSynchronize();
  
  for(int i = 0 ; i <= diff ; i += 1){ /* I made it <= and now it sorts? */
  
    for(int j = i; j >= 0; j -= 1){ 
      cSwap<<<blocks/2,threads*2,0>>>(d_data, d_data,(1<<j)*SMALL_SIZE);
      
      //  cudaThreadSynchronize();
    }
                
    tmergeSmall<<<blocks,threads,4096>>>(d_data, d_data);
    // cudaThreadSynchronize();
  }
}


int main(int argc, char *argv[]){ 
  
  int *values;
  int *result; 

  int *dvalues;

  values = (int*)malloc(LARGE_SIZE*sizeof(int));
  result = (int*)malloc(LARGE_SIZE*sizeof(int));
  
  for (int i = 0; i < LARGE_SIZE; ++i) { 
    values[i] = rand ();//  % 512; 
  }
  
  /* Allocate GPU arrays */   
  cudaMalloc((void**)&dvalues, sizeof(int) * LARGE_SIZE ); 
  cudaMemcpy(dvalues, values, sizeof(int) * LARGE_SIZE, cudaMemcpyHostToDevice);
  sort(dvalues); 
  cudaMemcpy(result, dvalues, sizeof(int) * LARGE_SIZE , cudaMemcpyDeviceToHost);
  cudaFree(dvalues);
 
  /* Results ?*/
  int passed = 1;
  for (int i = 1; i < LARGE_SIZE; ++i) { 
    if (result[i] < result[i-1]) {
      printf("[%d](%d, %d) ",i, result[i], result[i-1]); 
      passed = 0; 
    } 
  }

  printf("\n%s",passed ? "Passed!" : "Failed!");

  return 0;
}
