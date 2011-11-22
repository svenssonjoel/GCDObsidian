
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


//#define ALLTESTS 1 


/* SORTER KERNELS */
#include "bitonicKernel.cu"  // 512 threads 512 elements
#include "tsortKernel.cu"    // 256 threads 512 elements
#include "tsort1Kernel.cu"   // 512 threads 512 elements 
#include "vsortKernel.cu"    // 256 threads 512 elements
#include "vsort1Kernel.cu"   // 512 threads 512 elements

/* MERGER KERNELS */
#include "bmergeKernel.cu"



// -------------------------------------------------------------------------- 
// SORTERS

/*
#define SORTKERNEL  vsortSmall
#define SORTKERNELNAME "vsort"
#define SORTKERNELTHREADS 256
*/

/*
#define SORTKERNEL  bitonicSort
#define SORTKERNELNAME "bitonic"
#define SORTKERNELTHREADS 512
*/

/*
#define SORTKERNEL  vsort1
#define SORTKERNELNAME "vsort1"
#define SORTKERNELTHREADS 512
*/

/*
#define SORTKERNEL tsortSmall
#define SORTKERNELNAME "tsort2"
#define SORTKERNELTHREADS 256
*/



#define SORTKERNEL tsort1
#define SORTKERNELNAME "tsort1"
#define SORTKERNELTHREADS 512




// -------------------------------------------------------------------------- 
// Mergers


#define MERGEKERNEL bmergeSmall
#define MERGEKERNELNAME "bmerge"
#define MERGEKERNELTHREADS 256
 



// Threads used per block in the iswap kernels
#define SWAPTHREADS 512


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
    
    //unsigned int ix01 = ix00 + (stride >> 1);

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

void sort(int loglargesize,int *d_data)
{
   
  unsigned int arrayLength = 1 << loglargesize;
  unsigned int diff = loglargesize - LOG_SMALL_SIZE;
  unsigned int blocks = arrayLength / SMALL_SIZE;
 
  SORTKERNEL<<<blocks, SORTKERNELTHREADS,4096>>>(d_data, d_data);
  
  for(int i = 0 ; i < diff ; i += 1){ 
    vSwap<<<blocks/2,SWAPTHREADS,0>>>(d_data, d_data,(1<<i)*SMALL_SIZE);
     
    for(int j = i-1; j >= 0; j -= 3){ 
      
      if (j==0) {
	iSwap<<<blocks/2,SWAPTHREADS,0>>>(d_data, d_data,(1<<j)*SMALL_SIZE);
      }
      else {if (j==1) {
           
	  iSwap2<<<blocks/4,SWAPTHREADS,0>>>(d_data, d_data,(1<<j)*SMALL_SIZE);}
           
	else {iSwap3<<<blocks/8,SWAPTHREADS,0>>>(d_data, d_data,(1<<j)*SMALL_SIZE);}}}

   
    MERGEKERNEL<<<blocks,MERGEKERNELTHREADS,4096>>>(d_data, d_data);
   
  }
}


/* comparator for cpusort ------------------------------------------------------ */

int cmp(const void *a,const void * b)
{
  return(*(int *)a - *(int *)b );
}

void performWithMemTrans(int loglargesize, int *inputData) { 
  
  int largesize = 1 << loglargesize;
  int *dvalues;
  int *result; 
  int *cpusort;

  /*timing*/
  timeval gpustart,gpustop;
  timeval cpustart,cpustop;


  
  result = (int*)malloc(largesize*sizeof(int));
  cpusort = (int*)malloc(largesize*sizeof(int));
  
  /*duplicate input array for cpusorting*/
  memcpy(cpusort,inputData,sizeof(int)*largesize);
 
  /* Allocate GPU arrays */   
  cudaMalloc((void**)&dvalues, sizeof(int) * largesize ); 
  
  /*start measuring time */
  gettimeofday(&gpustart,NULL);
  
  cudaMemcpy(dvalues, inputData, sizeof(int) * largesize, cudaMemcpyHostToDevice);

  /* Launch sorting algorithm on gpu */
  sort(loglargesize,dvalues); 

  cudaMemcpy(result, dvalues, sizeof(int) * largesize , cudaMemcpyDeviceToHost);

  /*stop measuring time*/
  gettimeofday(&gpustop,NULL);
  cudaFree(dvalues);
  
  /* Results ?*/
  int passed0 = 1;
  for (int i = 1; i < largesize; ++i) { 
    if (result[i] < result[i-1]) {
      passed0 = 0; 
    } 
  }

  gettimeofday(&cpustart,NULL);
  qsort(cpusort,largesize,sizeof(int),cmp);
  gettimeofday(&cpustop,NULL);

  //printf("%s first test.\n",passed ? "Passed" : "Failed");  
  int passed1 = 1;
  for (int i = 0; i < largesize; ++i) { 
    if (result[i] != cpusort[i]) {
      passed1 = 0; 
    } 
  }
  
  printf("ELEMENTS: %d\n",largesize);
  printf("SORTS: [%s] [%s]\n",passed0 ? "P" : "F", passed1 ? "P" : "F");
  printf("GPUTIME: %dms\n", diff_ms(&gpustart,&gpustop));
  printf("CPUTIME: %dms\n", diff_ms(&cpustart,&cpustop));
  
} 

void performWithoutMemTrans(int loglargesize, int *inputData) { 
  
  int largesize = 1 << loglargesize;
  int *dvalues;
  int *result; 
  int *cpusort;

  /*timing*/
  timeval gpustart,gpustop;
  timeval cpustart,cpustop;


  
  result = (int*)malloc(largesize*sizeof(int));
  cpusort = (int*)malloc(largesize*sizeof(int));
  
  /*duplicate input array for cpusorting*/
  memcpy(cpusort,inputData,sizeof(int)*largesize);
 
  /* Allocate GPU arrays */   
  cudaMalloc((void**)&dvalues, sizeof(int) * largesize ); 
  
  
  cudaMemcpy(dvalues, inputData, sizeof(int) * largesize, cudaMemcpyHostToDevice);

  /*start measuring time */
  gettimeofday(&gpustart,NULL);


  /* Launch sorting algorithm on gpu */
  sort(loglargesize,dvalues); 

  /*stop measuring time*/
  cudaThreadSynchronize();
  gettimeofday(&gpustop,NULL);


  cudaMemcpy(result, dvalues, sizeof(int) * largesize , cudaMemcpyDeviceToHost);

  cudaFree(dvalues);
  
  /* Results ?*/
  int passed0 = 1;
  for (int i = 1; i < largesize; ++i) { 
    if (result[i] < result[i-1]) {
      passed0 = 0; 
    } 
  }

  gettimeofday(&cpustart,NULL);
  qsort(cpusort,largesize,sizeof(int),cmp);
  gettimeofday(&cpustop,NULL);

  //printf("%s first test.\n",passed ? "Passed" : "Failed");  
  int passed1 = 1;
  for (int i = 0; i < largesize; ++i) { 
    if (result[i] != cpusort[i]) {
      passed1 = 0; 
    } 
  }
  
  printf("ELEMENTS: %d\n",largesize);
  printf("SORTS: [%s] [%s]\n",passed0 ? "P" : "F", passed1 ? "P" : "F");
  printf("GPUTIME: %dms\n", diff_ms(&gpustart,&gpustop));
  printf("CPUTIME: %dms\n", diff_ms(&cpustart,&cpustop));
  
} 




int main(int argc, char *argv[]){ 
  
  int *values;

  values = (int*)malloc(LARGE_SIZE*sizeof(int));
  
  for (int i = 0; i < LARGE_SIZE; ++i) { 
    values[i] = rand ();
  }

  printf("SORTING KERNEL: %s\n",SORTKERNELNAME);
  printf("MERGER KERNEL: %s\n",MERGEKERNELNAME);
  printf("----------------------------------------\n\n");
  /*
  printf("Performing tests\n");
  printf("Memory copying overhead included in results\n");
#ifdef ALLTESTS
  printf("----------------------------------------\n");
  performWithMemTrans(20,values);
  printf("----------------------------------------\n");
  performWithMemTrans(21,values);
  printf("----------------------------------------\n");
  performWithMemTrans(22,values);
  printf("----------------------------------------\n");
  performWithMemTrans(23,values);
#endif
  printf("----------------------------------------\n");
  performWithMemTrans(24,values);

  printf("----------------------------------------\n\n");

  */
  printf("Performing tests\n");
  printf("Memory copying overhead *NOT* included in results\n");
#ifdef ALLTESTS
  printf("----------------------------------------\n");
  performWithoutMemTrans(20,values);
  printf("----------------------------------------\n");
  performWithoutMemTrans(21,values);
  printf("----------------------------------------\n");
  performWithoutMemTrans(22,values);
  printf("----------------------------------------\n");
  performWithoutMemTrans(23,values);
#endif
  printf("----------------------------------------\n");
  performWithoutMemTrans(24,values);
  printf("----------------------------------------\n");  



  return 0;
}
