#include <stdio.h>
#include <stdlib.h>

#include <sys/time.h>
#include <time.h>
#include "diff_ms.h"

#define ELEMS 512
#define THREADS 256

#define NUM_TEST_BLOCKS 32768 // 16384 //32768 // 65576
#define NUM_TEST_RUNS   1000

// Code Generated with GCDObsidian
__global__ void vsort(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
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
  (( int *)sbase)[(tid+(tid&4294967295))] = min((( int *)(sbase+2048))[(tid+(tid&4294967295))],(( int *)(sbase+2048))[((tid+(tid&4294967295))^1)]);
  (( int *)sbase)[((tid+(tid&4294967295))^1)] = max((( int *)(sbase+2048))[(tid+(tid&4294967295))],(( int *)(sbase+2048))[((tid+(tid&4294967295))^1)]);
  __syncthreads();
  result0[((bid*512)+(tid*2))] = (( int *)sbase)[(tid*2)];
  result0[((bid*512)+((tid*2)+1))] = (( int *)sbase)[((tid*2)+1)];
  
}

int main(int argc, char **argv){
  int *values;
  int *result;
  int *dvalues;
  int *dresult;


  timeval start;
  timeval stop; 
  

  values = (int*)malloc(NUM_TEST_BLOCKS * ELEMS * sizeof(int)); 
  result = (int*)malloc(NUM_TEST_BLOCKS * ELEMS * sizeof(int)); 

  //generate input data
  printf("generating %d input elements\n",NUM_TEST_BLOCKS * ELEMS);

  for (int i = 0; i < NUM_TEST_BLOCKS * ELEMS; ++i) { 
    values[i] = rand() % 512;
    result[i] = values[i];    // put same in result 
  }
  for (int i = 0; i < ELEMS; ++i) { 
    printf("%d ", values[i]);
  }
  printf("\n");
  

  
  cudaMalloc((void**)&dvalues, sizeof(int) * NUM_TEST_BLOCKS * ELEMS ); 
  cudaMalloc((void**)&dresult, sizeof(int) * NUM_TEST_BLOCKS * ELEMS ); 
  cudaMemcpy(dvalues, values, sizeof(int) * NUM_TEST_BLOCKS * ELEMS, cudaMemcpyHostToDevice);

  // start time 
  gettimeofday(&start,NULL);

  for (int i = 0; i < NUM_TEST_RUNS; ++i)  {     
    vsort<<<NUM_TEST_BLOCKS, THREADS,ELEMS*2* sizeof(int)>>>((int*)dvalues,(int*)dresult);
  } 
  cudaThreadSynchronize();
  // stop time 
  gettimeofday(&stop,NULL);

  cudaMemcpy(result, dresult, sizeof(int) * NUM_TEST_BLOCKS * ELEMS , cudaMemcpyDeviceToHost);
  cudaFree(dvalues);
  cudaFree(dresult);
  
  // show one subresult  results 
  for (int i = 0; i < ELEMS; ++i) { 
    printf("%d ", result[i]);
  }
  
  printf("\n");

  // Test all subresults for sortedness 
  for (int block = 0; block < NUM_TEST_BLOCKS; ++block){ 
    for (int i = 1; i < ELEMS; ++i) { 
      int a = block*512+i;
      int b = a-1;
      if (result[b] > result [a]) { 
	printf("error at %d, %d \nvalues[%d] = %d \nvalues[%d] = %d\n", 
	       b,a,b,result[b],a,result[a]);
	free(result);
	free(values);
	return -1;
      }
    }
  }
  
  printf("running %d blocks took %fms\n",NUM_TEST_BLOCKS, diff_ms(&start,&stop) / ((float)NUM_TEST_RUNS) );

  printf("PASSED!\n");
  free(result);
  free(values);
  return 0;
}
