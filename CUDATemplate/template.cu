#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>


// Code Generated with GCDObsidian
__global__ void two(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__ unsigned char sbase[];
  ((int *)sbase)[tid] = input0[((bid*32)+((tid&4294967280)|(15-(tid&15))))];
  __syncthreads();
  result0[((bid*32)+tid)] = ((int *)sbase)[tid];
  
}

int main(int argc, char **argv){
  int values[32];
  int result[32];
  int * dvalues;
  int * dresult;
 

  //generate input data
  for (int i = 0; i < 32; ++i) { 
    values[i] = i; 
  }

  cudaMalloc((void**)&dvalues, sizeof(int) * 32 ); 
  cudaMalloc((void**)&dresult, sizeof(int) * 32 ); 
  cudaMemcpy(dvalues, values, sizeof(int) * 32, cudaMemcpyHostToDevice);
  two<<<1, 32,32* sizeof(int)>>>((int*)dvalues,(int*)dresult);
  cudaMemcpy(result, dresult, sizeof(int) * 32 , cudaMemcpyDeviceToHost);
  cudaFree(dvalues);
  cudaFree(dresult);
  
  // show results 
  for (int i = 0; i < 32; ++i) { 
    printf("%d ", ((int*)result)[i]);
  }

}
