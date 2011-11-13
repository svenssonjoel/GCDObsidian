#include <stdio.h>
#include <stdlib.h>

typedef unsigned int word;

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
  word values[32];
  word result[32];
  word * dvalues;
  word * dresult;
 

  //generate input data
  for (int i = 0; i < 32; ++i) { 
    values[i] = i; 
  }

  cudaMalloc((void**)&dvalues, sizeof(word) * 32 ); 
  cudaMalloc((void**)&dresult, sizeof(word) * 32 ); 
  cudaMemcpy(dvalues, values, sizeof(word) * 32, cudaMemcpyHostToDevice);
  two<<<1, 32,32* sizeof(unsigned int)>>>((int*)dvalues,(int*)dresult);
  cudaMemcpy(result, dresult, sizeof(word) * 32 , cudaMemcpyDeviceToHost);
  cudaFree(dvalues);
  cudaFree(dresult);
  
  // show results 
  for (int i = 0; i < 32; ++i) { 
    printf("%d ", ((int*)result)[i]);
  }

}
