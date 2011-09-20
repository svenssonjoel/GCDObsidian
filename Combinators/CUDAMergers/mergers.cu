#include <stdio.h>
#include <stdlib.h>

typedef unsigned int word;

/* Merger 1 */
__global__ void mm1(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
  (( int *)sbase)[tid] = min(input0[((bid*32)+tid)],input0[((bid*32)+(tid+16))]);
  (( int *)sbase)[(tid+16)] = max(input0[((bid*32)+tid)],input0[((bid*32)+(tid+16))]);
  __syncthreads();
  (( int *)(sbase+128))[tid] = min((( int *)sbase)[((tid&4294967280)|(tid&7))],(( int *)sbase)[((tid&4294967280)|((tid&7)+8))]);
  (( int *)(sbase+128))[(tid+16)] = max((( int *)sbase)[((tid&4294967280)|(tid&7))],(( int *)sbase)[((tid&4294967280)|((tid&7)+8))]);
  __syncthreads();
  (( int *)sbase)[tid] = min((( int *)(sbase+128))[((tid&4294967288)|(tid&3))],(( int *)(sbase+128))[((tid&4294967288)|((tid&3)+4))]);
  (( int *)sbase)[(tid+16)] = max((( int *)(sbase+128))[((tid&4294967288)|(tid&3))],(( int *)(sbase+128))[((tid&4294967288)|((tid&3)+4))]);
  __syncthreads();
  (( int *)(sbase+128))[tid] = min((( int *)sbase)[((tid&4294967292)|(tid&1))],(( int *)sbase)[((tid&4294967292)|((tid&1)+2))]);
  (( int *)(sbase+128))[(tid+16)] = max((( int *)sbase)[((tid&4294967292)|(tid&1))],(( int *)sbase)[((tid&4294967292)|((tid&1)+2))]);
  __syncthreads();
  (( int *)sbase)[tid] = min((( int *)(sbase+128))[((tid&4294967294)|(tid&0))],(( int *)(sbase+128))[((tid&4294967294)|((tid&0)+1))]);
  (( int *)sbase)[(tid+16)] = max((( int *)(sbase+128))[((tid&4294967294)|(tid&0))],(( int *)(sbase+128))[((tid&4294967294)|((tid&0)+1))]);
  __syncthreads();
  result0[((bid*32)+(tid*2))] = (( int *)sbase)[(tid*2)];
  result0[((bid*32)+((tid*2)+1))] = (( int *)sbase)[((tid*2)+1)];
  
}

/* Merger 2 */

__global__ void mm2(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
  (( int *)sbase)[tid] = ((tid&31)<16) ? min(input0[((bid*32)+((tid&0)|(tid&31)))],input0[((bid*32)+((tid&0)|((tid&31)+16)))]) : max(input0[((bid*32)+((tid&0)|((tid&31)-16)))],input0[((bid*32)+((tid&0)|(((tid&31)-16)+16)))]);
  __syncthreads();
  (( int *)(sbase+128))[tid] = ((tid&15)<8) ? min((( int *)sbase)[((tid&16)|(tid&15))],(( int *)sbase)[((tid&16)|((tid&15)+8))]) : max((( int *)sbase)[((tid&16)|((tid&15)-8))],(( int *)sbase)[((tid&16)|(((tid&15)-8)+8))]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&7)<4) ? min((( int *)(sbase+128))[((tid&24)|(tid&7))],(( int *)(sbase+128))[((tid&24)|((tid&7)+4))]) : max((( int *)(sbase+128))[((tid&24)|((tid&7)-4))],(( int *)(sbase+128))[((tid&24)|(((tid&7)-4)+4))]);
  __syncthreads();
  (( int *)(sbase+128))[tid] = ((tid&3)<2) ? min((( int *)sbase)[((tid&28)|(tid&3))],(( int *)sbase)[((tid&28)|((tid&3)+2))]) : max((( int *)sbase)[((tid&28)|((tid&3)-2))],(( int *)sbase)[((tid&28)|(((tid&3)-2)+2))]);
  __syncthreads();
  result0[((bid*32)+tid)] = ((tid&1)<1) ? min((( int *)(sbase+128))[((tid&30)|(tid&1))],(( int *)(sbase+128))[((tid&30)|((tid&1)+1))]) : max((( int *)(sbase+128))[((tid&30)|((tid&1)-1))],(( int *)(sbase+128))[((tid&30)|(((tid&1)-1)+1))]);
  
}


int main(int argc, char **argv){
  word values[32];
  word result[32];
  word * dvalues;
  word * dresult;
 

  //generate input data
  for (int i = 0; i < 16; ++i) { 
    values[i] = i; 
  }

  for (int i = 16; i < 32; ++i) { 
    values[i] = 31 - (i-16); 
  }
  for (int i = 0; i < 32; ++i)  {
    printf("%d ", values[i]);
  }
  printf("\n ------------ \n");

  cudaMalloc((void**)&dvalues, sizeof(word) * 32 ); 
  cudaMalloc((void**)&dresult, sizeof(word) * 32 ); 
  cudaMemcpy(dvalues, values, sizeof(word) * 32, cudaMemcpyHostToDevice);
  //mm1<<<1, 16,2 * 32 * sizeof(unsigned int)>>>((int*)dvalues,(int*)dresult);
  //mm2<<<1, 32,2 * 32 * sizeof(unsigned int)>>>((int*)dvalues,(int*)dresult);
 
  cudaMemcpy(result, dresult, sizeof(word) * 32 , cudaMemcpyDeviceToHost);
  cudaFree(dvalues);
  cudaFree(dresult);
  
  // show results 
  for (int i = 0; i < 32; ++i) { 
    printf("%d ", ((int*)result)[i]);
  }

}
