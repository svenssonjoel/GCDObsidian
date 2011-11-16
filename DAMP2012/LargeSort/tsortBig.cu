
#include <stdio.h>
#include <stdlib.h>


#include <assert.h>
// #include <cutil_inline.h>
// #include "sortingNetworks_common.h"
// #include "sortingNetworks_common.cuh"


#define BLOCKS 2048 
#define LARGE_SIZE BLOCKS*SMALL_SIZE
#define SMALL_SIZE 512
#define THREADS 256
#define LOG_SMALL_SIZE 9

//* kernel for sorting small fixed size arrays all on the GPU

__global__ void tsortSmall(
    uint *d_input,
    uint *d_output)
{

}

//* Will be generated from Obsidian for SMALL_SIZE inputs



//* kernel for merging small fixed size arrays all on the GPU

__global__ void tmergeSmall(
    uint *d_input,
    uint *d_output)
{

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

    //  int v1 = d_input[ix];
    //int v2 = d_input[ix+ stride];
   
    //d_output[ix] = min(v1,v2);
    //d_output[ix + stride] = max(v1,v2);
    d_output[tid] = d_input[tid];
    
}




//* Assume input and output arrays have length 2^lenLog 
//* Small kernels work on SMALL_SIZE inputs. Assume lenLog >= ssLog > 0
void bitonicSort(
		 uint *d_input,
		 uint *d_output,
		 uint ssLog,
		 uint lenLog)
{
   
  uint arrayLength = 1 << lenLog;
  uint diff = lenLog - LOG_SMALL_SIZE;
  uint blocks = arrayLength / SMALL_SIZE;
  uint threads = SMALL_SIZE / 2;
    

    
  tsortSmall<<<blocks, threads>>>(d_input, d_output);

  for(int i = 0 ; i < diff ; i += 1){
  
    for(int j = i; j >= 0; j -= 1){ //Always true for uint
      //cSwap<<<blocks,threads>>>(d_input, d_output,(1<<j)*SMALL_SIZE);
      //* put correct synchronisation ? cudasynchronize();
    }
                
    tmergeSmall<<<blocks,threads>>>(d_input, d_output);
    

  }
}


int main(int argc, char *argv[]){ 
  
  int *values;
  int *result; 

  int *dvalues;
  int *dresult;

  values = (int*)malloc(LARGE_SIZE*sizeof(int));
  result = (int*)malloc(LARGE_SIZE*sizeof(int));
  
  for (int i = 0; i < LARGE_SIZE; ++i) { 
    values[i] = rand (); 
    // printf("%d ",values[i]);
  }
  
  /* Allocate GPU arrays */   
  cudaMalloc((void**)&dvalues, sizeof(int) * LARGE_SIZE ); 
  cudaMalloc((void**)&dresult, sizeof(int) * LARGE_SIZE ); 
  cudaMemcpy(dvalues, values, sizeof(int) * LARGE_SIZE, cudaMemcpyHostToDevice);
  cSwap<<<BLOCKS, SMALL_SIZE,0>>>((int*)dvalues,(int*)dresult,1);
  cudaMemcpy(result, dresult, sizeof(int) * LARGE_SIZE , cudaMemcpyDeviceToHost);
  cudaFree(dvalues);
  cudaFree(dresult);
 



  int passed = 1;
  for (int i = 1; i < LARGE_SIZE; ++i) { 
    if (i % 1000 == 0) {
      printf("% d",result[i]);
      printf("% d",result[i-1]);
    } 
    if (result[i] < result[i-1]) 
      passed = 0; 
  }
  
  printf("%s",passed ? "Passed!" : "Failed!");

  return 0;
}
