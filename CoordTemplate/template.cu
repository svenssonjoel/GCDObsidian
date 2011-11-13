#include <stdio.h>
#include <stdlib.h>


// Code Generated with GCDObsidian
__global__ void kernel0(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  //  extern __shared__ unsigned char sbase[];

  result0[((bid*32)+tid)] =  input0[((bid*32)+tid&15)] + 32;
  
}

// coordination code we want to generate
int coord(int *input0, int input0size, int *output0, int output0size){ 
  
  int* dinput0;
  int* doutput0;

  cudaMalloc((void**)&dinput0, sizeof(int) * input0size ); 
  cudaMalloc((void**)&doutput0, sizeof(int) * output0size ); 
  cudaMemcpy(dinput0, input0, sizeof(int) * input0size, cudaMemcpyHostToDevice);
  kernel0<<<1, 32,0 >>>((int*)dinput0,(int*)doutput0);
  cudaMemcpy(output0, doutput0, sizeof(int) * 32 , cudaMemcpyDeviceToHost);
  cudaFree(dinput0);
  cudaFree(doutput0);
 
  return 0; // Also. add some error checking... 
}




int main(int argc, char **argv){
  int values[32];
  int result[32];
 

  //generate input data
  for (int i = 0; i < 32; ++i) { 
    values[i] = i; 
  }


  coord(values,32,result,32);
  
  // show results 
  for (int i = 0; i < 32; ++i) { 
    printf("%d ", ((int*)result)[i]);
  }

}

