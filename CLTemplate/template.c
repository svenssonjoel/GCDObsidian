
#include <stdio.h>
#include <stdlib.h>
#include <CL/cl.h>

#define SIZE 32
#define NUMBLOCKS 1

// Add generated code here 

const char* source[] = {
  "__kernel void generated(__global int *input0,__global int *result0){"
  "unsigned int tid = get_local_id(0);"
  "unsigned int bid = (get_global_id(0)-tid) / get_local_size(0);"
  "__local unsigned char sbase[256];"
  "((int *)sbase)[tid] = input0[((bid*32)+((tid&4294967280)|(15-(tid&15))))];"
  "barrier(CLK_LOCAL_MEM_FENCE);"
  "result0[((bid*32)+tid)] = ((int *)sbase)[tid];"
  "}"
};


int main(int argc, char **argv)
{
  int values[SIZE];
  int results[SIZE];

  for(int i = 0; i < SIZE; i++){
    values[i] = i;
  }

  for(int i = 0; i < SIZE; i++){
    printf("%d ", values[i]);
  }


  cl_platform_id clpf;
  clGetPlatformIDs(1, &clpf, NULL);

  // I guess this means: "is there a GPU in the system?" 
  cl_device_id cldev;
  clGetDeviceIDs(clpf, CL_DEVICE_TYPE_GPU, 1, &cldev, NULL);

  // Show informaion about found device
  char cBuffer[1024];
  clGetDeviceInfo(cldev, CL_DEVICE_NAME, sizeof(cBuffer), &cBuffer, NULL);
  printf("CL_DEVICE_NAME:\t%s\n", cBuffer);
  clGetDeviceInfo(cldev, CL_DRIVER_VERSION, sizeof(cBuffer), &cBuffer, NULL);
  printf("CL_DRIVER_VERSION:\t%s\n\n", cBuffer);


  // Handle on the "device"
  cl_context GPUContext = clCreateContext(0, 1, &cldev, NULL, NULL, NULL);

  cl_command_queue cqCommandQueue = clCreateCommandQueue(GPUContext, cldev, 0, NULL);

  // Copy host values into the GPU 
  cl_mem dvalues = 
    clCreateBuffer(GPUContext, 
                   CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, 
		   sizeof(int) * SIZE, 
		   values, 
		   NULL);

  // result array on GPU 
  cl_mem dresults = 
    clCreateBuffer(GPUContext, 
                   CL_MEM_WRITE_ONLY,
                   sizeof(int) * SIZE, 
                   NULL, 
                   NULL);


  cl_program clProgram = 
    clCreateProgramWithSource(GPUContext, 1, source, NULL, NULL); // the integer is "number of strings"

  // Compile 
  int r = clBuildProgram(clProgram, 0, NULL, NULL, NULL, NULL);
						       
  if (r != CL_SUCCESS) {
    printf("Bad! cannot compile cl source\n");
    exit(EXIT_FAILURE);
  }
      						       
  // Kernel 
  cl_kernel generated = clCreateKernel(clProgram,"generated", NULL);


  clSetKernelArg(generated, 0, sizeof(cl_mem), &dvalues);   
  clSetKernelArg(generated, 1, sizeof(cl_mem), &dresults);


  // Set up the "grid" and "block" 
  size_t nThreadsLocal[1] = {SIZE}; 
  size_t nThreadsGlobal[1] = {SIZE*NUMBLOCKS};
  r = clEnqueueNDRangeKernel(cqCommandQueue, 
       		             generated, 
			     1, 
			     NULL,
                             nThreadsLocal, 
			     nThreadsGlobal, 
			     0, 
			     NULL, 
			     NULL);
 
  if (r != CL_SUCCESS) {
    printf("Bad! cannot launch NDRange. Error %d \n",r);
    exit(EXIT_FAILURE);
  }
  

  r = clEnqueueReadBuffer(cqCommandQueue, 
		          dresults, 
		          CL_TRUE, 
		          0, 
                          SIZE * sizeof(int), 
		          results, 0, NULL, NULL);

  if (r != CL_SUCCESS) {
    printf("BAD!\n");
    exit(EXIT_FAILURE);
  }

  
  clReleaseKernel(generated);
  clReleaseProgram(clProgram);
  clReleaseCommandQueue(cqCommandQueue);
  clReleaseContext(GPUContext);
  clReleaseMemObject(dvalues);
  clReleaseMemObject(dresults);

  // Print out the results
  for (int i = 0; i < SIZE; i++){
    printf("%d ",results[i]);
  }

  return 0;
}
