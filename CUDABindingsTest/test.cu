
extern "C" void two(int *, int*);

__global__ void two(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__ unsigned char sbase[];
  ((int *)sbase)[tid] = input0[((bid*32)+((tid&4294967280)|(15-(tid&15))))];
  __syncthreads();
  result0[((bid*32)+tid)] = ((int *)sbase)[tid];
  
}
