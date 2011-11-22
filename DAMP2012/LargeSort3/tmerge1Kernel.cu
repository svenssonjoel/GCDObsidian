__global__ void tmerge1(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
  (( int *)sbase)[tid] = ((tid&256)==0) ? min(input0[((bid*512)+tid)],input0[((bid*512)+(tid^511))]) : max(input0[((bid*512)+tid)],input0[((bid*512)+(tid^511))]);
  __syncthreads();
  (( int *)(sbase + 2048))[tid] = ((tid&128)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^128)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^128)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&64)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^64)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^64)]);
  __syncthreads();
  (( int *)(sbase + 2048))[tid] = ((tid&32)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^32)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^32)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&16)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^16)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^16)]);
  __syncthreads();
  (( int *)(sbase + 2048))[tid] = ((tid&8)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^8)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^8)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&4)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^4)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^4)]);
  __syncthreads();
  (( int *)(sbase + 2048))[tid] = ((tid&2)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^2)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^2)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&1)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^1)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^1)]);
  __syncthreads();
  result0[((bid*512)+tid)] = (( int *)sbase)[tid];
  
}
