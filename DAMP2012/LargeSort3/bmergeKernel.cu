
__global__ void bmergeSmall(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__  unsigned char sbase[];
  (( int *)sbase)[(tid+(tid&4294967040))] = min(input0[((bid*512)+(tid+(tid&4294967040)))],input0[((bid*512)+((tid+(tid&4294967040))^256))]);
  (( int *)sbase)[((tid+(tid&4294967040))^256)] = max(input0[((bid*512)+(tid+(tid&4294967040)))],input0[((bid*512)+((tid+(tid&4294967040))^256))]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967168))] = min((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^128)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967168))^128)] = max((( int *)sbase)[(tid+(tid&4294967168))],(( int *)sbase)[((tid+(tid&4294967168))^128)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967232))] = min((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^64)]);
  (( int *)sbase)[((tid+(tid&4294967232))^64)] = max((( int *)(sbase+2048))[(tid+(tid&4294967232))],(( int *)(sbase+2048))[((tid+(tid&4294967232))^64)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967264))] = min((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^32)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967264))^32)] = max((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^32)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967280))] = min((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^16)]);
  (( int *)sbase)[((tid+(tid&4294967280))^16)] = max((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^16)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967288))] = min((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^8)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967288))^8)] = max((( int *)sbase)[(tid+(tid&4294967288))],(( int *)sbase)[((tid+(tid&4294967288))^8)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967292))] = min((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^4)]);
  (( int *)sbase)[((tid+(tid&4294967292))^4)] = max((( int *)(sbase+2048))[(tid+(tid&4294967292))],(( int *)(sbase+2048))[((tid+(tid&4294967292))^4)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967294))] = min((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^2)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967294))^2)] = max((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)sbase)[(tid<<1)] = min((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  (( int *)sbase)[((tid<<1)^1)] = max((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  __syncthreads();
  result0[((bid*512)+tid)] = (( int *)sbase)[tid];
  result0[((bid*512)+(tid+256))] = (( int *)sbase)[(tid+256)];
  
}
