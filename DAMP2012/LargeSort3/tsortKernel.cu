 
__global__ void tsortSmall(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__  unsigned char sbase[];
  (( int *)sbase)[(tid<<1)] = min(input0[((bid*512)+(tid<<1))],input0[((bid*512)+((tid<<1)^1))]);
  (( int *)sbase)[((tid<<1)^1)] = max(input0[((bid*512)+(tid<<1))],input0[((bid*512)+((tid<<1)^1))]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967294))] = min((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^3)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967294))^3)] = max((( int *)sbase)[(tid+(tid&4294967294))],(( int *)sbase)[((tid+(tid&4294967294))^3)]);
  __syncthreads();
  (( int *)sbase)[(tid<<1)] = min((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  (( int *)sbase)[((tid<<1)^1)] = max((( int *)(sbase+2048))[(tid<<1)],(( int *)(sbase+2048))[((tid<<1)^1)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967292))] = min((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^7)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967292))^7)] = max((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^7)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967294))] = min((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  (( int *)sbase)[((tid+(tid&4294967294))^2)] = max((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid<<1)] = min((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  (( int *)(sbase + 2048))[((tid<<1)^1)] = max((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967288))] = min((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^15)]);
  (( int *)sbase)[((tid+(tid&4294967288))^15)] = max((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^15)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967292))] = min((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967292))^4)] = max((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967294))] = min((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  (( int *)sbase)[((tid+(tid&4294967294))^2)] = max((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid<<1)] = min((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  (( int *)(sbase + 2048))[((tid<<1)^1)] = max((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967280))] = min((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^31)]);
  (( int *)sbase)[((tid+(tid&4294967280))^31)] = max((( int *)(sbase+2048))[(tid+(tid&4294967280))],(( int *)(sbase+2048))[((tid+(tid&4294967280))^31)]);
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
  (( int *)(sbase + 2048))[(tid+(tid&4294967264))] = min((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^63)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967264))^63)] = max((( int *)sbase)[(tid+(tid&4294967264))],(( int *)sbase)[((tid+(tid&4294967264))^63)]);
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
  (( int *)(sbase + 2048))[(tid+(tid&4294967232))] = min((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^127)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967232))^127)] = max((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^127)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967264))] = min((( int *)(sbase+2048))[(tid+(tid&4294967264))],(( int *)(sbase+2048))[((tid+(tid&4294967264))^32)]);
  (( int *)sbase)[((tid+(tid&4294967264))^32)] = max((( int *)(sbase+2048))[(tid+(tid&4294967264))],(( int *)(sbase+2048))[((tid+(tid&4294967264))^32)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967280))] = min((( int *)sbase)[(tid+(tid&4294967280))],(( int *)sbase)[((tid+(tid&4294967280))^16)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967280))^16)] = max((( int *)sbase)[(tid+(tid&4294967280))],(( int *)sbase)[((tid+(tid&4294967280))^16)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967288))] = min((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^8)]);
  (( int *)sbase)[((tid+(tid&4294967288))^8)] = max((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^8)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967292))] = min((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967292))^4)] = max((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967294))] = min((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  (( int *)sbase)[((tid+(tid&4294967294))^2)] = max((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid<<1)] = min((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  (( int *)(sbase + 2048))[((tid<<1)^1)] = max((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967168))] = min((( int *)(sbase+2048))[(tid+(tid&4294967168))],(( int *)(sbase+2048))[((tid+(tid&4294967168))^255)]);
  (( int *)sbase)[((tid+(tid&4294967168))^255)] = max((( int *)(sbase+2048))[(tid+(tid&4294967168))],(( int *)(sbase+2048))[((tid+(tid&4294967168))^255)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967232))] = min((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^64)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967232))^64)] = max((( int *)sbase)[(tid+(tid&4294967232))],(( int *)sbase)[((tid+(tid&4294967232))^64)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967264))] = min((( int *)(sbase+2048))[(tid+(tid&4294967264))],(( int *)(sbase+2048))[((tid+(tid&4294967264))^32)]);
  (( int *)sbase)[((tid+(tid&4294967264))^32)] = max((( int *)(sbase+2048))[(tid+(tid&4294967264))],(( int *)(sbase+2048))[((tid+(tid&4294967264))^32)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967280))] = min((( int *)sbase)[(tid+(tid&4294967280))],(( int *)sbase)[((tid+(tid&4294967280))^16)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967280))^16)] = max((( int *)sbase)[(tid+(tid&4294967280))],(( int *)sbase)[((tid+(tid&4294967280))^16)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967288))] = min((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^8)]);
  (( int *)sbase)[((tid+(tid&4294967288))^8)] = max((( int *)(sbase+2048))[(tid+(tid&4294967288))],(( int *)(sbase+2048))[((tid+(tid&4294967288))^8)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid+(tid&4294967292))] = min((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  (( int *)(sbase + 2048))[((tid+(tid&4294967292))^4)] = max((( int *)sbase)[(tid+(tid&4294967292))],(( int *)sbase)[((tid+(tid&4294967292))^4)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967294))] = min((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  (( int *)sbase)[((tid+(tid&4294967294))^2)] = max((( int *)(sbase+2048))[(tid+(tid&4294967294))],(( int *)(sbase+2048))[((tid+(tid&4294967294))^2)]);
  __syncthreads();
  (( int *)(sbase + 2048))[(tid<<1)] = min((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  (( int *)(sbase + 2048))[((tid<<1)^1)] = max((( int *)sbase)[(tid<<1)],(( int *)sbase)[((tid<<1)^1)]);
  __syncthreads();
  (( int *)sbase)[(tid+(tid&4294967040))] = min((( int *)(sbase+2048))[(tid+(tid&4294967040))],(( int *)(sbase+2048))[((tid+(tid&4294967040))^511)]);
  (( int *)sbase)[((tid+(tid&4294967040))^511)] = max((( int *)(sbase+2048))[(tid+(tid&4294967040))],(( int *)(sbase+2048))[((tid+(tid&4294967040))^511)]);
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