#ifndef _VSORT_KERNEL_CU_
#define _VSORT_KERNEL_CU_

#define NUM    512

__device__ inline void swap(int & a, int & b)
{	
    int tmp = a;
    a = b;
    b = tmp;
}
__global__ void vsort(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
  (( int *)sbase)[tid] = ((tid&256)==0) ? min(input0[((bid*512)+tid)],input0[((bid*512)+(tid^256))]) : max(input0[((bid*512)+tid)],input0[((bid*512)+(tid^256))]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&256)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^384)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^384)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&128)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^128)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^128)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&256)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^448)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^448)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&128)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^192)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^192)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&64)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^64)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^64)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&256)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^480)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^480)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&128)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^224)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^224)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&64)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^96)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^96)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&32)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^32)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^32)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&256)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^496)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^496)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&128)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^240)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^240)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&64)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^112)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^112)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&32)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^48)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^48)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&16)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^16)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^16)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&256)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^504)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^504)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&128)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^248)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^248)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&64)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^120)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^120)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&32)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^56)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^56)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&16)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^24)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^24)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&8)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^8)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^8)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&256)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^508)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^508)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&128)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^252)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^252)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&64)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^124)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^124)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&32)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^60)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^60)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&16)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^28)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^28)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&8)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^12)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^12)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&4)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^4)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^4)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&256)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^510)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^510)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&128)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^254)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^254)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&64)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^126)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^126)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&32)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^62)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^62)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&16)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^30)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^30)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&8)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^14)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^14)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&4)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^6)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^6)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&2)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^2)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^2)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&256)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^511)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^511)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&128)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^255)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^255)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&64)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^127)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^127)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&32)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^63)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^63)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&16)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^31)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^31)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&8)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^15)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^15)]);
  __syncthreads();
  (( int *)sbase)[tid] = ((tid&4)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^7)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^7)]);
  __syncthreads();
  (( int *)(sbase+2048))[tid] = ((tid&2)==0) ? min((( int *)sbase)[tid],(( int *)sbase)[(tid^3)]) : max((( int *)sbase)[tid],(( int *)sbase)[(tid^3)]);
  __syncthreads();
  result0[((bid*512)+tid)] = ((tid&1)==0) ? min((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^1)]) : max((( int *)(sbase+2048))[tid],(( int *)(sbase+2048))[(tid^1)]);
  
}

/* 
__global__ static void vsort(int *input0,int *result0){
  unsigned int tid = threadIdx.x;
  unsigned int bid = blockIdx.x;
  extern __shared__  int sbase[];
  extern __shared__  int tbase[];
  ((int *)sbase)[tid] = ((tid&256)==0) ? min(input0[((bid*512)+tid)],input0[((bid*512)+(tid^256))]) : max(input0[((bid*512)+tid)],input0[((bid*512)+(tid^256))]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&256)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^384)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^384)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&128)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^128)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^128)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&256)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^448)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^448)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&128)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^192)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^192)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&64)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^64)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^64)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&256)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^480)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^480)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&128)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^224)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^224)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&64)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^96)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^96)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&32)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^32)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^32)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&256)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^496)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^496)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&128)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^240)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^240)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&64)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^112)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^112)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&32)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^48)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^48)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&16)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^16)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^16)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&256)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^504)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^504)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&128)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^248)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^248)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&64)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^120)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^120)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&32)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^56)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^56)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&16)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^24)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^24)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&8)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^8)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^8)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&256)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^508)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^508)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&128)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^252)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^252)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&64)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^124)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^124)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&32)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^60)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^60)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&16)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^28)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^28)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&8)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^12)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^12)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&4)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^4)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^4)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&256)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^510)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^510)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&128)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^254)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^254)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&64)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^126)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^126)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&32)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^62)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^62)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&16)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^30)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^30)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&8)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^14)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^14)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&4)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^6)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^6)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&2)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^2)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^2)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&256)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^511)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^511)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&128)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^255)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^255)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&64)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^127)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^127)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&32)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^63)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^63)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&16)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^31)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^31)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&8)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^15)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^15)]);
  __syncthreads();
  ((int *)sbase)[tid] = ((tid&4)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^7)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^7)]);
  __syncthreads();
  ((int *)tbase)[tid] = ((tid&2)==0) ? min(((int *)sbase)[tid],((int *)sbase)[(tid^3)]) : max(((int *)sbase)[tid],((int *)sbase)[(tid^3)]);
  __syncthreads(); 
  result0[((bid*512)+tid)] = ((tid&1)==0) ? min(((int *)tbase)[tid],((int *)tbase)[(tid^1)]) : max(((int *)tbase)[tid],((int *)tbase)[(tid^1)]);
  __syncthreads();
}
*/



#endif // _VSORT_KERNEL_H_
