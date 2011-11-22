

#define NUM    512

__device__ inline void swap(int & a, int & b)
{
	// Alternative swap doesn't use a temporary register:
	// a ^= b;
	// b ^= a;
	// a ^= b;
	
    int tmp = a;
    a = b;
    b = tmp;
}

__global__ static void bitonicSort(int * values, int *results)
{
    extern __shared__ int shared[];

    const unsigned int tid = threadIdx.x;
    const unsigned int bid = blockIdx.x;

    // Copy input to shared mem.
    shared[tid] = values[(bid*NUM) + tid];

    __syncthreads();

    // Parallel bitonic sort.
    for (unsigned int k = 2; k <= NUM; k *= 2)
    {
        // Bitonic merge:
        for (unsigned int j = k / 2; j>0; j /= 2)
        {
            unsigned int ixj = tid ^ j;
            
            if (ixj > tid)
            {
                if ((tid & k) == 0)
                {
                    if (shared[tid] > shared[ixj])
                    {
                        swap(shared[tid], shared[ixj]);
                    }
                }
                else
                {
                    if (shared[tid] < shared[ixj])
                    {
                        swap(shared[tid], shared[ixj]);
                    }
                }
            }
            
            __syncthreads();
        }
    }

    // Write result.
    results[(bid*NUM) + tid] = shared[tid];
}
