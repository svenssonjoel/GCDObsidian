

#include <stdio.h>
#include <stdlib.h>



#include "vsort_kernel.cu"




int main(int argc, char** argv)
{
    int values[NUM];

    for(int i = 0; i < NUM; i++)
    {
        values[i] = rand() % 256;
        printf("%d ", values[i]);
    }
    printf( "\n");
    printf( "------------------------------\n");
    
    

    int * dvalues;
    cudaMalloc((void**)&dvalues, sizeof(int) * NUM);
    cudaMemcpy(dvalues, values, sizeof(int) * NUM, cudaMemcpyHostToDevice);
    
    int * rvalues;
    cudaMalloc((void**)&rvalues, sizeof(int) * NUM);
    


    vsort<<<1, NUM, 2* sizeof(int) * NUM>>>((int*)dvalues,(int*)rvalues);

   
    cudaMemcpy(values, rvalues, sizeof(int) * NUM, cudaMemcpyDeviceToHost);

    cudaFree(dvalues);
    cudaFree(rvalues);
    

    bool passed = true;
    for(int i = 1; i < NUM; i++)
    {   
        printf( "%d ", values[i-1]); 
        
        if (values[i-1] > values[i])
        {
            passed = false;
            printf("%d ", values[i-1] );
            
        }
    }
    passed ? printf("PASSED :)\n"): printf("FAILED\n");
    

    cudaThreadExit();


}
