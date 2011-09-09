#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>


// Code Generated with GCDObsidian
void sklansky(uint32_t bid,uint32_t *input0,uint32_t *result0){
  unsigned char *sbase;
  sbase = (unsigned char*) malloc(256);
  for (uint32_t tid = 0; tid < 32; ++tid){
    ((uint32_t *)sbase)[tid] = ((tid&1)<1) ? input0[((bid*32)+((tid&4294967294)|(tid&1)))] : (input0[((bid*32)+((tid&4294967294)|(((tid&1)-1)+1)))]+input0[((bid*32)+((tid&4294967294)|0))]);
    
  }
  for (uint32_t tid = 0; tid < 32; ++tid){
    ((uint32_t *)(sbase+128))[tid] = ((tid&3)<2) ? ((uint32_t *)sbase)[((tid&4294967292)|(tid&3))] : (((uint32_t *)sbase)[((tid&4294967292)|(((tid&3)-2)+2))]+((uint32_t *)sbase)[((tid&4294967292)|1)]);
    
  }
  for (uint32_t tid = 0; tid < 32; ++tid){
    ((uint32_t *)sbase)[tid] = ((tid&7)<4) ? ((uint32_t *)(sbase+128))[((tid&4294967288)|(tid&7))] : (((uint32_t *)(sbase+128))[((tid&4294967288)|(((tid&7)-4)+4))]+((uint32_t *)(sbase+128))[((tid&4294967288)|3)]);
    
  }
  for (uint32_t tid = 0; tid < 32; ++tid){
    ((uint32_t *)(sbase+128))[tid] = ((tid&15)<8) ? ((uint32_t *)sbase)[((tid&4294967280)|(tid&15))] : (((uint32_t *)sbase)[((tid&4294967280)|(((tid&15)-8)+8))]+((uint32_t *)sbase)[((tid&4294967280)|7)]);
    
  }
  for (uint32_t tid = 0; tid < 32; ++tid){
    ((uint32_t *)sbase)[tid] = (tid<16) ? ((uint32_t *)(sbase+128))[tid] : (((uint32_t *)(sbase+128))[((tid-16)+16)]+((uint32_t *)(sbase+128))[15]);
    
  }
  for (uint32_t tid = 0; tid < 32; ++tid){
    result0[((bid*32)+tid)] = ((uint32_t *)sbase)[tid];
    
  }
  free(sbase);
}



// Change below to match kernel above 
int main(int argc, char **argv){
  uint32_t values[32];
  uint32_t result[32];
 
  //generate input data
  for (int i = 0; i < 32; ++i) { 
    values[i] = i; 
  }

  sklansky(0,values,result);

  
  // show results 
  for (int i = 0; i < 32; ++i) { 
    printf("%d ", result[i]);
  }

}
