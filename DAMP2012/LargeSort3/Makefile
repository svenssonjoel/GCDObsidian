TARGET := mssort
COMMON_PATH := $(CUDA_SDK_PATH)/common

LIBPATHS := -L$(CUDA_INSTALL_PATH)/lib -L$(COMMON_PATH)/lib -L$(CUDA_SDK_PATH)/lib
INCPATHS := -I$(CUDA_INSTALL_PATH)/include -I$(COMMON_PATH)/inc
LIBRARIES := -lcuda -lcudart

LIBRARIESEMU :=-lcudart

NVCC := nvcc 

all: $(TARGET) 
$(TARGET): $(TARGET).cu
	$(NVCC) -o $(TARGET) $(TARGET).cu $(INCPATHS) $(LIBPATHS) $(LIBRARIES) -O2

emulation: $(TARGET).cu
	$(NVCC) -deviceemu -o $(TARGET) $(TARGET).cu $(INCPATHS) $(LIBPATHS) $(LIBRARIESEMU)


# The -O3 has effect only on the CPU experiment
sorttest: sorttest.cu
	$(NVCC) -o sorttest sorttest.cu $(INCPATHS) $(LIBPATHS) $(LIBRARIES) -O3

clean:
	rm $(TARGET)
	rm sorttest

