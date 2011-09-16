TARGET := template

GCC := gcc -std=c99 

INC := -I/usr/local/cuda/include
LIB := -lOpenCL

all: $(TARGET)
$(TARGET): $(TARGET).c
	$(GCC) -o $(TARGET) $(TARGET).c $(INC) $(LIB)

clean:
	rm $(TARGET)
