# Compiler and flags
CC = gcc

CFLAGS = -fPIC -Wall -Wextra -O2
LDFLAGS = -shared

# Debug flags
DEBUG_CFLAGS = -fPIC -Wall -Wextra -g
DEBUG_LDFLAGS = -shared

# Source files
SRCS = common.c file.c os.c

# Object files
OBJS = $(SRCS:.c=.o)

# Debug object files
DEBUG_OBJS = $(SRCS:.c=.debug.o)

# Shared library names
TARGET = libchezpp.so
DEBUG_TARGET = libchezpp_debug.so

# Default target
all: $(TARGET)

# Rule to create shared library
$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS)

# Rule to create debug shared library
debug: $(DEBUG_TARGET)

$(DEBUG_TARGET): $(DEBUG_OBJS)
	$(CC) $(DEBUG_LDFLAGS) -o $@ $(DEBUG_OBJS)

# Rule to create object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Rule to create debug object files
%.debug.o: %.c
	$(CC) $(DEBUG_CFLAGS) -c $< -o $@

# Clean up
clean:
	rm -f $(OBJS) $(DEBUG_OBJS) $(TARGET) $(DEBUG_TARGET)

# Phony targets
.PHONY: all debug clean
