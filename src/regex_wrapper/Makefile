# Part of an easy but machine inefficient interface to libc regex
# facilities.

# See the public header regex_wrapper.h for details.

include ../../make_support/system_rules

# The name of this library.
LIB_NAME = regex_wrapper

HEADERS = $(LIB_NAME).h

OBJS = regex_match.o \
       get_subexp_string.o \
       matched_subexps_free.o

# Build and install.
all: build_only
	mv $(LIB_NAME).a $(LIBDIR)
	$(MAKE) clean

# Build, but don't install.
build_only: $(OBJS)
	ar rv $(LIB_NAME).a $(OBJS)
	$(RANLIB) $(LIB_NAME).a

# For simplicity, everything is rebuilt if the Makefile or any header
# or library we depend on has changed.
$(OBJS): Makefile $(ASF_INCLUDE_DIR)/$(LIB_NAME).h

# Compile and run a test program.
check: clean build_only test/test_regex_wrapper.c
	$(CC) -c -I. $(CFLAGS) -ggdb3 test/test_regex_wrapper.c \
              -o test/test_regex_wrapper.o
	$(CC) test/test_regex_wrapper.o $(LIB_NAME).a \
              -o test/test_regex_wrapper
	@echo "\nStarting automatic testing...\n"
	@if test/test_regex_wrapper; then \
             echo "\nAutomatic testing succeeded.\n"; \
         else \
             echo "\nAutomatic testing failed.\n"; \
         fi

clean:
	rm -f $(LIB_NAME).a *.o *~ 
	rm -f test/test_regex_wrapper test/test_regex_wrapper.o test/*~
