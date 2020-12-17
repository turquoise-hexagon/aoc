CSC      ?= csc
CSCFLAGS ?= -O5

BIN = $(shell printf '%s\n' *.scm | sed 's|\.scm$$||g')
#BIN  = $(NAME)
#SRC  = $(NAME).scm

all : $(BIN)

% : %.scm
	$(CSC) $(CSCFLAGS) $< -o $@

clean :
	rm -f $(BIN)

.PHONY : all clean
