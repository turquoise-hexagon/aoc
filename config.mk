CSC      ?= csc
CSCFLAGS ?= -O5

BIN = $(shell printf '%s\n' *.scm | sed 's|\.scm$$||g')

all : $(BIN)

% : %.scm
	$(CSC) $(CSCFLAGS) $<

clean :
	rm -f $(BIN)

.PHONY : all clean
