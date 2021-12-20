BIN = $(shell find . -name 'day_*.scm' | sed 's|\.scm$$||g')

all : CSCFLAGS += -O5
all : $(BIN)

debug : CSCFLAGS += -O0 -d3
debug : $(BIN)

% : %.scm
	csc $(CSCFLAGS) $<

clean :
	rm -f $(BIN)

.PHONY : all debug clean
