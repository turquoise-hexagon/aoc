CSC      ?= csc
CSCFLAGS += -O5

NAME = $(shell basename $(PWD))
BIN  = $(NAME)
SRC  = $(NAME).scm

all : $(BIN)

$(BIN) : $(SRC)
	$(CSC) $(CSCFLAGS) $< -o $@

clean :
	rm $(BIN)

.PHONY : all clean