##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile to build project
##
CFLAGS = -W -Wall -Wextras 
CC = ghc
EXEC = imageCompressor
OBJ = $(SRC:.hs=.o)
HI = $(SRC:.hs=.hi)
SRC = imageCompressor.hs

all: $(EXEC)

$(EXEC):
	stack build --allow-different-user
	cp .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/imageCompressor-exe/imageCompressor-exe ./imageCompressor

clean:
	rm -f $(OBJ)
	rm -f $(HI)
	stack clean --allow-different-user

fclean: clean
	rm -f $(EXEC)

re:		fclean all

.PHONY: 	all clean fclean re
