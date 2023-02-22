##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile
##

SRC	=	Read.hs	\
		Compute.hs	\
		interpreteursLength.hs	\

CC	=	ghc

NAME	=	interpreteursLength

FLAGS	+=	-Wall

all:	$(NAME)

$(NAME):	$(SRC)
	@$(CC) $(FLAGS) $(NAME)

test:
	ghci $(SRC)

clean:
	@rm -f *.o
	@rm -f *.hi

fclean:	clean
	@rm -f $(NAME)

re:	fclean all

.PHONY: all test clean fclean re
