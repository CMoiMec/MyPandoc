##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## Makefile
##

NAME= 	mypandoc

PAF	=	$(shell stack path --local-install-root)

all: 	$(NAME)

$(NAME):
	cd Pandoc
	stack build
	cp $(PAF)/bin/Pandoc-exe .
	mv Pandoc-exe $(NAME)

clean:
	rm -f *.o

fclean: clean
	rm -rf $(EXE)

re: 	fclean all

.PHONY: clean, fclean, re, $(NAME)