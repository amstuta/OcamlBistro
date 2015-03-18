##
## Makefile for makefile in /home/amstuta/rendu/ocaml_bistro
##
## Made by arthur
## Login   <amstuta@epitech.net>
##
## Started on  Wed Mar 18 19:47:55 2015 arthur
## Last update Wed Mar 18 19:53:06 2015 arthur
##

RM	= rm -f

CAMLC	= ocamlc
CAMLOPT	= ocamlopt

NAME	= bistro

SRCS	= main.ml

OBJS	= $(SRCS:.ml=.cmo)

all:	$(NAME)

$(NAME):$(OBJS)
	$(CAMLC) $(OBJS) -o $(NAME)

.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	$(CAMLC) -c $<

clean:
	$(RM) *.cm[iox]

fclean:	clean
	$(RM) $(NAME)

re:	fclean all

.PHONY:	all clean fclean re
