##
## Makefile for makefile in /home/amstuta/rendu/ocaml_bistro
##
## Made by arthur
## Login   <amstuta@epitech.net>
##
## Started on  Wed Mar 18 19:47:55 2015 arthur
## Last update Fri Mar 20 16:35:34 2015 arthur
##

SOURCES = Bigint.ml \
	  eval_expr.ml

EXEC = bistro

CAMLC = ocamlc
CAMLOPT = ocamlopt

all:	$(EXEC)

opt:	$(EXEC).opt

SMLIY = $(SOURCES:.mly=.ml)
SMLIYL = $(SMLIY:.mll=.ml)
SMLYL = $(filter %.ml,$(SMLIYL))
OBJS = $(SMLYL:.ml=.cmo)
OPTOBJS = $(OBJS:.cmx=.cmx)

$(EXEC):$(OBJS)
	$(CAMLC) $(CUSTOM) -o $(EXEC) $(LIBS) $(OBJS)

$(EXEC).opt:	$(OPTOBJS)
		$(CAMLOPT) -o $(EXEC) $(OPTOBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	rm -f *.cm[iox] *~ .*~ *.o

fclean:	clean
	rm -f $(EXEC)

re:	fclean all

.PHONY:	all clean fclean re
