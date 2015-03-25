NAME =	bistro

ML  =	bigint.ml \
	arithExpr.ml \
	bistroReloaded.ml

MLI =	bigint.mli \
	arithExpr.mli \
	bistroReloaded.mli

CAMLC	  = ocamlc
CAMLDEP	  = ocamldep
CAMLOPT   = ocamlopt
CAMLFLAGS = -w A

CMX = $(ML:.ml=.cmx)
CMO = $(ML:.ml=.cmo)
CMI = $(ML:.ml=.cmi)

.PHONY: byte clean fclean re
.SUFFIXES: .ml .mli .cmo .cmi .cmx

all: $(NAME)
byte: $(NAME).byte

$(NAME):$(CMO)
	$(CAMLC)  -o $(NAME) $+

$(NAME).byte:$(CMX)
	     $(CAMLOPT) -o $(NAME) $+


.ml.cmo:
	$(CAMLC) -c $(CAMLFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(CAMLFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(CAMLFLAGS) $<

clean:
	rm -f $(CMX) $(CMO) $(CMI) $(CMX:.cmx=.o) .depend

fclean: clean
	rm -f $(NAME)

re: fclean all

.depend:$(MLI) $(ML)
	$(CAMLDEP) $(ML) $(MLI) > .depend

-include .depend
