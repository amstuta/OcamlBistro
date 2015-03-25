NAME =	bistro

ML  =	bigint.ml \
	arithExpr.ml \
	bistroReloaded.ml

MLI =	bigint.mli \
	arithExpr.mli \
	bistroReloaded.mli

OCAMLFLAGS = -w A

CMX = $(ML:.ml=.cmx)
CMO = $(ML:.ml=.cmo)
CMI = $(ML:.ml=.cmi)

.PHONY: byte clean fclean re
.SUFFIXES: .ml .mli .cmo .cmi .cmx

all: $(NAME)
byte: $(NAME).byte

$(NAME):$(CMO)
	ocamlc  -o $(NAME) $+

$(NAME).byte:	$(CMX)
		ocamlopt -o $(NAME) $+


.ml.cmo:
	ocamlc -c $(OCAMLFLAGS) $<

.mli.cmi:
	ocamlc -c $(OCAMLFLAGS) $<

.ml.cmx:
	ocamlopt -c $(OCAMLFLAGS) $<

clean:
	rm -f $(CMX) $(CMO) $(CMI) $(CMX:.cmx=.o) .depend

fclean: clean
	rm -f $(NAME)

re: fclean all

.depend: $(MLI) $(ML)
	ocamldep $(ML) $(MLI) > .depend

-include .depend
