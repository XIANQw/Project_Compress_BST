CC=ocamlopt
RM	= rm -f
DIR_SRC = ./src
DIR_BUILD=./build
EXEC = $(DIR_BUILD)/abr
SRC = $(wildcard ${DIR_SRC}/*.cmx)


all : $(EXEC)

$(EXEC): ${DIR_SRC}/Util.cmx ${DIR_SRC}/abr.cmx
	$(CC) -o $@ $^

${DIR_SRC}/Util.cmx: ${DIR_SRC}/Util.ml
	$(CC) -c $^

${DIR_SRC}/abr.cmx: ${DIR_SRC}/abr.ml
	cd src; $(CC) -c abr.ml


ast:
	dot -Tpdf ast.dot -o ast.pdf

compressor:
	dot -Tpdf compressor.dot -o compressor.pdf


.PHONY: clean test
clean:
	$(RM) $(DIR_SRC)/*.cmo $(DIR_SRC)/*.cmi $(DIR_SRC)/*.cmx
	$(RM) *.cmo *.cmi
	$(RM) $(EXEC)

test:
	$(EXEC)
