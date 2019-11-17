CC=ocamlc
RM	= rm -f
DIR_SRC = ./src
DIR_BUILD=./build
EXEC = $(DIR_BUILD)/abr
SRC = $(wildcard ${DIR_SRC}/*.ml)


all : $(EXEC)

$(EXEC): $(SRC)
	$(CC) -o $@ $^


ast:
	dot -Tpdf ast.dot -o ast.pdf

compressor:
	dot -Tpdf compressor.dot -o compressor.pdf


.PHONY: clean test
clean:
	$(RM) $(DIR_SRC)/*.cmo $(DIR_SRC)/*.cmi
	$(RM) *.cmo *.cmi
	$(RM) $(EXEC)

test:
	$(EXEC)
