CC=ocamlopt
RM	= rm -f
DIR_SRC = ./src
DIR_BUILD=./build
EXEC = $(DIR_BUILD)/main
SRC = $(wildcard ${DIR_SRC}/*.cmx)


all : $(EXEC)

$(EXEC): ${DIR_SRC}/Util.cmx ${DIR_SRC}/Abr.cmx ${DIR_SRC}/Com_list_abr.cmx ${DIR_SRC}/Com_map_abr.cmx ${DIR_SRC}/Generation_list.cmx ${DIR_SRC}/main.cmx 
	$(CC) -o $@ $^

${DIR_SRC}/Util.cmx: ${DIR_SRC}/Util.ml
	$(CC) -c $^

${DIR_SRC}/Abr.cmx: ${DIR_SRC}/Abr.ml
	cd src; $(CC) -c Abr.ml

${DIR_SRC}/Com_list_abr.cmx: ${DIR_SRC}/Com_list_abr.ml
	cd src; $(CC) -c Com_list_abr.ml

${DIR_SRC}/Com_map_abr.cmx: ${DIR_SRC}/Com_map_abr.ml
	cd src; $(CC) -c Com_map_abr.ml

${DIR_SRC}/Generation_list.cmx: ${DIR_SRC}/Generation_list.ml
	cd src; $(CC) -c Generation_list.ml

${DIR_SRC}/main.cmx: ${DIR_SRC}/main.ml
	cd src; $(CC) -c main.ml




ast.dot: $(EXEC)
	./build/abr > ast.dot

ast: ast.dot
	dot -Tpdf ast.dot -o ast.pdf

compressor.dot: $(EXEC)
	./build/abr > compressor.dot

compressor: compressor.dot
	dot -Tpdf compressor.dot -o compressor.pdf

compressorMap.dot: $(EXEC)
	./build/abr > compressorMap.dot

compressorMap: compressorMap.dot 
	dot -Tpdf compressorMap.dot -o compressorMap.pdf


.PHONY: clean test
clean:
	$(RM) $(DIR_SRC)/*.cmo $(DIR_SRC)/*.cmi $(DIR_SRC)/*.cmx $(DIR_SRC)/*.o
	$(RM) *.cmo *.cmi
	$(RM) $(EXEC)

test:
	$(EXEC)
