OFLAGS=-annot
LFLAGS=-cc g++
OC=ocamlc
OBJS = Parse.cmo Codegen.cmo Main.cmo
INTERFACES = Parse.cmi Codegen.cmi Main.cmi
LIBS= llvm.cma llvm_bitwriter.cma

all: compile tags

compile: ${INTERFACES} ${OBJS}
	${OC} ${LFLAGS} ${LIBS} ${OBJS} -o $@

%.cmo: %.cmi %.ml
	${OC} ${OFLAGS} -c $*.ml

%.cmi: %.mli
	${OC} ${OFLAGS} -c $*.mli

clean:
	rm parse *.cmo *.cmi

tags:
	ctags --languages=+ocaml --langmap=ocaml:.ml -R .
