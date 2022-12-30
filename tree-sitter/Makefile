
CFLAGS=`pkg-config --cflags swipl tree-sitter` -Wall
LFLAGS=`pkg-config --libs tree-sitter` -lstdc++
OBJ=norg-parser.o
LIB=norg_parser.so

.PHONY: all
all: $(LIB)

.PHONY: clean
clean:
	rm -f $(OBJ) $(LIB) parser.o scanner.o

$(OBJ): main.c Makefile
	gcc $(CFLAGS) -o $@ -c $<

parser.o: ${NORG_PARSER_SOURCE}/parser.c
	gcc $(CFLAGS) -o $@ -c $<

scanner.o: ${NORG_PARSER_SOURCE}/scanner.cc
	g++ $(CFLAGS) -o $@ -c $<

$(LIB): $(OBJ) parser.o scanner.o Makefile
	swipl-ld $(LFLAGS) -shared -o $@ $< parser.o scanner.o



