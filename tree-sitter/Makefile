
CFLAGS=`pkg-config --cflags swipl` -Wall -Werror
OBJ=norg-parser.o
LIB=norg_parser.so

.PHONY: all
all: $(LIB)

.PHONY: clean
clean:
	rm -f $(OBJ) $(LIB)

$(OBJ): main.c Makefile
	gcc $(CFLAGS) -o $@ -c $<

$(LIB): $(OBJ) Makefile
	swipl-ld -shared -o $@ $<



