
CFLAGS=-Wall `pkg-config --cflags swipl`
LFLAGS=
OBJ=posix.o
LIB=posix.so

.PHONY: all
all: $(LIB)

.PHONY: clean
clean:
	rm -f $(OBJ) $(LIB)

$(OBJ): main.c Makefile
	gcc $(CFLAGS) -o $@ -c $<

$(LIB): $(OBJ) Makefile
	swipl-ld $(LFLAGS) -shared -o $@ $<



