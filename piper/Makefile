OUT=piper.out
OBJS=main.o
CFLAGS=-Wall -Wextra -Werror
LDFLAGS=
CC=gcc

.PHONY: all
all: $(OUT)

$(OUT): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f $(OUT)
	rm -f $(OBJS)
