all: proc clean
proc: my.l my.y
	flex my.l
	yacc -d my.y
	cc lex.yy.c  y.tab.c -o my
clean:
	rm lex.yy.c y.tab.h y.tab.c
