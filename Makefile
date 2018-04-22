src/flex.yy.c: src/lambda.l
	flex -o $@ $<

y.tab.c: src/lambda.y
	yacc -d $<

lambda: src/flex.yy.c y.tab.c
	gcc -o $@ y.tab.c src/flex.yy.c -pthread -I./ -I/usr/include/guile/2.2 -lfl -lguile-2.2 -lgc

clean:
	rm y.tab* src/flex.yy.c lambda
