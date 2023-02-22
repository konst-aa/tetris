compile: shapes grid main.scm
	csc -o out shapes.o grid.o -uses shapes -uses grid main.scm
shapes: shapes.scm
	csc -c -J shapes.scm -unit shapes -o shapes.o
grid: grid.scm
	csc -c -J grid.scm -unit grid -o grid.o
clean:
	rm -f *.o
	rm -f out
	rm -f result
	rm -f *.link
	rm -f *.import.scm
	rm -f *.c
