compile shapes grid:
	csc -o out shapes.o -uses shapes main.scm
shapes:
	csc -c -J shapes.scm -unit shapes -o shapes.o
grid:
	csc -c -J grid.scm -unit grid -o grid.o
clean:
	rm -f *.o
	rm -f out
	rm -f result
	rm -f *.link
	rm -f *.import.scm
