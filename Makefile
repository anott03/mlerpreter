main: *.sml main.cm
	./bin/compile-cm -o a.out main.cm

clean:
	rm -f *.ui *.uo
