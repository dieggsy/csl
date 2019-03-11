all:
	chicken-install -n

install:
	chicken-install -s

clean:
	rm -vf *.so *.import.scm *.link *.static.o *.build.sh *.install.sh
