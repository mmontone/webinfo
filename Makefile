all: webinfo makewebinfo

webinfo:
	sbcl --load cli.lisp

makewebinfo:
	sbcl --load makewebinfo.lisp

install:
	cp -f webinfo /usr/local/bin
	cp -f makewebinfo /usr/local/bin

clean:
	rm -f webinfo
	rm -f makewebinfo
