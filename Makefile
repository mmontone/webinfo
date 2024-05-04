all: webinfo makewebinfo

webinfo:
	sbcl --load webinfo.asd --load cli.lisp

makewebinfo:
	sbcl --load webinfo.asd --load makewebinfo.lisp

install:
	cp -f webinfo /usr/local/bin
	cp -f makewebinfo /usr/local/bin

clean:
	rm -f webinfo
	rm -f makewebinfo
