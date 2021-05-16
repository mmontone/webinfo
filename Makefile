webinfo:
	sbcl --load cli.lisp
install:
	cp webinfo /usr/local/bin
clean:
	rm -f webinfo
