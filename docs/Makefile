.PHONY: all clean

all: webinfo.html index.html

webinfo.html:
	texi2any --no-split --html --css-ref texinfo-klare.css webinfo.texi -o webinfo.html

index.html:
	texi2any --no-split --html --css-ref texinfo-klare.css webinfo.texi -o index.html

clean:
	rm -f *.pdf
	rm -f *.html
