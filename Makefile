all:
	$(MAKE) postilotta

build:
	cd src && \
	mirage configure -t unix --net socket && \
	make depend && \
	make && \
	sudo ./postilotta -l "*:debug"


test:
	ocaml pkg/pkg.ml build -n postilotta -q --tests true && \
	ocaml pkg/pkg.ml test -n postilotta


clean:
	ocaml pkg/pkg.ml clean -n postilotta
