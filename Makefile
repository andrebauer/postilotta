all:
	$(MAKE) postilotta

unikernel-all:
	cd src && \
	mirage configure -t unix --net socket && \
	make depend && \
	make

build-unikernel:
	cd src && \
	make

run-unikernel:
	sudo ./src/postilotta -l "*:debug"


build:
	topkg build

test:
	# ocaml pkg/pkg.ml build -n postilotta -q --tests true && \
	# ocaml pkg/pkg.ml test -n postilotta
	topkg test


clean:
	# ocaml pkg/pkg.ml clean -n postilotta
	topkg clean


.phony: test
