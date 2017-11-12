SETUP = ocaml setup.ml

-include makefile.data

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean: rm_makefile.data
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

ifdef LIB

uninstall_base:
	ocamlfind remove $(LIB)

else
uninstall_base:
	$(SETUP) -uninstall $(UNINSTALLFLAGS)
endif

uninstall:
	make uninstall_base

reinstall: uninstall install

rm_makefile.data:
	rm makefile.data || true

rm_data: rm_makefile.data
	rm setup.data || true

configure_lib:
	$(file >makefile.data,LIB = $(LIB))
	$(file >>makefile.data,ROOTDIR = $(ROOTDIR))

kernel_lib: CONFIGUREFLAGS = --enable-kernel-lib
kernel_lib: LIB = psyche_kernel_lib
kernel_lib: ROOTDIR = src/lib

kernel: CONFIGUREFLAGS = --enable-kernel
kernel: LIB = psyche_kernel
kernel: ROOTDIR = src/kernel

portfolio: CONFIGUREFLAGS = --enable-portfolio
portfolio: LIB = psyche_portfolio
portfolio: ROOTDIR = src/portfolio

SAT_API: CONFIGUREFLAGS = --enable-sat-api
SAT_API: LIB = psyche_SAT_API
SAT_API: ROOTDIR = src/SAT_API

cdsat: CONFIGUREFLAGS = --enable-exec

kernel_lib kernel portfolio SAT_API: rm_data configure_lib setup.data

cdsat: rm_data setup.data

.PHONY: build doc test libs install reinstall uninstall_libs clean distclean rm_makefile.data rm_data configure_lib kernel_lib kernel portfolio SAT_API psyche

libs:
	make clean kernel_lib build reinstall
	make clean kernel build reinstall
	make clean portfolio build reinstall
	make clean SAT_API build reinstall

uninstall_libs:
	make clean SAT_API uninstall
	make clean portfolio uninstall
	make clean kernel uninstall
	make clean kernel_lib uninstall

