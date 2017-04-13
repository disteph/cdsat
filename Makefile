SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install:: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall:: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean: rm_makefile_data
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure: rm_setup_data
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure rm_makefile_data configure_lib kernel_lib kernel portfolio psyche

-include Makefile.data

ifdef LIB
install reinstall:: TO_INSTALL =                         \
	$(ROOTDIR)/META \
	$(shell find _build/src -name open.cmx)  \
	$(shell find _build/src -name flags.cmx)  \
	$(shell find _build/src -name dump.cmx)
install reinstall::
	ocamlfind install -add $(LIB) $(TO_INSTALL)
endif

rm_makefile_data:
	rm Makefile.data || true
rm_setup_data:
	rm setup.data || true

configure_lib:
	$(file >Makefile.data,LIB = $(LIB))
	$(file >>Makefile.data,ROOTDIR = $(ROOTDIR))

kernel_lib: CONFIGUREFLAGS = --enable-kernel-lib
kernel_lib: LIB = psyche_kernel_lib
kernel_lib: ROOTDIR = src/lib

kernel: CONFIGUREFLAGS = --enable-kernel
kernel: LIB = psyche_kernel
kernel: ROOTDIR = src/kernel

portfolio: CONFIGUREFLAGS = --enable-portfolio
portfolio: LIB = psyche_portfolio
portfolio: ROOTDIR = src/portfolio

psyche: CONFIGUREFLAGS = --enable-psyche

kernel_lib kernel portfolio: configure_lib configure
psyche: rm_makefile_data configure
