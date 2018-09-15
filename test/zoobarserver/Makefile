VS      := $(%.v)

.PHONY: coq clean

all: coq

coq: Makefile.coq
	$(MAKE) -f $<

Makefile.coq: Makefile $(VS)
	coq_makefile -f _CoqProject -o $@

clean:: Makefile.coq
	$(MAKE) -f $< clean
	rm -f $< Makefile.coq.conf

