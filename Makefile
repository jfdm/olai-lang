# -- [ Makefile ]
#
# Makefile for the project.
#
# Copyright : (c) Jan de Muijnck-Hughes
# License   : see LICENSE
#
# -- [ EOH ]

PROJECT=olai
IDRIS2=idris2

TARGETDIR = ${CURDIR}/build/exec
TARGET = ${TARGETDIR}/${PROJECT}

# [ Core Project Definition ]

.PHONY: olai olai-test-build olai-test-run olai-test-run-re olai-test-update \
       # olai-bench

olai:
	$(IDRIS2) --build ${PROJECT}.ipkg

# To be activated once frontend is completed.

olai-test-build:
	${MAKE} -C tests testbin IDRIS2=$(IDRIS2)

olai-test-run: olai-test-build
	${MAKE} -C tests test \
			 IDRIS2=$(IDRIS2) \
			 PROG_BIN=$(TARGET) \
			 UPDATE='' \
			 ONLY=$(ONLY)

olai-test-run-re: olai-test-build
	${MAKE} -C tests test-re \
			 IDRIS2=$(IDRIS2) \
			 PROG_BIN=$(TARGET) \
			 ONLY=$(ONLY)

olai-test-update: olai-test-build
	${MAKE} -C tests test \
			 IDRIS2=$(IDRIS2) \
			 PROG_BIN=$(TARGET) \
			 THREADS=1 \
			 ONLY=$(ONLY)

olai-bench: olai olai-test-build
	${ECHO} "Todo"

#	$(HYPERFINE) --warmup 10 '${MAKE} olai-test-run'


# [ Housekeeping ]

.PHONY: clobber clean

clean:
	$(IDRIS2) --clean ${PROJECT}.ipkg
	${MAKE} -C tests clean

clobber: clean
	$(IDRIS2) --clean ${PROJECT}.ipkg
	${MAKE} -C tests clobber
	${RM} -rf build/

# -- [ EOF ]
