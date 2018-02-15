# Disable implicit rules
.SUFFIXES:

BUILD_DIR=build

# Find Haskell source files, but exclude generated sources. If these already
# exist, we don't want to add them to the dependencies list!
HS_SRC=$(shell find ioc-analyzer -iname '*.hs' -or -iname '*.x' | \
	fgrep -v .stack-work | \
	 grep -v /Query.*.hs | \
	fgrep -v /Lexer.hs | \
	fgrep -v /ExpParser.hs)

# Generated source files
HS_GENERATED_SRC=\
	ioc-analyzer/FieldInfo.hs

ifneq ($(NO_STACK),)
HS_GENERATED_SRC+=\
	ioc-analyzer/Lexer.hs \
	ioc-analyzer/ExpParser.hs
endif

GHC_FLAGS=-fno-warn-tabs -fwarn-unused-binds
STACK_BUILD_FLAGS=--ghc-options '$(GHC_FLAGS)'

# When you type "make", these things will be built.
ARTIFACTS=\
	$(BUILD_DIR)/symbolic/env.rkt \
	$(BUILD_DIR)/symbolic/dbcore.rkt \
	$(BUILD_DIR)/symbolic/arith.rkt \
	$(BUILD_DIR)/symbolic/tracing.rkt \
	$(BUILD_DIR)/symbolic/shell.rkt \
	$(BUILD_DIR)/symbolic/ishell.rkt \
	$(BUILD_DIR)/symbolic/explanations.rkt \
	$(BUILD_DIR)/symbolic/model-completion.rkt \
	$(BUILD_DIR)/bin/neutrons \
	$(BUILD_DIR)/bin/setup-symbolic \
	$(BUILD_DIR)/bin/run-symbolic \
	$(BUILD_DIR)/bin/check-symbolic \

###############################################################################

.PHONY: all artifacts dist clean clean-generated-hs

# This rule looks weird, but it ensures that check.ok is built before any
# artifact, even during parallel builds.
all: check.ok
	$(MAKE) artifacts

check.ok: check.sh
	./$<
	touch $@

artifacts: $(ARTIFACTS)

# Create .tar.gz distribution
dist: scripts/dist.sh $(ARTIFACTS) doc/symbolic-interpreter.pdf
	./scripts/dist.sh

vars.sh: scripts/build-epics.sh scripts/epics-support-makeReleaseConsistent.patch
	./$<

clean: clean-generated-hs
	$(RM) -r $(BUILD_DIR)
	$(RM) -r symbolic-evaluator/compiled
	$(RM) check.ok
	$(RM) -r ioc-analyzer/.stack-work
	find ioc-analyzer \( -iname '*.o' -or -iname '*.hi' \) -print -delete
	$(RM) -r epics
	$(RM) vars.sh

clean-generated-hs:
	$(RM) $(HS_GENERATED_SRC)
	$(RM) $(HS_GENERATED_SRC:.hs=.o) $(HS_GENERATED_SRC:.hs=.hi)

###############################################################################

$(BUILD_DIR):
	mkdir -p $@

$(BUILD_DIR)/bin: | $(BUILD_DIR)
	mkdir -p $@

$(BUILD_DIR)/symbolic: | $(BUILD_DIR)
	mkdir -p $@

$(BUILD_DIR)/bin/neutrons: ioc-analyzer/Main | $(BUILD_DIR)/bin
	cp $< $@

$(BUILD_DIR)/bin/setup-symbolic: symbolic-evaluator/setup | $(BUILD_DIR)/bin
	cp $< $@
	chmod +x $@

$(BUILD_DIR)/bin/run-symbolic: symbolic-evaluator/run | $(BUILD_DIR)/bin
	cp $< $@
	chmod +x $@

$(BUILD_DIR)/bin/check-symbolic: symbolic-evaluator/check | $(BUILD_DIR)/bin
	cp $< $@
	chmod +x $@

$(BUILD_DIR)/symbolic/%.rkt: symbolic-evaluator/%.rkt | $(BUILD_DIR)/symbolic
	cp $< $@

###############################################################################

PDFLATEX_FLAGS=-interaction batchmode

.PHONY: doc
doc: doc/symbolic-interpreter.pdf

doc/%.pdf: doc/%.tex
	cd doc && latexmk -pdf -pdflatex='pdflatex $(PDFLATEX_FLAGS)' $$(basename '$<')

doc/symbolic-interpreter.pdf: doc/codestyle.tex

###############################################################################

ioc-analyzer/FieldInfo.hs: scripts/dbd2hs.py vars.sh
	. vars.sh && ./$< "$$EPICS_BASE/dbd" "$$SUPPORT/"{asyn-4-13,calc-2-8}'/dbd' >'$@' || ( RV=$$?; rm -f '$@'; exit $$RV )

ioc-analyzer/stack-setup.ok: ioc-analyzer/ioc-analyzer.cabal ioc-analyzer/stack.yaml
	cd ioc-analyzer && stack setup
	touch '$@'

ifeq ($(NO_STACK),)
ioc-analyzer/Main: ioc-analyzer/stack-setup.ok $(HS_SRC) $(HS_GENERATED_SRC)
	cd ioc-analyzer && stack build $(STACK_BUILD_FLAGS)
	ln -f `find ioc-analyzer/.stack-work -type f -perm +111 -path */bin/Main | head -n1` '$@'
else
%.hs: %.x
	alex $<

ioc-analyzer/Main: $(HS_SRC) $(HS_GENERATED_SRC)
	cd ioc-analyzer && ghc --make Main.hs $(GHC_FLAGS) -o $$(basename '$@')
endif
