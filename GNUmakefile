### XXX we eventually want building of the manual to be part of Setup.lhs

# hardcode some bits
RUBBER=rubber

DARCS = dist/build/darcs/darcs
PREPROC=./dist/build/darcs/darcs --preprocess-manual
PREPROCHTML=--html
TEXSOURCES = src/darcs.tex $(wildcard src/*.tex) $(filter %.lhs,$(DARCS_FILES))
doc/manual/darcs.tex: $(TEXSOURCES) $(DARCS)
	mkdir -p doc/manual
	$(PREPROC) darcs.tex $(PREPROCHTML) >$@
doc/manual/darcs_print.tex: $(TEXSOURCES) $(DARCS)
	mkdir -p doc/manual
	$(PREPROC) darcs.tex >$@
doc/manual/patch-theory.tex: $(TEXSOURCES) $(UNIT_FILES) $(DARCS)
	mkdir -p doc/manual
	$(PREPROC) Darcs/Test/Patch/Properties.lhs >$@

%.pdf: %.tex
	cd $(<D) && $(RUBBER) --pdf $(<F)
%.ps: %.tex
	cd $(<D) && $(RUBBER) --ps $(<F)

ps pdf: %: doc/manual/darcs.% doc/manual/patch-theory.%
html: doc/manual/index.html
website: ps pdf html doc/manual/bigpage.html

### TODO use latex2html since bigpage seems to needslatex2html anyway;
###      we can restore hevea/tex4ht support when we move this to Setup.lhs
doc/manual/index.html: doc/manual/darcs.tex src/gpl.tex doc/darcs.css
	latex2html -long_titles 2 -split +1 -dir doc/manual doc/manual/darcs.tex
	cp -f doc/darcs.css doc/manual/darcs.css

doc/manual/bigpage.html: doc/manual/darcs.tex src/gpl.tex doc/darcs.css
	ln -sf darcs.tex doc/manual/bigpage.tex
	latex2html -split 0 -external_file darcs -prefix big \
		-no_auto_link -dir doc/manual doc/manual/bigpage.tex
	cp -f doc/darcs.css doc/manual/bigpage.css

doc/manual/darcs.ps: doc/manual/darcs_print.ps
	cp $< $@
doc/manual/darcs.pdf: doc/manual/darcs_print.pdf
	cp $< $@

# Good for tags.
DARCS_FILES = $(wildcard src/[A-Z]*.hs src/*/[A-Z]*.hs src/*/*/[A-Z]*.hs) \
	      $(wildcard src/[A-Z]*.lhs src/*/[A-Z]*.lhs src/*/*/[A-Z]*.lhs)

tags: $(DARCS_FILES) src/*.c
	hasktags -c $(filter %.lhs %.hs,$^)
	ctags -a $(filter %.c,$^)

# TAGS is for etags, whereas tags is for ctags
TAGS: $(DARCS_FILES) src/*.c
	hasktags -e $(filter %.lhs %.hs,$^)
	etags -a $(filter %.c,$^)

clean:
	rm -f TAGS tags
	rm -f doc/manual/bigpage.tex doc/manual/bigpage.css
	rm -f doc/manual/bigimg*.png doc/manual/bigimages.*
	rm -f doc/manual/*.html doc/manual/darcs*.??? doc/manual/darcs.lg
	rm -f doc/manual/darcs.html
	rm -f doc/manual/darcs.xref c_context.c doc/darcs_print.ps
	rm -f doc/manual/patch-theory.??? doc/manual/patch-theory.ps
	rm -f doc/manual/WARNINGS doc/manual/*.pl
	rm -f doc/manual/images.* doc/manual/img*.png doc/manual/*.html
	rm -f doc/manual/darcs_print.ps doc/manual/darcs.ps

.PHONY: ps pdf html clean website
