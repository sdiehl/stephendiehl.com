all:
	#ghc --make hakyll.hs
	./hakyll clean
	./hakyll rebuild

IN_DIAGRAMS=$(wildcard diagrams/*.tex)
SVG_DIAGRAMS=$(patsubst %.tex,%.svg,$(IN_DIAGRAMS))
PNG_DIAGRAMS=$(patsubst %.tex,%.png,$(IN_DIAGRAMS))

.PRECIOUS: diagrams/%.png diagrams/%.svg

diagrams/%.svg: diagrams/%.pdf
	pdf2svg $< $@

diagrams/%.pdf: diagrams/%.tex
	pdflatex -halt-on-error -output-directory=./diagrams $<

diagrams/%.png: diagrams/%.svg
	inkscape -f $< -e $@
	cp $< images/

touch:
	touch diagrams/*.tex

clean:
	-rm -f diagrams/*.aux
	-rm -f diagrams/*.log
	-rm -f diagrams/*.pdf
	-rm -f diagrams/*.toc
	-rm -f diagrams/*.svg
	-rm -f diagrams/*.png
