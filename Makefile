OPTS = -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d

build: hakyll
	./hakyll build

preivew: hakyll
	./hakyll preview

hakyll:
	ghc $(OPTS) --make hakyll.hs -o hakyll

clean: hakyll
	./hakyll clean
