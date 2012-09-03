all:
	ghc --make hakyll.hs
	./hakyll clean
	./hakyll rebuild
