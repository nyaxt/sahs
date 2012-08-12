test: sahs
	./sahs abracadabra

sahs: sahs.hs
	ghc sahs.hs
