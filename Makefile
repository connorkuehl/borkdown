all: Markerel.hs
	@ghc $^

test:
	@runghc *Test.hs
