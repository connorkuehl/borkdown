all: Markerel.hs
	@ghc $^

test:
	@runghc *Test.hs

.PHONY: clean

clean:
	@rm -f *.hi *.o Markerel
