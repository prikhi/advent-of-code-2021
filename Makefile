.PHONY: run watch

run:
	runghc --ghc-arg=-Wall Day$(day).hs < inputs/day$(day).txt

watch:
	ls **/*.hs inputs/*.txt | entr -s "make run day=$(day)"
