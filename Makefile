.PHONY: run watch compile clean

run:
	runghc --ghc-arg=-Wall Day$(day).hs < inputs/day$(day).txt

watch:
	ls **/*.hs inputs/*.txt | entr -s "make run day=$(day)"

watch-compile:
	ls **/*.hs inputs/*.txt | entr -s "make compile day=$(day)"

compile:
	ghc -dynamic -threaded -rtsopts -O2 -Wall -main-is Day$(day) Day$(day).hs -o run
	./run < inputs/day$(day).txt
	make clean

clean:
	rm -f run Data/*.hi Data/*.o *.hi *.o
