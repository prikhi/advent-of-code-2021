.PHONY: run watch compile watch-compile new-day clean

run:
	runghc --ghc-arg=-Wall Day$(day).hs < inputs/day$(day).txt

watch:
	ls **/*.hs inputs/*.txt | entr -s "make run day=$(day)"

watch-compile:
	ls **/*.hs inputs/*.txt | entr -s "make compile day=$(day)"

compile:
	ghc -dynamic -threaded -rtsopts -O2 -Wall -main-is Day$(day) Day$(day).hs -odir build -hidir build -o run
	./run < inputs/day$(day).txt
	rm -f run

new-day:
	./mk-day.sh "$(day)"

clean:
	rm -f run build
