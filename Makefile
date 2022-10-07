build: main.hs
	ghc -o turing main.hs
	rm -rf **/*.o **/*.hi *.o *.hi

clean:
	rm -rf **/*.o **/*.hi *.o *.hi turing
