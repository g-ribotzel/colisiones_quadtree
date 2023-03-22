SRC=main.hs
all:
	stack exec -- ghc -O3 -dynamic -threaded -eventlog -rtsopts ${SRC} -o prog

