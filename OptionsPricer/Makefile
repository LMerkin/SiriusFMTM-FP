HOPTS = -O3 -dynamic -threaded
SRCS  = Common.hs Contracts.hs Diffusions.hs Main.hs MonteCarlo.hs OptPricer.hs

OptPricerTest: $(SRCS)
	ghc $(HOPTS) -o $@ Main.hs

