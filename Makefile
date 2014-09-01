tags:
	find src test -type f -name '*.hs' | xargs hothasktags > tags

S01C01:
	cabal exec -- runhaskell -isrc:src test/S01C01Spec.hs
