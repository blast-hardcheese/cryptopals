tags:
	find src test -type f -name '*.hs' | xargs hothasktags > tags

S01C01:
	cabal exec -- runhaskell -isrc:src test/S01C01Spec.hs

S01C02:
	cabal exec -- runhaskell -isrc:src test/S01C02Spec.hs

S01C03:
	cabal exec -- runhaskell -isrc:src test/S01C03Spec.hs

S01C04:
	cabal exec -- runhaskell -isrc:src test/S01C04Spec.hs
