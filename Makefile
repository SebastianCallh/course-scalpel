.PHONY: ghcid
ghcid:
	nix-shell --run 'ghcid --command "cabal new-repl course-scalpel --ghc-options=-Wall" --test "Hlint.run"'

