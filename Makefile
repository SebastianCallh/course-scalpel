.PHONY: ghcid
ghcid:
	nix-shell --run 'ghcid --command "cabal new-repl course-scalpel --ghc-options=-Wall" --test "Hlint.run"'

ghcid-test:
	nix-shell --run 'ghcid --command "cabal new-repl course-scalpel-test --ghc-options=-Wall" --test "Hlint.run"'

ghcid-cli:
	nix-shell --run 'ghcid --command "cabal new-repl course-scalpel-cli --ghc-options=-Wall" --test "Hlint.run"'

