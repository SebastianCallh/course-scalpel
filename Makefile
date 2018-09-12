.PHONY: ghcid ghcid-test ghcid-cli
ghcid:
	nix-shell --run 'ghcid -c "cabal new-repl course-scalpel --ghc-options=-Wall" --test "Hlint.run"'

ghcid-test:
	nix-shell --run 'ghcid -c "cabal new-repl course-scalpel-test --ghc-options=-Wall" --test "Hlint.run"'

ghcid-cli:
	nix-shell --run 'ghcid -c "cabal new-repl course-scalpel-cli --ghc-options=-Wall" --test "Hlint.run"'

