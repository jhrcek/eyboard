.PHONY: deploy
deploy:
	elm make src/Main.elm --output docs/index.html --optimize

.PHONY: elm-live
elm-live:
	elm-live --open --start-page docs/index.html -- src/Main.elm --output docs/index.html --debug

