build:
	elm-make Elementary.elm --output index.html

clean:
	rm -rf elm-stuff
	make build
