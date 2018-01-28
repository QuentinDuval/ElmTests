build: format
	elm-make --yes Elementary.elm --output index.html

clean:
	rm -rf elm-stuff
	make build

format:
	elm-format --yes .
