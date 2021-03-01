.PHONY: static/tailwind.css
static/tailwind.css:
	mkdir -p static
	curl -L https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css > $@
