.PHONY: all
all:
	@echo 'Try `cargo build` instead.'

DOC = doc
BOOK = book

.PHONY: book
book:
	cd doc/en-US && mdbook build
	cd doc/ko-KR && mdbook build
	rm -rf $(BOOK)
	mkdir -p $(BOOK)
	git -C $(BOOK) init
	git -C $(BOOK) checkout --orphan gh-pages
	touch $(BOOK)/.nojekyll
	cp $(DOC)/index.html $(BOOK)/
	cp -R $(DOC)/en-US/book $(BOOK)/en-US
	cp -R $(DOC)/ko-KR/book $(BOOK)/ko-KR
	git -C $(BOOK) add .
	git -C $(BOOK) commit -m 'updated docs.'
	git -C $(BOOK) push -f "$$(git remote get-url upstream)" gh-pages -f

