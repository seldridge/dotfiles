.PHONY: stow unstow

stow: $(HOME)/usr
	stow -v -t $(HOME) home

unstow:
	stow -D -v -t $(HOME) home

$(HOME)/usr:
	mkdir $@
