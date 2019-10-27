all:
	mosmlc -liberal -toplevel monoid.sml foldable.sml interpose.sml map.sml pretty.sml internal.sml

run: all
	./a.out

.PHONY: all run
