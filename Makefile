all: bib2pl
bib2pl: bib2pl.pl
	gplc --no-susp-warn --no-redef-error --no-singl-warn --no-top-level --no-debugger bib2pl.pl
