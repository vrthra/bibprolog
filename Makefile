all: bib2pl
bib2pl: bib2pl.pl
	gplc --no-susp-warn --no-redef-error --no-singl-warn --no-debugger bib2pl.pl
	# --no-top-level 
