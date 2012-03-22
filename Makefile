all: bibdb
bibdb: bibdb.pl
	gplc --no-susp-warn --no-redef-error --no-singl-warn --no-debugger bibdb.pl
	# --no-top-level 
