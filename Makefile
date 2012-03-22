all: bibdb
bibdb: bibdb.pl
	gplc --no-susp-warn --no-redef-error --no-singl-warn bibdb.pl --no-top-level --no-debugger 
