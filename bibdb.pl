%#!/usr/bin/prolog
% --------------------------------------------
% CLI for interacting with a bibtex db.
% Enter your bibtex file to .bib.db
% i.e cat wang.bib > .db.bib
% --------------------------------------------

% address: Publisher's address (usually just the city, but can be the full address for lesser-known publishers)
% annote: An annotation for annotated bibliography styles (not typical)
% author: The name(s) of the author(s) (in the case of more than one author, separated by and)
% booktitle: The title of the book, if only part of it is being cited
% chapter: The chapter number
% crossref: The key of the cross-referenced entry
% edition: The edition of a book, long form (such as "first" or "second")
% editor: The name(s) of the editor(s)
% eprint: A specification of an electronic publication, often a preprint or a technical report
% howpublished: How it was published, if the publishing method is nonstandard
% institution: The institution that was involved in the publishing, but not necessarily the publisher
% journal: The journal or magazine the work was published in
% key: A hidden field used for specifying or overriding the alphabetical order of entries (when the "author" and "editor" fields are missing). Note that this is very different from the key (mentioned just after this list) that is used to cite or cross-reference the entry.
% month: The month of publication (or, if unpublished, the month of creation)
% note: Miscellaneous extra information
% number: The "(issue) number" of a journal, magazine, or tech-report, if applicable. (Most publications have a "volume", but no "number" field.)
% organization: The conference sponsor
% pages: Page numbers, separated either by commas or double-hyphens.
% publisher: The publisher's name
% school: The school where the thesis was written
% series: The series of books the book was published in (e.g. "The Hardy Boys" or "Lecture Notes in Computer Science")
% title: The title of the work
% type: The field overriding the default type of publication (e.g. "Research Note" for techreport, "{PhD} dissertation" for phdthesis, "Section" for inbook/incollection)
% url: The WWW address
% volume: The volume of a journal or multi-volume book
% year: The year of publication (or, if unpublished, the year of creation)
%
% article
%   An article from a journal or magazine.
%   Required fields: author, title, journal, year
%   Optional fields: volume, number, pages, month, note, key
% book
%   A book with an explicit publisher.
%   Required fields: author/editor, title, publisher, year
%   Optional fields: volume/number, series, address, edition, month, note, key
% booklet
%   A work that is printed and bound, but without a named publisher or sponsoring institution.
%   Required fields: title
%   Optional fields: author, howpublished, address, month, year, note, key
% conference
%   The same as inproceedings, included for Scribe compatibility.
%   inbook
%   A part of a book, usually untitled. May be a chapter (or section or whatever) and/or a range of pages.
%   Required fields: author/editor, title, chapter/pages, publisher, year
%   Optional fields: volume/number, series, type, address, edition, month, note, key
% incollection
%   A part of a book having its own title.
%   Required fields: author, title, booktitle, publisher, year
%   Optional fields: editor, volume/number, series, type, chapter, pages, address, edition, month, note, key
% inproceedings
%   An article in a conference proceedings.
%   Required fields: author, title, booktitle, year
%   Optional fields: editor, volume/number, series, pages, address, month, organization, publisher, note, key
% manual
%   Technical documentation.
%   Required fields: title
%   Optional fields: author, organization, address, edition, month, year, note, key
% mastersthesis
%   A Master's thesis.
%   Required fields: author, title, school, year
%   Optional fields: type, address, month, note, key
% misc
%   For use when nothing else fits.
%   Required fields: none
%   Optional fields: author, title, howpublished, month, year, note, key
% phdthesis
%   A Ph.D. thesis.
%   Required fields: author, title, school, year
%   Optional fields: type, address, month, note, key
% proceedings
%   The proceedings of a conference.
%   Required fields: title, year
%   Optional fields: editor, volume/number, series, address, month, publisher, organization, note, key
% techreport
%   A report published by a school or other institution, usually numbered within a series.
%   Required fields: author, title, institution, year
%   Optional fields: type, number, address, month, note, key
% unpublished
%   A document having an author and title, but not formally published.
%   Required fields: author, title, note
%   Optional fields: month, year, key

i(K,V, bibentry(Type, Key, Entries)):-
  bibentry(Type, Key, Entries),
  member(i(K,V), Entries).

read_txt(FName, Txt) :-
  open(FName, read, File),
  read_file(File, Txt),
  close(File).

read_file(Stream, []) :- at_end_of_stream(Stream).
read_file(Stream, [X|L]) :-
  \+ at_end_of_stream(Stream),
  get_code(Stream, X),
  read_file(Stream, L).

% ============================================================
%  DCG support.
% ============================================================
isalpha(Ch) :-
  Ch >= 0'a, Ch =< 0'z -> true ; Ch >= 0'A, Ch =< 0'Z -> true.
isdigit(Ch) :-
  0'0 =< Ch, Ch =< 0'9.
ispunct(Ch) :-
  Ch >= 0'!, Ch =< 0'@ -> true ; Ch >= 0'{, Ch =< 0'~ -> true.
islchar(Ch) :-
  Ch = 0'_ ; Ch = 0'. ; Ch = 0'- ; Ch = 0'+ .

not_brace(Ch) :-
  Ch =\= 0'{, Ch =\= 0'}.
not_quote(Ch) :-
  Ch =\= 0'".

tilleol(L) --> tilleol([], L).
tilleol(L0, L) -->
  [Char], { Char =\= 13 ; Char =\= 10 },
  { L1 = [Char|L0] },
  (tilleol(L1, L) ; {reverse(L1, L)}).

% TODO : Collapse all the three into a single function
digits(N) --> digits(0, N).
digits(N0, N) --> % N0 is number already read
  [Char], {isdigit(Char)},
  { N1 is N0 * 10 + (Char - 0'0) },
  ( digits(N1, N) ; { N = N1 }).

letters(L) --> letters([], L).
letters(L0, L) -->
  [Char], { isalpha(Char) },
  {L1 = [Char|L0] },
  (letters(L1, L) ; {reverse(L1, L)}).

alphanum(L) --> alphanum([], L).
alphanum(L0, L) --> 
  [Char], {isalpha(Char) ; isdigit(Char)},
  {L1 = [Char|L0] },
  (alphanum(L1, L) ; {reverse(L1, L) }).

alphanum_a(L) --> alphanum(LC),{atom_codes(L, LC)}.

punctanum(L) --> punctanum([], L).
punctanum(L0, L) --> 
  [Char], {isalpha(Char) ; isdigit(Char) ; ispunct(Char)},
  {L1 = [Char|L0] },
  (punctanum(L1, L) ; {reverse(L1, L) }).

wordanum(L) --> wordanum([], L).
wordanum(L0, L) --> 
  [Char], {isalpha(Char) ; isdigit(Char) ; islchar(Char)},
  {L1 = [Char|L0] },
  (wordanum(L1, L) ; {reverse(L1, L) }).

% Just consume spaces
s1_ --> (" ";"\n" ; "\t"), s_.
s_ --> s1_ ; "".

any_case(XX) --> [X], { char_code(C, X), lower_upper(CC, C), char_code(CC, XX)}. 

i_([])     --> []. 
i_([C|Cs]) --> any_case(C), i_(Cs).

% ============================================================
%  Now the bibtex parser.
% ============================================================

parse_txt([]).
parse_txt(Txt) :- phrase(bibs(Bibs), Txt), apply(Bibs).

apply([Bib|Bibs]) :- asserta(Bib), apply(Bibs).
apply([]).

bibs([Bib | Bibs]) --> s_, bib(Bib), s_, bibs(Bibs).
bibs([]) --> [].

bib(Bib) --> ( bib_comment(Bib) ; bib_preamble(Bib) ; bib_string(Bib) ; bib_entry(Bib) ).

bib_entry(bibentry(Type, Name, Keys)) -->
  "@", bib_type(Type),
  "{", s_, bib_name(Name), s_, ",", {!}, s_, bib_keys(Keys), s_, "}", s_.

bib_comment(bibentry(comment,comment, [i(key,Val)])) -->
  "@", i_("comment"), bib_braces(Val), s_.

bib_preamble(bibentry(preamble,preamble, [i(key,Val)])) -->
  "@", i_("preamble"), bib_braces(Val), s_.

bib_string(bibentry(string, K, [i(key,K),i(val,V)])) -->
  "@", i_("string"),
  ("{", s_, bib_keys([i(K,V)]), s_, "}" ; "(", s_, bib_keys([i(K,V)]), s_, ")"), s_.

bib_type(Type) -->
  wordanum(TypeC), {atom_codes(Type, TypeC)}.

bib_name(Name) -->
  wordanum(NameC), {atom_codes(Name, NameC)}.
bib_keys([Pair | Rest]) --> bib_kv(Pair), s_, bib_keys(Rest).
bib_keys([]) --> [].

bib_kv(i(Key,Val)) -->
  bib_key(Key), s_, "=", s_, bib_value(Val), s_, ((",",{!}) ; "").
  % we have a !cut here so that once a pair is parsed, we should not go back
  % there are no possible circumstances in bibtex which would require it.

bib_key(Key) --> wordanum(KeyC), {atom_codes(Key, KeyC)}.

bib_value(Val) -->(bib_braces(V1) ; bib_quotes(V1) ; bib_word(V1)),
  s_, "#", s_, bib_value(V2),
  {list(V2) -> Val = [V1|V2] ; [V1,V2] = Val}. % TODO, fetch from string db
bib_value(Val) --> bib_word(Val).
bib_value(Val) --> bib_braces(Val).
bib_value(Val) --> bib_quotes(Val).

bib_word(word(Val)) --> wordanum(ValC), {atom_codes(Val, ValC)}.
bib_braces(Val) --> parse_brace(ValC), {atom_codes(Val, ValC)}.
bib_quotes(Val) --> parse_quote(ValC), {atom_codes(Val, ValC)}.

% Use the below if the string parens are required (to know what kind of string it was)
% parse_brace(Val) --> "{", parse_bstring(ValS), "}", {append( [0'{| ValS], "}", Val)}.

parse_brace(Val) --> "{", parse_bstring(Val), "}".

parse_bstring([0'\\| [Char|Val]] ) --> "\\", [Char], parse_bstring(Val).
parse_bstring([Char|Val]) --> [Char], {not_brace(Char)}, parse_bstring(Val).
parse_bstring(Val) --> parse_brace(ValA), parse_bstring(ValB), {append(ValA, ValB, Val)}.
parse_bstring([]) --> [].

% Use the below if the string parens are required (to know what kind of string it was)
% parse_quote(Val) --> "\"", parse_qstring(ValS), "\"", {append( [0'"| ValS], "\"", Val)}.

parse_quote(Val) --> "\"", parse_qstring(Val), "\"".

% Warning escaped Quote
parse_qstring([0'\\| [Char|Val]] ) --> "\\", [Char], parse_qstring(Val).
parse_qstring(Val) --> parse_brace(V), parse_qstring(U), {append(V, U, Val)}.
parse_qstring([Char|Val]) --> [Char], {not_quote(Char)}, parse_qstring(Val).
parse_qstring([]) --> [].

color(red, '[0;31m').
color(green, '[0;32m').
color(yellow, '[0;33m').
color(blue, '[0;34m').
color(magenta, '[0;35m').
color(cyan, '[0;36m').
color(white, '[0;37m').

color(bred, '[1;31m').
color(bgreen, '[1;32m').
color(byellow, '[1;33m').
color(bblue, '[1;34m').
color(bmagenta, '[1;35m').
color(bcyan, '[1;36m').
color(bwhite, '[1;37m').
color(_, '[0;37m').

color(end, '[0m').

%--------------------------------------------------------
% Some colour
%--------------------------------------------------------
c_show(C,Str) :- color(C,Col), color(end,End), format("~w~w~w", [Col,Str,End]).
c_start(C) :- color(C,Col), write(Col).
c_end :- write('[0m').

%--------------------------------------------------------
% Load bibtex files
%--------------------------------------------------------

load_bib(File) :-
  read_txt(File, Txt),
  parse_txt(Txt).

%--------------------------------------------------------
read_line(Codes) :-
    get0(Code),
    (   Code < 0 /* end of file */ -> Codes = "exit"
    ;   Code =:= 10 /* end of line */ -> Codes = []
    ;   Codes = [Code|Codes1],
        read_line(Codes1)
    ).

%--------------------------------------------------------
% Read User Commands
%--------------------------------------------------------

parse_line(Line, Cmd) :- 
  phrase(read_command(Cmd), Line) ; Cmd = unknown.

read_kv(pair(Key, Value)) --> 
  alphanum_a(Key), s_, ":", s_, alphanum_a(Value).

read_kv(pair(Key, Value)) --> 
  alphanum_a(Key), s_, ":", s_, {atom_codes(Value,"")}.

read_kv(has(Key, Value)) -->
  alphanum_a(Key), s_, "~", s_, alphanum_a(Value).

read_kv(has(Key, Value)) --> 
  alphanum_a(Key), s_, "~", s_, {atom_codes(Value,"")}.

read_kv(ne(Key, Value)) -->
  alphanum_a(Key), s_, "!", s_, alphanum_a(Value).

read_kv(gt(Key, Value)) -->
  alphanum_a(Key), s_, ">", s_, alphanum_a(Value).

read_kv(lt(Key, Value)) -->
  alphanum_a(Key), s_, "<", s_, alphanum_a(Value).

read_kv(ge(Key, Value)) -->
  alphanum_a(Key), s_, ">=", s_, alphanum_a(Value).

read_kv(le(Key, Value)) -->
  alphanum_a(Key), s_, "<=", s_, alphanum_a(Value).


read_command(exit) --> "exit".
read_command(help) --> "help".
read_command(show(V)) --> "show", s_, alphanum_a(V), s_.
read_command(colour(K,V)) --> ("colour" ; "color"), s_, alphanum_a(K), s_, alphanum_a(V), s_.
read_command(no) --> s_.

read_command(or(P1, P2)) -->
  s_, read_expr(P1), s_, ";", s_, read_command(P2), s_.

read_command(and(P1, P2)) -->
  s_, read_expr(P1), s_, ",", s_, read_command(P2), s_.

read_command(P) --> s_, read_expr(P), s_.


read_command(no) --> [].

read_expr(Pair) --> read_kv(Pair).
read_expr(E) --> "(", read_command(E), ")".
read_expr(all(V)) --> s_, "*", alphanum_a(V), s_.
read_expr(all('')) --> s_, "*", s_.

%--------------------------------------------------------
% Some libraries
%--------------------------------------------------------
awrite([L|Ls]) :- print(L), nl, awrite(Ls).
awrite([]).

portray(bibentry(comment,Key,[i(key, Entry)])) :-
  c_show(green,'>'), write(' '), c_show(green,Entry).

portray(bibentry(preamble,Key,[i(key,Key),i(val,Entry)])):-
  c_show(blue,'*>'), write(' '), c_show(yellow,Entry).

portray(bibentry(string,Key,[i(key,Key), i(val,Entry)])):-
  c_show(yellow,Key), write(' = '), print(Entry).

portray(bibentry(Type,Key,Entries)) :- opt(show,prolog),
  write('bibentry('), c_show(blue,Type), write(', '), c_show(yellow,Key),write(','), nl,
  write('['),nl,
  awrite(Entries),
  write(']'),nl,
  write(')'),
  nl.

portray(bibentry(Type,Key,Entries)) :- opt(show,bib),
  write('@'),c_show(blue, Type), write('{'), c_show(yellow, Key),write(','), nl,
  awrite(Entries),
  write('}'),
  nl.

portray(bibentry(Type,Key,Entries)) :- 
  c_show(blue,Type), write(' '), c_show(yellow,Key), nl,
  awrite(Entries), nl.

portray(i(K,V)) :- opt(show,prolog),
  write('  '),write('i('), print(key(K)), write(', '), wvals(V), write('),').

portray(i(K,V)) :- opt(show,bib),
  write('  '), print(key(K)), c_show(blue,'= '), write('{'), wvals(V), write('},').

portray(i(K,V)) :-
  write('  '), print(key(K)), c_show(blue,': '), wvals(V).

portray(key(K)) :-
  mycolour(K,V), c_start(V), (opt(show,prolog) -> write(K) ; format('~15a', [K])), c_end.
portray(key(K)) :-
  (opt(show,prolog) -> write(K) ; format('~15a', [K])).

portray(word(K)) :- opt(show,prolog), write('word('),print(K),write(')').

% String substitution happens here.
portray(word(K)) :-
  bibentry(string, K, [_,i(val,V)]), print(V).

wvals([V|Vs]) :- print(V), wvals(Vs).
wvals([]).
wvals(V) :- opt(show,prolog),write('\''), print(V), write('\'').
wvals(V) :- print(V).

ulcaseatom(L, U) :-
  atom_chars(L, Ls), 
  ulcase(Ls, Us),
  atom_chars(U, Us). % !!!! this is dependent on sequence.

ulcase([L|Ls], [U|Us]) :-
  lower_upper(L, U),
  ulcase(Ls, Us).
ulcase([], []).

%--------------------------------------------------------
find_num(K,V, N, V1, N1, Res) :- 
  atom_codes(V, V1),
  phrase(digits(N), V1),
  i(K,Vi, Res),
  atom_codes(Vi, Vi1),
  phrase(digits(N1), Vi1).

process_cmd(unknown) :-
  write('*'), nl.

process_cmd(help) :-
  write('COMMANDS'),nl,
  tab(1),write('help'),nl,
  tab(1),write('colour <key> <value>'),nl,
  tab(2),write('  e.g: colour title red'),nl,
  tab(1),write('*'),nl,
  tab(1),write('*<type>'),nl,
  tab(2),write('  e.g: *article'),nl,
  tab(1),write('*show <bib|prolog|format>'),nl,
  tab(2),write('  e.g: show bib'),nl.

process_cmd(show(V)) :-
  retractall(opt(show,_)),
  assertz(opt(show,V)).

process_cmd(colour(K,V)) :-
  assertz(mycolour(K,V)).


process_cmd(E) :-
  setof(Res, process_expr(E, Res), ResL),
  awrite(ResL), nl.

process_expr(all(V), Res) :-
  (V = '' ->
    (bibentry(X,Key,R), Res = bibentry(X, Key, R)) ;
    (bibentry(V,Key,R), Res = bibentry(V, Key, R)) ).

process_expr(pair(K,V), Res) :- 
  i(K,V, Res).

process_expr(gt(K,V), Res) :- 
  find_num(K,V,N,V1,N1, Res),
  N1 #># N, fd_domain([N,N1],1,10000).

process_expr(lt(K,V), Res) :- 
  find_num(K,V,N,V1,N1, Res),
  N1 #<# N, fd_domain([N,N1],1,10000).

process_expr(ge(K,V), Res) :- 
  find_num(K,V,N,V1,N1, Res),
  N1 #>=# N, fd_domain([N,N1],1,10000).

process_expr(le(K,V), Res) :- 
  find_num(K,V,N,V1,N1, Res),
  N1 #=<# N, fd_domain([N,N1],1,10000).

process_expr(ne(K,V), Res) :- 
  i(K,V1, Res), V \= V1.

process_expr(has(K,V), Res) :- 
  i(K,V1, Res), ulcaseatom(V1, V2), ulcaseatom(V, Vi), sub_atom(V2, _, _, _, Vi).

process_expr(or(E1,E2), Res) :-
  process_expr(E1, Res) ; process_expr(E2, Res).

process_expr(and(E1,E2), Res) :-
  process_expr(E1, Res), process_expr(E2, Res).

%--------------------------------------------------------
% Standard Incantation for a REPL
%--------------------------------------------------------

do_command :-
  write('bib> '),
  read_line(Line),!,
  parse_line(Line, Cmd),
  ( Cmd = exit; %halt ;
    Cmd = no, do_command ;
    process_cmd(Cmd),
    do_command
  ).


main :-
  load_bib('.db.bib'),
  do_command.

:- dynamic([bibentry/3, opt/2, mycolour/2]).
:- initialization(main).


