%#!/usr/bin/gprolog
% --------------------------------------------
% CLI for interacting with a bibtex db.
% Enter your bibtex file to .bib.db
% i.e cat wang.bib > .bib.db
% --------------------------------------------
awrite([L|Ls]) :- print(L), nl, awrite(Ls).
awrite([]).

i(K,V, bibentry(Type, Key, Entries)):-
  bibentry(Type, Key, Entries),
  member(i(K,V), Entries).

portray(bibentry(Type, Key, Entries)):- bwrite(bibentry(Type, Key, Entries)).

bwrite(bibentry(Type,Key,Entries)):-
  c_blue(Type), write(' '), c_yellow(Key), nl,
  eswrite(Entries), nl.

eswrite([E|Es]):- ewrite(E), nl, eswrite(Es).
eswrite([]).

ewrite(i(K,V)):-
  write('  '),format('~15a', [K]), c_blue(':'), write(V).

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

read_txt(FName, Txt) :-
  open(FName, read, File),
  read_file(File, Txt),
  close(File).

put_all([]).
put_all([X|Xs]) :-
  put_code(X),
  put_all(Xs).

read_file(Stream, []) :- at_end_of_stream(Stream).
read_file(Stream, [X|L]) :-
  \+ at_end_of_stream(Stream),
  get_code(Stream, X),
  read_file(Stream, L).

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

% ============================================================

any_case(XX) --> [X], { char_code(C, X), lower_upper(CC, C), char_code(CC, XX)}. 

i_([])     --> []. 
i_([C|Cs]) --> any_case(C), i_(Cs).

parse_txt([]).
parse_txt(Txt) :- phrase(bibs(Bibs), Txt).

bibs([Bib | Bibs]) --> s_, bib(Bib), s_, bibs(Bibs).
bibs([]) --> [].

bib(Bib) --> ( bib_comment(Bib) ; bib_preamble(Bib) ; bib_string(Bib) ; bib_entry(Bib) ), {asserta(Bib)}.

bib_entry(bibentry(Type, Name, Keys)) -->
  "@", bib_type(Type),
  "{", s_, bib_name(Name), s_, ",", {!}, s_, bib_keys(Keys), s_, "}", s_.

bib_comment(bib(comment,comment, [i('key', Val)])) -->
  "@", i_("comment"), bib_braces(Val), s_.

bib_preamble(bib(preamble,preamble, [i('key', Val)])) -->
  "@", i_("preamble"), bib_braces(Val), s_.

bib_string(bib(string, K, [V])) -->
  "@", i_("string"),
  ("{", s_, bib_keys([i(K,V)]), s_, "}" ; "{", s_, bib_keys([i(K,V)]), s_, "}"), s_.

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

bib_value(Val) -->(bib_braces(V1) ; bib_quotes(V1) ; bib_word(V1) ),
  s_, "#", s_, bib_value(V2), {[V1,V2] = Val}. % TODO, fetch from string db
bib_value(Val) --> bib_word(Val).
bib_value(Val) --> bib_braces(Val).
bib_value(Val) --> bib_quotes(Val).

bib_word(Val) --> wordanum(ValC), {atom_codes(Val, ValC)}.
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


%--------------------------------------------------------
% Some colour
%--------------------------------------------------------
c_red(Str):- write('0;31m'), write(Str),write('[0m').
c_green(Str):- write('[0;32m'), write(Str),write('[0m').
c_yellow(Str):- write('[0;33m'), write(Str),write('[0m').
c_byellow(Str):- write('[1;33m'), write(Str),write('[0m').
c_blue(Str):- write('[0;34m'), write(Str),write('[0m').
c_magenta(Str):- write('[0;35m'), write(Str),write('[0m').
c_cyan(Str):- write('[0;36m'), write(Str),write('[0m').
c_white(Str):- write('[0;37m'), write(Str),write('[0m').

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
% Commands
%--------------------------------------------------------

parse_line(Line, Cmd) :- 
  phrase(read_command(Cmd), Line) ; Cmd = unknown.

read_kv(pair(Key, Value)) -->
  alphanum(KeyC), s_, ":", s_, alphanum(ValueC), {atom_codes(Key, KeyC), atom_codes(Value, ValueC)}.

read_command(exit) --> "exit".
read_command(no) --> s_.

read_command(or(P1, P2)) -->
  s_, read_expr(P1), s_, ";", s_, read_command(P2), s_.

read_command(and(P1, P2)) -->
  s_, read_expr(P1), s_, ",", s_, read_command(P2), s_.

read_command(P) --> s_, read_expr(P), s_.

read_command(no) --> [].

read_expr(Pair) --> read_kv(Pair).
read_expr(E) --> "(", read_command(E), ")".


%--------------------------------------------------------

process_cmd(unknown) :-
  write('*'), nl.

process_cmd(E) :-
  bagof(Res, process_expr(E, Res), ResL),
  awrite(ResL), nl.

process_expr(pair(K,V), Res) :- 
  i(K,V, Res).

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
  ( Cmd = exit, halt ;
    Cmd = no, do_command ;
    process_cmd(Cmd),
    do_command
  ).


main :-
  load_bib('.bib.db'),
  do_command.

:- dynamic([bibentry/3]).
:- initialization(main).


