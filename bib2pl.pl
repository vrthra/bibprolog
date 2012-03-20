:- initialization(main).

main :-
  argument_value(1, File),
  start(File).

start(File) :-
  read_txt(File, Txt),
  parse_txt(Txt),
  nl, halt.

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
  0'0 =< Ch , Ch =< 0'9.
notBrace(Ch) :-
  Ch =\= "{", Ch =\= "}".

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
  [Char], {isalpha(Char); isdigit(Char)},
  {L1 = [Char|L0] },
  (alphanum(L1, L) ; {reverse(L1, L) }).

spaces1 --> (" ";"\n";"\t"), spaces.
spaces --> (spaces1 ; "").

not_braces(L) --> not_braces([], L).
not_braces(L0, L) -->
  [Char], { notBrace(Char) },
  {L1 = [Char|L0] },
  (not_braces(L1, L) ; {reverse(L1, L)}).

% ============================================================
%parsetxt(Txt) :- phrase(bib(Entry), Txt), write(Entry), nl.

parse_txt([]).
parse_txt(Txt) :- phrase(bib(Bib), Txt).
    %, write(Bib). %, put_all(Bib).

bib(Bib) -->
  "@", bib_type(Bib),
  "{", spaces, bib_name(Name), spaces, ",", spaces,
  bib_keys(Keys), spaces, "}","\n".
bib_type(Type) -->
  alphanum(TypeC), {atom_codes(Type, TypeC),
  write('Type : '), write(Type), nl}.
bib_name(Name) -->
  alphanum(NameC), {atom_codes(Name, NameC),
  write('Name : '), write(Name), nl}.
bib_keys(KVP) -->
  ((bib_kv(KVP), spaces) -> bib_keys(Rest) ; "").
bib_kv(Key) -->
  bib_key(Key), spaces, "=", spaces, bib_value(Val), spaces, ("," ; ""),
  {write('KV : '), write(Key), write('='), write(Val), nl}.

bib_key(Key) --> alphanum(Key1), {atom_codes(Key, Key1)}.
bib_value(Val) --> (bib_word(Val) ; bib_braces(Val)).

bib_word(Val) --> alphanum(Val1), {atom_codes(Val, Val1)}.
bib_braces(Val) --> "{", not_braces(Val1), "}", {atom_codes(Val, Val1)}.

