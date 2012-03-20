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
not_brace(Ch) :-
  Ch =\= "{", Ch =\= "}".
not_quote(Ch) :-
  Ch =\= "\"".


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
  [Char], { not_brace(Char) },
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

bib_key(Key) --> alphanum(KeyC), {atom_codes(Key, KeyC)}.
bib_value(Val) --> (bib_word(Val) ; bib_braces(Val); bib_quotes(Val)).

% TODO : not_braces to be replaced with nested braces

bib_word(Val) --> alphanum(ValC), {atom_codes(Val, ValC)}.
bib_braces(Val) --> parse_brace(ValC), {atom_codes(Val, ValC)}.
bib_quotes(Val) --> parse_quote(ValC), {atom_codes(Val, ValC)}.


parse_brace(Val) --> "{", parse_bstring(ValS), "}", {append( [0'{| ValS], "}", Val)}.

parse_bstring([0'\\| [Char|Val]] ) --> "\\", [Char], parse_bstring(Val).
parse_bstring([Char|Val]) --> [Char], {not_brace(Char)}, parse_bstring(Val).
parse_bstring(Val) --> parse_brace(ValA), parse_bstring(ValB), {append(ValA, ValB, Val)}.
parse_bstring([]) --> [].

parse_quote(Val) --> "\"", parse_qstring(ValS), "\"", {append( [0'"| ValS], "\"", Val)}.

parse_qstring([0'\\| [Char|Val]] ) --> "\\", [Char], parse_qstring(Val).
parse_qstring([Char|Val]) --> [Char], {not_quote(Char)}, parse_qstring(Val).
parse_qstring([]) --> [].




