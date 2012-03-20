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
  0'0 =< Ch, Ch =< 0'9.
ispunct(Ch) :-
  Ch >= 0'!, Ch =< 0'@ -> true ; Ch >= 0'{, Ch =< 0'~ -> true.
islchar(Ch) :-
  Ch = 0'_; Ch = 0'. ; Ch = 0'-; Ch = 0'+ .

not_brace(Ch) :-
  Ch =\= 0'{, Ch =\= 0'}.
not_quote(Ch) :-
  Ch =\= 0'".

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
  [Char], {isalpha(Char); isdigit(Char)},
  {L1 = [Char|L0] },
  (alphanum(L1, L) ; {reverse(L1, L) }).

punctanum(L) --> punctanum([], L).
punctanum(L0, L) --> 
  [Char], {isalpha(Char); isdigit(Char); ispunct(Char)},
  {L1 = [Char|L0] },
  (punctanum(L1, L) ; {reverse(L1, L) }).

wordanum(L) --> wordanum([], L).
wordanum(L0, L) --> 
  [Char], {isalpha(Char); isdigit(Char); islchar(Char)},
  {L1 = [Char|L0] },
  (wordanum(L1, L) ; {reverse(L1, L) }).

% Just consume spaces
s1_ --> (" ";"\n";"\t"), s_.
s_ --> (s1_ ; "").

% ============================================================

any_case(XX) --> [X], { char_code(C, X), lower_upper(CC, C), char_code(CC, XX)}. 

i_([])     --> []. 
i_([C|Cs]) --> any_case(C), i_(Cs).

parse_txt([]).
parse_txt(Txt) :- phrase(bibs(Bibs), Txt), write(Bibs).

bibs([Bib | Bibs]) --> s_, bib(Bib), s_, bibs(Bibs).
bibs([]) --> [].

bib(Bib) --> ( bib_comment(Bib) ; bib_preamble(Bib) ; bib_string(Bib) ; bib_entry(Bib) ).

bib_entry(bib(Type, Name, Keys)) -->
  "@", bib_type(Type),
  "{", s_, bib_name(Name), s_, ",", s_, bib_keys(Keys), s_, "}", s_.

bib_comment(bib(comment,comment, [kv('key', Val)])) -->
  "@", i_("comment"), bib_braces(Val), s_.

bib_preamble(bib(preamble,preamble, [kv('key', Val)])) -->
  "@", i_("preamble"), bib_braces(Val), s_.

bib_string(bib(string, string, Keys)) -->
  "@", i_("string"), "{", s_, bib_keys(Keys), s_, "}", s_.
bib_string(bib(string, string, Keys)) -->
  "@", i_("string"), "(", s_, bib_keys(Keys), s_, ")", s_.

bib_type(Type) -->
  wordanum(TypeC), {atom_codes(Type, TypeC)}.

bib_name(Name) -->
  wordanum(NameC), {atom_codes(Name, NameC)}.
bib_keys([Pair | Rest]) --> bib_kv(Pair), s_, bib_keys(Rest).
bib_keys([]) --> [].

bib_kv(kv(Key,Val)) -->
  bib_key(Key), s_, "=", s_, bib_value(Val), s_, ("," ; "").

bib_key(Key) --> wordanum(KeyC), {atom_codes(Key, KeyC)}.

bib_value(Val) -->(bib_word(V1); bib_braces(V1); bib_quotes(Val)),
  s_, "#", s_, bib_value(V2), {[V1,V2] = Val}.
bib_value(Val) --> bib_word(Val).
bib_value(Val) --> bib_braces(Val).
bib_value(Val) --> bib_quotes(Val).

bib_word(Val) --> wordanum(ValC), {atom_codes(Val, ValC)}.
bib_braces(Val) --> parse_brace(ValC), {atom_codes(Val, ValC)}.
bib_quotes(Val) --> parse_quote(ValC), {atom_codes(Val, ValC)}.


parse_brace(Val) --> "{", parse_bstring(ValS), "}", {append( [0'{| ValS], "}", Val)}.

parse_bstring([0'\\| [Char|Val]] ) --> "\\", [Char], parse_bstring(Val).
parse_bstring([Char|Val]) --> [Char], {not_brace(Char)}, parse_bstring(Val).
parse_bstring(Val) --> parse_brace(ValA), parse_bstring(ValB), {append(ValA, ValB, Val)}.
parse_bstring([]) --> [].

parse_quote(Val) --> "\"", parse_qstring(ValS), "\"", {append( [0'"| ValS], "\"", Val)}.

parse_qstring([0'\\| [Char|Val]] ) --> "\\", [Char], parse_qstring(Val).
parse_qstring(Val) --> parse_brace(V), parse_qstring(U), {append(V, U, Val)}.
parse_qstring([Char|Val]) --> [Char], {not_quote(Char)}, parse_qstring(Val).
parse_qstring([]) --> [].



