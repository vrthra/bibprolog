:- initialization(go).

go :-
    argument_value(1, File),
    start(File).

start(File):-
    write(trying_file(File)),nl,
    read_txt(File, Txt),
    parsetxt(Txt),
    nl,
    halt.

read_txt(FName, Txt) :-
    open(FName, read, File), read_file(File, Txt), close(File).

put_all([]).
put_all([X|Xs]):- put_code(X), put_all(Xs).

read_file(Stream,[]) :- at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    get_code(Stream, X),
    read_file(Stream,L).

% ============================================================
isalpha(Ch) :- ( Ch >= 0'a, Ch =< 0'z -> true ; Ch >= 0'A, Ch =< 0'Z -> true).
isdigit(Ch) :- 0'0 =< Ch , Ch =< 0'9.
notBrace(Ch) :- Ch =\= "{",Ch =\= "}".

digits(N) --> digits(0, N).
% Accumulator
digits(N0, N) --> % N0 is number already read
    [Char], {isdigit(Char)},
    { N1 is N0 * 10 + (Char - 0'0) },
    ( digits(N1, N) ; { N = N1 }).

letters(L) --> letters([], L).
letters(L0, L) -->
    [Char], { isalpha(Char) },
    {L1 = [Char|L0] },
    (letters(L1, L) ; {reverse(L1,L)}).

alphanum(L) --> alphanum([], L).
alphanum(L0,L) --> 
    [Char], {isalpha(Char); isdigit(Char)},
    {L1 = [Char|L0] },
    (alphanum(L1, L) ; {reverse(L1,L) }).

spaces1 --> (" ";"\n";"\t"),spaces.
spaces --> (spaces1 ; "").

notBraces(L) --> notBraces([], L).
notBraces(L0, L) -->
    [Char], { notBrace(Char) },
    {L1 = [Char|L0] },
    (notBraces(L1, L) ; {reverse(L1,L)}).

% ============================================================
%parsetxt(Txt) :- phrase(bib(Entry), Txt), write(Entry),nl.

parsetxt([]).
parsetxt(Txt) :- phrase(bib(Bib),Txt). %, write(Bib). %, put_all(Bib).

bib(Bib) --> "@",bibType(Bib),
                "{", spaces, bibName(Name), spaces, ",", spaces, bibKeys(Keys), spaces, "}","\n".
bibType(B1) --> alphanum(B), {atom_codes(B1,B), write('Type : '),write(B1), nl}.
bibName(B1) --> alphanum(B), {atom_codes(B1,B), write('Name : '),write(B1), nl}.
bibKeys(KVP) --> ((bibKV(KVP), spaces) -> bibKeys(Rest) ; "").
bibKV(Key) --> bibKey(Key), spaces, "=", spaces, bibValue(Val), spaces, ("," ; ""),
              {write('KV : '), write(Key), write('='), write(Val), nl}.

bibKey(Key) --> alphanum(Key1), {atom_codes(Key, Key1)}.
bibValue(Val) --> (bibWord(Val) ; bibBraces(Val)).

bibWord(Val) --> alphanum(Val1),{atom_codes(Val, Val1)}.
bibBraces(Val) --> "{", notBraces(Val1) ,"}",{atom_codes(Val, Val1)}.

