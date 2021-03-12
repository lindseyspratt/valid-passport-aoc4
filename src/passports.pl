listify(F, L) :-
	open(F, read, S),
	listify1(L, S),
	close(S).

listify1(L, S) :-
  get_code(S, H),
  (H = -1
   -> L = [H]
  ; L = [H|T],
    listify1(T, S)
  ).

passport_count(File, Count, Bad) :-
    listify(File, Codes),
    phrase(passport_count(Count, Bad), Codes, []).


% file starts with a passport, not whitespace.
% passport parse consumes trailing newlines or EOF.
passport_count(0, 0) --> [].
passport_count(N, B) --> passport(Valid), passport_count(N1, B1),
    {Valid=1 -> N is N1+1, B=B1; N = N1, B is B1+1}.

/*
byr (Birth Year)
iyr (Issue Year)
eyr (Expiration Year)
hgt (Height)
hcl (Hair Color)
ecl (Eye Color)
pid (Passport ID)
cid (Country ID)

*/

% pairs parse consumes trailing newlines or EOF.
passport(Valid) --> pairs(P),
    {setof(K, (member(K,P), K \= cid), [byr,ecl,eyr,hcl,hgt,iyr,pid])
        ->Valid=1
     ;Valid=0, writeq(P), nl}.

pairs([]) --> "\n";[-1].
pairs([K|T]) --> pair(K), pairs(T).

pair(K1) --> key(K), value, {atom_codes(K1, K)}.

key([]) --> ":", !.
key([C|T]) --> [C], {[C] \= ":"}, key(T).

value --> space_or_newline_or_eof, !.
value --> [_], value.

space_or_newline_or_eof --> " ".
space_or_newline_or_eof --> "\n".
space_or_newline_or_eof --> [-1].