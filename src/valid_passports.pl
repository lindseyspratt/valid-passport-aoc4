:- set_prolog_flag(double_quotes, codes).

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

valid_passport_count(File, Count, Bad) :-
    listify(File, Codes),
    phrase(valid_passport_count(Count, Bad), Codes, []).


% file starts with a passport, not whitespace.
% passport parse consumes trailing newlines or EOF.
valid_passport_count(N, B) --> valid_passport(Valid), !, valid_passport_count(N1, B1),
    {Valid=1 -> N is N1+1, B=B1; N = N1, B is B1+1}.
valid_passport_count(0, 0) --> [].

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
valid_passport(Valid) -->
    pairs(P)
      -> {
         setof(K, (member(K,P), K \= cid), [byr,ecl,eyr,hcl,hgt,iyr,pid])
            -> Valid=1
         ;
         Valid=0, writeq(P), nl
         }
    ;
    skip_bad_pairs,
    {Valid = 0}.

skip_bad_pairs --> ("\n\n";[-1]), !.
skip_bad_pairs --> [_], skip_bad_pairs.

pairs([]) --> ("\n";[-1]), !.
pairs([K|T]) --> pair(K), pairs(T).

pair(K1) --> key(K), value(V), {atom_codes(K1, K), validate1(K1, V)}.

key([]) --> ":", !.
key([C|T]) --> [C], {[C] \= ":"}, key(T).

value([]) --> space_or_newline_or_eof, !.
value([H|T]) --> [H], value(T).

space_or_newline_or_eof --> " ".
space_or_newline_or_eof --> "\n".
space_or_newline_or_eof --> [-1].

/*
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
*/

validate1(K, V) :-
    validate(K, V) -> true;
    atom_codes(C, V), writeq(invalid(K, C)), nl -> fail.

validate(byr, V) :- number_codes(N, V), between(1920, 2002, N).
validate(iyr, V) :- number_codes(N, V), between(2010, 2020, N).
validate(eyr, V) :- number_codes(N, V), between(2020, 2030, N).
validate(hgt, V) :- phrase(hgt, V, []).
validate(hcl, V) :- phrase(hcl, V, []).
validate(ecl, V) :- atom_codes(ECL, V), ecl(ECL).
validate(pid, V) :- catch(number_codes(_,V), _, fail), length(V, 9).
validate(cid, _).


hgt --> hgt_value(V), hgt_units(U),
    {number_codes(N, V),
     (U=cm->between(150, 193, N);U=in->between(59,76,N))}.

hgt_value([D|T]) --> [D], {is_digit(D)}, !, hgt_value(T).
hgt_value([]) --> [], !.

hgt_units(in) --> "in", !.
hgt_units(cm) --> "cm", !.


hcl --> "#", hex(H), {length(H,6)}.

hex([C|T]) --> [C], {member(C, "0123456789abcdef")}, !, hex(T).
hex([]) --> [], !.

ecl(ECL) :- member(ECL, [amb, blu, brn, gry, grn, hzl, oth]).
