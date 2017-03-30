:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(sets)).

% Data for the puzzle is from "Changing Rooms" a Star Battle puzzle
% from MIT Mystery Hunt 2017. The solution to the puzzle is:
% [0/1,1/5,2/1,3/5,4/0,5/4,6/0,7/4,0/3,1/7,2/3,3/7,4/2,5/6,6/2,7/6]

solution(Solution) :-
  generate(Solution),
  satisfiesRegions(Solution, [
    [0/0,1/0,0/1,3/1,0/2,1/2,2/2,3/2],
    [2/0,3/0,4/0,5/0,6/0,7/0,1/1,2/1],
    [8/0,9/0,9/1,6/2,7/2,8/2,9/2],
    [4/1,5/1,6/1,7/1,8/1,4/2,0/3,1/3,2/3,3/3,4/3,0/4],
    [5/2,5/3,6/3,6/4,6/5,6/6,6/7,7/7],
    [7/3,8/3,9/3,9/4,8/5,9/5],
    [1/4,2/4,3/4,4/4,5/4,0/5,1/5,0/6,0/7,2/7,3/7,0/8,3/8,0/9,1/9,2/9,3/9],
    [7/4,8/4,7/5,7/6,8/6,9/6,9/7,9/8,7/9,8/9,9/9],
    [2/5,3/5,4/5,5/5,1/6,2/6,5/6,1/7,5/7,1/8,2/8,5/8],
    [3/6,4/6,4/7,8/7,4/8,6/8,7/8,8/8,4/9,5/9,6/9]
    ]).

generate(Solution) :-
  Vars1 = [_,_,_,_,_,_,_,_,_,_],
  Vars2 = [_,_,_,_,_,_,_,_,_,_],
  append(Vars1, Vars2, Vars),
  columns(Vars1, Vars2),
  notadjactent(Vars1, Vars2),
  rows(Vars),
  labeling([], Vars1),
  labeling([], Vars2),
  indexed(0,Vars1,I1),
  indexed(0,Vars2,I2),
  append(I1, I2, Solution).

indexed(_, [], []).
indexed(N, [UH|UT], [IH|IT]) :-
  N1 is N + 1,
  IH = N/UH,
  indexed(N1, UT, IT).

columns([], []).
columns([V1|Vars1], [V2|Vars2]) :-
  V1 + 1 #< V2, columns(Vars1,Vars2).

notadjactent([], []).
notadjactent([_], [_]).
notadjactent([A1,B1|Vars1], [A2,B2|Vars2]) :-
  1 #< abs(A1-B1),
  1 #< abs(A1-B2),
  1 #< abs(A2-B1),
  1 #< abs(A2-B2),
  notadjactent([B1|Vars1],[B2|Vars2]).

rows(L) :-
  global_cardinality(L,[0-2,1-2,2-2,3-2,4-2,5-2,6-2,7-2,8-2,9-2]).

satisfiesRegions(Solution, Regions) :-
  maplist(satisfiesRegion(Solution), Regions).

satisfiesRegion(Solution, Region) :-
  intersection(Region, Solution, Intersection),
  length(Intersection, 2).

go :-
  solution(X),
  writeq(X), nl.
