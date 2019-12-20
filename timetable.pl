:- use_module(library(clpfd)).
:- use_module([library(dialect/sicstus/block)]).

% Author: LiangLiang Zheng 0548966

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test( Data ), timetable( Data, Timetable ), print_timetable( Timetable ).  %
% have all been implemented                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Please run the program by simply typing 'start' in the swipl terminal       %
% The start contains test data, and function test                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% succuessful case 
start :-
    Data = [prof,teacher3,teaches,classes,c2,and,c3,fullstop,
    he,also,teaches,class,c4,fullstop,
    prof,teacher9,teaches,classes,c2,and,c5,fullstop,
    prof,teacher10,teaches,classes,c1,and,c2,fullstop,
    he,also,teaches,class,c4,fullstop,
    prof,teacher2,teaches,classes,c2,and,c5,fullstop,
    prof,teacher1,teaches,classes,c1,and,c3,fullstop,
    she,also,teaches,class,c5,fullstop,
    prof,teacher4,teaches,classes,c3,and,c5,fullstop,
    prof,teacher5,teaches,class,c4,fullstop,
    prof,teacher6,teaches,classes,c4,and,c5,fullstop,
    prof,teacher7,teaches,class,c5,fullstop,
    prof,teacher8,teaches,classes,c3,and,c4,fullstop,
    class,c1,is,in,room,1,fullstop,
    class,c2,is,in,room,2,fullstop,
    class,c4,is,in,room,3,fullstop,
    class,c3,is,in,room,3,fullstop,
    class,c5,is,in,room,3,fullstop,
    class,c1,has,30,students,fullstop,
    class,c2,has,35,students,fullstop,
    class,c3,has,100,students,fullstop,
    class,c4,has,40,students,fullstop,
    class,c5,has,50,students,fullstop,
    room,1,seats,35,students,fullstop,
    room,2,seats,60,students,fullstop,
    room,3,seats,100,students,fullstop,
    classes,c1,and,c2,have,the,same,teacher,fullstop,
    classes,c3,and,c5,are,in,the,same,room,fullstop,
    class,c1,is,before,class,c2,fullstop,
    class,c4,is,after,class,c3,fullstop,
    classes,c1,and,c2,are,on,the,same,day,fullstop
    ],
    timetable(Data, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is a test case where some sentence is not gramatically correct         %
% or he & she have no one to refer to                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fail case 2, error will be thrown 
start1 :-
    %test function test case, have conflict case inside:
    test([
    class,c1,is,in,room,1,fullstop,
    classes,c1,and,c2,have,the,same,teacher,fullstop,
    classes,c1,and,c3,are,in,the,same,room,fullstop,
    class,c1,has,40,students,fullstop,
    he,also,teaches,class,c5,fullstop,
    room,1,seats,100,students,fullstop,
    class,c1,is,before,class,c2,fullstop,
    class,c4,is,after,class,c3,fullstop,
    classes,c1,and,c3,are,on,the,same,day,fullstop
    ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is a test case where some sentence is not gramatically correct         %                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fail case 2, error will be thrown 
start2 :-
    %test function test case, have conflict case inside:
    Data = [has, class, c1,fullstop,
        seats, 35, students,fullstop],

    split(Data, fullstop, WithEmptyResult),
    select([], WithEmptyResult, Result),
    dosomething(Result, _).

test(Data) :-
    split(Data, fullstop, WithEmptyResult),
    select([], WithEmptyResult, Result),
    dosomething(Result, Answer),
    nl,
    mergeHeShe(Answer, Aftermerge),
    sort(Aftermerge, DistictList),
    changeFunctorName(DistictList, AdjustFunctorName),
    nl,
    assertFunctors(AdjustFunctorName).
    
dosomething([], []).
dosomething([H|T], [Head|Tail]) :-
     s(_,Head,H,[]),
     dosomething(T, Tail).


split(In, Sep, [Left|Rest]) :-
    append(Left, [Sep|Right], In), !, split(Right, Sep, Rest).
split(In, _Sep, [In]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% if 2 teacher functors are neighbors in a list and the latter one       %
% contains he or she, then merge these 2 functors into 1, if before      % 
% he or she there is no one to refer, throw an e                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  有错的时候抛出异常
mergeHeShe([], []).
mergeHeShe([H|T],[Head|Tail]) :-
    T == [],
    H = Head,
    mergeHeShe(T, Tail).
mergeHeShe([H|T], [Head|Tail]) :- 
    % starting from the 2nd of the list 
    nth0(0, T, Second),
    % get functor name   
    functor(H, FirstFunctorName, _),
    functor(Second, SecondFunctorName, _),
    % compare with the previous one and return the merged list
    compareNsubmerge(H, Second, FirstFunctorName, SecondFunctorName, Head),
    % if matched, combined them 
    mergeHeShe(T, Tail).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% If both functors are all teacher, one starts with with proper noun, one %
% start with personal noun, merge them, leave the logic for now           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compareNsubmerge(First, Second, FirstFunctor, SecondFunctor, NewFunctor) :-
    FirstFunctor == teacher,
    SecondFunctor == teacher,
    First =.. A,
    Second =.. B,
    nth0(1, A, FirstLSecondEle),
    nth0(1, B, SecondLSecondEle),
    proper_noun(FirstLSecondEle),
    personal_noun(SecondLSecondEle),
    % add the element in the second term to the first functor second term(as a list)
    nth0(2, A, FirstLThirdList),
    nth0(2, B, SecondLThirdList),
    add_tail([FirstLThirdList],SecondLThirdList, ListAdd),
    flatArray(ListAdd, FlatList),
    NewFunctor = teacher(FirstLSecondEle, FlatList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Both of them is teacher, but both is proper noun                        % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compareNsubmerge(First, Second, FirstFunctor, SecondFunctor, NewFunctor) :-
    FirstFunctor == teacher,
    SecondFunctor == teacher,
    First =.. A,
    Second =.. B,
    nth0(1, A, FirstLSecondEle),
    nth0(1, B, SecondLSecondEle),
    proper_noun(FirstLSecondEle),
    proper_noun(SecondLSecondEle),
    First = NewFunctor.

compareNsubmerge(First, _, FirstFunctor, SecondFunctor, NewFunctor) :-
    FirstFunctor == teacher,
    SecondFunctor \== teacher,
    First =.. A,
    nth0(1, A, FirstLSecondEle),
    proper_noun(FirstLSecondEle),
    First = NewFunctor.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% return the second functor situation                                     % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compareNsubmerge(First, Second, FirstFunctor, _, NewFunctor) :-
    FirstFunctor == teacher,
    First =.. A,
    nth0(1, A, FirstLSecondEle),
    personal_noun(FirstLSecondEle),
    Second = NewFunctor.

compareNsubmerge(First, Second, FirstFunctor, SecondFunctor, NewFunctor) :-
    FirstFunctor \== teacher,
    SecondFunctor == teacher,
    Second =.. B,
    nth0(1, B, SecondLSecondEle),
    personal_noun(SecondLSecondEle),
    write('Warning: there is he or she which can not be referred to the former sentence, the system ignores it.'),
    nl,
    nl,
    First = NewFunctor.

compareNsubmerge(First, _, FirstFunctor, SecondFunctor, NewFunctor) :-
    FirstFunctor \== teacher,
    SecondFunctor \== teacher,
    First = NewFunctor.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ChangeFunctorName                                                       % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

changeFunctorName([], []).
changeFunctorName([H|T],[Head|Tail]) :-
    processFunctorName(H, Head),
    changeFunctorName(T, Tail).

% functor name: teacher : simply return it
processFunctorName(H, ReturnFunctor) :-
    functor(H, FunctorName, _),
    FunctorName == teacher,
    ReturnFunctor = H.

% functor name: before, modify it
processFunctorName(H, ReturnFunctor) :-
    functor(H, FunctorName, _),
    H =.. Functor,
    FunctorName == class,
    flatArray(Functor, FlatList),
    member(before, FlatList),
    select(before, FlatList, DeleteBefore),
    select(class, DeleteBefore, DeleteClass),
    append([classbefore], DeleteClass, ReturnList),
    ReturnFunctor =.. ReturnList.

processFunctorName(H, ReturnFunctor) :-
    functor(H, FunctorName, _),
    H =.. Functor,
    FunctorName == class,
    flatArray(Functor, FlatList),
    member(after, FlatList),
    select(after, FlatList, DeleteAfter),
    select(class, DeleteAfter, DeleteClass),
    append([classafter], DeleteClass, ReturnList),
    ReturnFunctor =.. ReturnList.


processFunctorName(H, ReturnFunctor) :-
    functor(H, FunctorName, _),
    H =.. Functor,
    FunctorName == class,
    flatArray(Functor, FlatList),
    member(in, FlatList),
    \+ member(same, FlatList),
    select(in, FlatList, DeleteIn),
    select(class, DeleteIn, DeleteClass),
    append([classinroom], DeleteClass, ReturnList),
    ReturnFunctor =.. ReturnList.

processFunctorName(H, ReturnFunctor) :-
    functor(H, FunctorName, _),
    H =.. Functor,
    FunctorName == classes,
    flatArray(Functor, FlatList),
    member(in, FlatList),
    member(same, FlatList),
    member(room, FlatList),
    select(in, FlatList, DeleteIn),
    select(same, DeleteIn, DeleteSame),
    select(room, DeleteSame, DeleteRoom),
    select(classes, DeleteRoom, DeleteClass),
    append([sameroom], DeleteClass, ReturnList),
    ReturnFunctor =.. ReturnList.

processFunctorName(H, ReturnFunctor) :-
    functor(H, FunctorName, _),
    H =.. Functor,
    FunctorName == classes,
    flatArray(Functor, FlatList),
    member(on, FlatList),
    member(same, FlatList),
    member(day, FlatList),
    select(on, FlatList, DeleteOn),
    select(same, DeleteOn, DeleteSame),
    select(day, DeleteSame, DeleteDay),
    select(classes, DeleteDay, DeleteClass),
    append([sameday], DeleteClass, ReturnList),
    ReturnFunctor =.. ReturnList.

processFunctorName(H, ReturnFunctor) :-
    functor(H, FunctorName, _),
    H =.. Functor,
    FunctorName == classes,
    flatArray(Functor, FlatList),
    member(same, FlatList),
    member(teacher, FlatList),
    select(same, FlatList, DeleteSame),
    select(teacher, DeleteSame, DeleteTeacher),
    select(classes, DeleteTeacher, DeleteClass),
    append([sameteacher], DeleteClass, ReturnList),
    ReturnFunctor =.. ReturnList.

processFunctorName(H, ReturnFunctor) :-
    functor(H, FunctorName, _),
    H =.. Functor,
    FunctorName == class,
    flatArray(Functor, FlatList),
    member(students, FlatList),
    select(students, FlatList, DeleteStudent),
    select(class, DeleteStudent, DeleteClass),
    append([classstudent], DeleteClass, ReturnList),
    ReturnFunctor =.. ReturnList.


processFunctorName(H, ReturnFunctor) :-
    functor(H, FunctorName, _),
    H =.. Functor,
    FunctorName == room,
    flatArray(Functor, FlatList),
    member(students, FlatList),
    select(students, FlatList, DeleteStudent),
    select(room, DeleteStudent, DeleteRoom),
    append([roomstudent], DeleteRoom, ReturnList),
    ReturnFunctor =.. ReturnList.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assert functors to the database                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assertFunctors([]).
assertFunctors([X|Rest]) :- 
    processFunctor(X),
    assertFunctors(Rest).

processFunctor(Functor) :-
    functor(Functor, FunctorName, _),
    FunctorName == teacher,
    Functor=..FunctorList,
    nth0(2, FunctorList, FunctorList1),
    member(class, FunctorList1),
    select(class, FunctorList1, FunctorB),
    nth1(3, FunctorList, _, Remainder),
    append(Remainder, [FunctorB], FunctorA),
    Functor1=..FunctorA,
    mapTNC1(Functor1, FunctorBack),
    write(FunctorBack),
    write(' inserted into system'),
    nl,
    assertz(FunctorBack).

processFunctor(Functor) :-
    functor(Functor, FunctorName, _),
    FunctorName == teacher,
    Functor=..FunctorList,
    nth0(2, FunctorList, FunctorList1),
    member(classes, FunctorList1),
    select(classes, FunctorList1, FunctorB),
    nth1(3, FunctorList, _, Remainder),
    append(Remainder, [FunctorB], FunctorA),
    Functor1=..FunctorA,
    length(FunctorB, N),
    mapTNC1(Functor1, FunctorBack, N),
    write(FunctorBack),
    write(' inserted into system'),
    nl,
    assertz(FunctorBack).

processFunctor(Functor) :-
    functor(Functor, FunctorName, _),
    Functor=..FunctorList,
    nth0(2, FunctorList, FunctorList1),
    \+ is_list(FunctorList1),
    FunctorName == teacher,
    mapTNC(Functor, FunctorBack),
    write(FunctorBack),
    write(' inserted into system'),
    nl,
    assertz(FunctorBack).

processFunctor(Functor) :-
    functor(Functor, FunctorName, _),
    FunctorName \== teacher,
    Functor =.. FunctorList,
    flatArray(FunctorList, FlatList),
    FlatFunctor =.. FlatList,
    mapClass2Number(FlatFunctor, FunctorBack),
    write(FunctorBack),
    write(' inserted into system'),
    nl,
    assertz(FunctorBack).

% replace 
replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

mapTNC1(Functor, BackFunctor, N) :-
    N = 3,
    Functor =.. FunctorList,
    nth0(1, FunctorList, Element1),
    teacherNname(Number1, Element1),
    replace(Element1, Number1, FunctorList, FunctorList1),
    nth0(2, FunctorList, Element2),
    nth0(0, Element2, C1),
    class(N1, C1),
    replace(C1, N1, Element2, P1),
    nth0(1, P1, C2),
    class(N2, C2),
    replace(C2, N2, P1, P2),
    nth0(2, P2, C3),
    class(N3, C3),
    replace(C3, N3, P2, P3),
    replace(Element2, P3, FunctorList1, FunctorBackList),
    BackFunctor =.. FunctorBackList.

mapTNC1(Functor, BackFunctor, N) :-
    N = 2,
    Functor =.. FunctorList,
    nth0(1, FunctorList, Element1),
    teacherNname(Number1, Element1),
    replace(Element1, Number1, FunctorList, FunctorList1),
    nth0(2, FunctorList, Element2),
    nth0(0, Element2, C1),
    class(N1, C1),
    replace(C1, N1, Element2, P1),
    nth0(1, P1, C2),
    class(N2, C2),
    replace(C2, N2, P1, P2),
    replace(Element2, P2, FunctorList1, FunctorBackList),
    BackFunctor =.. FunctorBackList.  
% 
mapTNC(Functor, BackFunctor) :-
    Functor =.. FunctorList,
    nth0(1, FunctorList, Element1),
    teacherNname(Number1, Element1),
    replace(Element1, Number1, FunctorList, FunctorList1),
    nth0(2, FunctorList, Element2),
    class(Number2, Element2),
    replace(Element2, [Number2], FunctorList1, BackFunctorList),
    BackFunctor =.. BackFunctorList.

% map class to number 
mapClass2Number(Functor, BackFunctor) :-
    functor(Functor, FunctorName, _),
    FunctorName = roomstudent,
    BackFunctor = Functor.

mapClass2Number(Functor, BackFunctor) :-
    functor(Functor, FunctorName, _),
    FunctorName = classbefore,
    Functor =.. FunctorList,
    nth0(1, FunctorList, Element1),
    class(Number1, Element1),
    replace(Element1, Number1, FunctorList, FunctorList1),
    nth0(2, FunctorList, Element2),
    class(Number2, Element2),
    replace(Element2, Number2, FunctorList1, BackFunctorList),
    BackFunctor =.. BackFunctorList.

mapClass2Number(Functor, BackFunctor) :-
    functor(Functor, FunctorName, _),
    FunctorName = classafter,
    Functor =.. FunctorList,
    nth0(1, FunctorList, Element1),
    class(Number1, Element1),
    replace(Element1, Number1, FunctorList, FunctorList1),
    nth0(2, FunctorList, Element2),
    class(Number2, Element2),
    replace(Element2, Number2, FunctorList1, BackFunctorList),
    BackFunctor =.. BackFunctorList.

mapClass2Number(Functor, BackFunctor) :-
    functor(Functor, FunctorName, _),
    FunctorName = sameday,
    Functor =.. FunctorList,
    nth0(1, FunctorList, Element1),
    class(Number1, Element1),
    replace(Element1, Number1, FunctorList, FunctorList1),
    nth0(2, FunctorList, Element2),
    class(Number2, Element2),
    replace(Element2, Number2, FunctorList1, BackFunctorList),
    BackFunctor =.. BackFunctorList.

mapClass2Number(Functor, BackFunctor) :-
    functor(Functor, FunctorName, _),
    FunctorName = sameteacher,
    Functor =.. FunctorList,
    nth0(1, FunctorList, Element1),
    class(Number1, Element1),
    replace(Element1, Number1, FunctorList, FunctorList1),
    nth0(2, FunctorList, Element2),
    class(Number2, Element2),
    replace(Element2, Number2, FunctorList1, BackFunctorList),
    BackFunctor =.. BackFunctorList.

mapClass2Number(Functor, BackFunctor) :-
    functor(Functor, FunctorName, _),
    FunctorName = sameroom,
    Functor =.. FunctorList,
    nth0(1, FunctorList, Element1),
    class(Number1, Element1),
    replace(Element1, Number1, FunctorList, FunctorList1),
    nth0(2, FunctorList, Element2),
    class(Number2, Element2),
    replace(Element2, Number2, FunctorList1, BackFunctorList),
    BackFunctor =.. BackFunctorList.

mapClass2Number(Functor, BackFunctor) :-
    functor(Functor, FunctorName, _),
    FunctorName = classinroom,
    Functor =.. FunctorList,
    nth0(1, FunctorList, Element1),
    class(Number1, Element1),
    replace(Element1, Number1, FunctorList, BackFunctorList),
    BackFunctor =.. BackFunctorList.

mapClass2Number(Functor, BackFunctor) :-
    functor(Functor, FunctorName, _),
    FunctorName = classstudent,
    Functor =.. FunctorList,
    nth0(1, FunctorList, Element1),
    class(Number1, Element1),
    replace(Element1, Number1, FunctorList, BackFunctorList),
    BackFunctor =.. BackFunctorList.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% flatten a list                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flatArray([],[]).
flatArray([Head|Tail],R) :-
	flatArray(Head,New_Head),
	flatArray(Tail,New_Tail),
	append(New_Head,New_Tail,R).
flatArray([Head|Tail1], [Head|Tail2]) :-
	Head \= [],
	Head \= [_|_],
    flatArray(Tail1,Tail2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this could work no matter the appended is an element or a list          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_tail([],X,[X]).
add_tail([H|T],X,[H|L]):-add_tail(T,X,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sentence                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% starting from prof or he or she 
s(s(NP, VP), Functor)   --> [class], np(NP, X), vp(VP, T), {Functor=..[class, X, T]}.
s(s(NP, VP), Functor)   --> [classes], np(NP, X), vp(VP, T), {Functor=..[classes, X, T]}.
s(s(NP, VP), Functor)   --> [room], np(NP, X), vp(VP, T), {Functor=..[room, X, T]}.
% if not match with class, automatically match with sentence starting of: prof,he,she
s(s(NP, VP), Functor)   --> [prof], np(NP, X), vp(VP,T), {Functor=..[teacher,X,T]}.
% more gerneral one, could match with personal noun 
s(s(NP, VP), Functor)   --> person(NP, X), vp(VP,T), {Functor=..[teacher,X,T]}.
s(s(NP, VP), _)         --> vp(VP,_), np(NP,_),{write('Catched, it seems like you have problem in the grammer'), nl}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Noun Phrase Rule                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
np(np(PN),X)                 --> prop(PN, X).
np(np(P),X)                  --> person(P, X).
np(np(N),X)                  --> n(N, X).
np(np(N1,N2),[X1,X2])        --> n(N1, X1), [and], n(N2, X2).
np(np(N1,N2,N3),[X1,X2,X3])  --> n(N1, X1), n(N2, X2), [and], n(N3,X3).
% match 2 nouns from specific to general
np(np(N),X)                  --> [class], n(N, X).
np(np(N),X)                  --> [room], n(N, X).
np(np(Q1,N2),[X1,X2])        --> quantity(Q1,X1), n(N2, X2).
np(np(N1,N2,N3),[X1,X2,X3])  --> [classes], n(N1, X1), n(N2, X2), [and], n(N3,X3).
np(np(DET, ADJ, N), [Adj, X])--> det(DET, _DET),
                                 adj(ADJ, Adj),
                                 n(N, X).
np((DET, N), X)              --> det(DET, _DET),
                                 n(N, X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Verb Phrase Rule                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vp(vp(TV), T)                    -->  tv(TV, T).
vp(vp(PREP, TV, NP),T)           -->  prep(PREP, _PREP),
                                      tv(TV, _TV),
                                      np(NP, T).
vp(vp(TV,NP), T)                 -->  tv(TV, _TV),
                                      np(NP, T).
vp(vp(TV, PREP,NP), [Prep, Np])  -->  tv(TV, _TV),
                                      prep(PREP, Prep),
                                      np(NP, Np).
vp(vp(ADVB,TV,NP), T)            -->  adv(ADVB,_ADV),
                                      tv(TV, _TV),
                                      np(NP, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Lexical                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
det(det(X), X)            -->     [X], {article(X)}.
conj(conj(X), X)          -->     [X], {conjunction(X)}.
person(person(X), X)      -->     [X], {personal_noun(X)}.
prop(prop(X), X)          -->     [X], {proper_noun(X)}.
n(n(X), X)                -->     [X], {noun(X)}.
adj(adj(X),X)             -->     [X], {adjective(X)}.
tv(tv(X), X)              -->     [X], {transitive_verb(X)}.
prep(prep(X), X)          -->     [X], {preposition(X)}.
adv(adv(X), X)            -->     [X], {advb(X)}.
quantity(quantity(X),X)   -->     [X], {qua(X)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vocabulary                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
qua(X) :-
    member(X, [30,35,100,40,50,60]).

% % Noun
noun(X) :- 
    member(X, [class, classes, student, students, prof, room, day, c1, c2, c3, c4, c5, a1,teacher,1,2,3]).

% % conjunction
conjunction(X) :- 
    member(X, [and]).

% % article
article(X) :-
    member(X, [a, the, some]).

% % preposition
preposition(X) :-
    member(X, [in, on, before, after]).

% % verb
transitive_verb(X) :-
    member(X, [is, are, teach, teaches, has, have, seats]).

% % proper noun
proper_noun(X) :-
    member(X, [teacher9, teacher10,teacher1,teacher2,teacher3,teacher4,teacher5,teacher6,teacher7,teacher8]).

personal_noun(X) :-
    member(X, [he, she]).

% % adjective
adjective(X) :-
    member(X, [same]).

advb(X) :-
    member(X, [also]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TimeTable data structure                                                %
% schedule(Number, TimePeriod, Day, Teacher, Class, Room)                 %
% 1. Time is from 8 means:8~9 9 means:9~10                                %
% if a teacher needs prepare 2 hours ago if there is a course before,     %
% then eg: first class 8(8~9), then the second class must starts after    %
% 11(11~12)                                                               %
% 2. Day: 5 week days from 1 ~ 5                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COSTRAINTS                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constrain_class(Class) :-
    Class in 1..5.
    
constrain_teacher(Teacher) :-
    Teacher in 1..10.

constrain_room(Room) :-
    Room in 1..3.


% Constaints for processing before and after 
% constraint_beforeAfter(A, B) :-
%     A #< B.

% Map Class to number 

class(1,c1).
class(2,c2).
class(3,c3).
class(4,c4).
class(5,c5).

% Map teacher to number 

teacherNname(1, teacher9).
teacherNname(2, teacher10).
teacherNname(3, teacher1).
teacherNname(4, teacher2).
teacherNname(5, teacher3).
teacherNname(6, teacher4).
teacherNname(7, teacher5).
teacherNname(8, teacher6).
teacherNname(9, teacher7).
teacherNname(10, teacher8).

% Map 25 (day and time) to number
dayNTime(1, 1, 8).
dayNTime(2, 1, 9).
dayNTime(3, 1, 10).
dayNTime(4, 1, 11).
dayNTime(5, 1, 12).
dayNTime(6, 2, 8).
dayNTime(7, 2, 9).
dayNTime(8, 2, 10).
dayNTime(9, 2, 11).
dayNTime(10, 2, 12).
dayNTime(11, 3, 8).
dayNTime(12, 3, 9).
dayNTime(13, 3, 10).
dayNTime(14, 3, 11).
dayNTime(15, 3, 12).
dayNTime(16, 4, 8).
dayNTime(17, 4, 9).
dayNTime(18, 4, 10).
dayNTime(19, 4, 11).
dayNTime(20, 4, 12).
dayNTime(21, 5, 8).
dayNTime(22, 5, 9).
dayNTime(23, 5, 10).
dayNTime(24, 5, 11).
dayNTime(25, 5, 12).


% Some facts in the database: now only for testing 
% classstudent(1,30).
% classstudent(2,35).
% classstudent(3,100).
% classstudent(4,40).
% classstudent(5,50).
% roomstudent(1,35).
% roomstudent(2,60).
% roomstudent(3,100).

% classinroom(1,1). 
% classinroom(2,2).
% classinroom(4,3).
% classinroom(3,3).
% classinroom(5,3).
% classbefore(1,2).
% classafter(4,3).
% sameteacher(1,2).
% sameday(1,2).
% sameroom(3,5).

% teacher(1,[2,5]).
% teacher(2,[1,2,4]).
% teacher(3,[1,3,5]).
% teacher(4,[2,5]).
% teacher(5,[2,3,4]).
% teacher(6,[3,5]).
% teacher(7,[4]).
% teacher(8,[4,5]).
% teacher(9,[5]).
% teacher(10,[3,4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% timetable/2                                                             % 
% The first argument of this function is Test data, the second argument is%
% the variable of the timetable, first defined all the timeslot should be %
% different in the 25 long list time slot, then process with basic        %
% constraints such as class range, teacher range, room range              %
% then a frequency constraint about how many courses a teacher should     %
% teach in a week is also implemented to ensure that all teacher have     %
% at least some courses that could cover all the week                     %
% Then sameday constraints is implemented                                 %
% other constraints are implemented in a outSchedule, in which a nested   %
% recursion is implemented, since this is not 2 neigbor time slot         %
% comparison, a timeslot should compare to any other 24 other timeslot    %
% detailed explanation could be found in outerSchedule                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timetable(Data, Timetable) :-
    test(Data),
    nl,
    nl,
    write('      Please wait for 1.5~2 minutes patiently, then the timetable will be shown below. Thanks! '),
    nl,
    nl,
    write('      At this moment you could resize your terminal bigger to have a good view of the timetable!'),
    nl,
    nl,
    length(Timetable, 25),
    length(Numbers, 25),
    all_different(Numbers),
    basic_constrain_schedule(Numbers, Variables, Timetable),
    teacher_constraint1(Timetable),
    course_constraint1(Timetable),
    % onlyforSameDay(Timetable),
    outerSchedule(Timetable),
    % labeling([ffc], Variables),
    print_timetable(Timetable).

% recursively get teacher list 
teacher_constraint1(Timetable) :-
    getTList(Timetable, TeacherList),
    at_least_one_teacher(TeacherList).

% recursively get class list
course_constraint1(Timetable) :-
    getCList(Timetable, Classes),
    at_least_one_course(Classes).

% recursively get teacher list 
getTList([],[]).
getTList([schedule(_, _, _, Teacher, _, _)|T], [Head|Tail]) :- 
    Head = Teacher,
    getTList(T, Tail).

% recursively get class list
getCList([],[]).
getCList([schedule(_, _, _, _, Class, _)|T], [Head|Tail]) :- 
    Head = Class,
    getCList(T, Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some basic constraints, range of class, teacher, room etc..             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
basic_constrain_schedule([], [], []).
basic_constrain_schedule([Number|Numbers],[Teacher, Class, Room| Variables],[schedule(Number, TimePeriod, Day, Teacher, Class, Room) | DayList]) :-
                    % day and time slots
                    dayNTime(Number, Day, TimePeriod),
                    %write(schedule(Number, TimePeriod, Day, Teacher, Class, Room)),
                    % range of class
                    constrain_class(Class),
                    % range of teacher 
                    constrain_teacher(Teacher),
                    % range of room
                    constrain_room(Room),
                    % basic facts constraints teacher class
                    teacherClass_constraints(Class,Teacher),
                    % basic facts class room
                    classRoom_constraints(Class, Room),
                    % which teacher teach which class
                    basic_constrain_schedule(Numbers, Variables, DayList).


% teacherClass constraints in the basic constraints ,
% block is used here because 
:- block teacherClass_constraints(-,-).
:- block classRoom_constraints(-,-).
teacherClass_constraints(Class1,Teacher1) :-
    teacher(Teacher1, ClassList),
    member(Class1, ClassList).

classRoom_constraints(Class1, Room1) :-
    classinroom(Class1, Room1),
    
    classstudent(Class1, StudentNumber),
    % Seats in room 
    roomstudent(Room1, RoomSeats),
    % StudentNumber should less than Roomseats 
    StudentNumber #=< RoomSeats.

classRoom_constraints(Class1, Room1) :-
    \+ classinroom(Class1, _),
    classstudent(Class1, StudentNumber),
    % Seats in room 
    roomstudent(Room1, RoomSeats),
    % StudentNumber should less than Roomseats 
    StudentNumber #=< RoomSeats.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 10 teacher, at least one course                                        %
% The reason I set C2~C3 to at least courses is that                     %
% after processing with constraints, C1 has been assigned                %
% a lot in a week, which is not reasonable for a common                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
at_least_one_course(Classes) :-
    % length(Classes, 25), 
    global_cardinality(Classes, [1-C1,2-C2,3-C3,4-C4,5-C5]),
    (C1 #>= 1) #/\
    (C2 #>= 3) #/\
    (C3 #>= 3) #/\
    (C4 #>= 3) #/\
    (C5 #>= 3).

% Same reason as mentioned above
at_least_one_teacher(Teachers) :-
    % length(Teachers, 25),
    global_cardinality(Teachers, [1-T1,2-T2,3-T3,4-T4,5-T5,6-T6,7-T7,8-T8,9-T9,10-T10]),
    (T1 #>= 2) #/\
    (T2 #>= 2) #/\
    (T3 #>= 2) #/\
    (T4 #>= 2) #/\
    (T5 #>= 2) #/\
    (T6 #>= 2) #/\
    (T7 #>= 2) #/\
    (T8 #>= 2) #/\
    (T9 #>= 2) #/\
    (T10 #>= 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Nested recursion, recursively compare 1 schedule with the rest of ther schedules  %
% Comparison detail 1 : 2~25, 2: 3~25, 3:4~25... in total (24×25/2) = 300 times     %
% comparisons                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
outerSchedule([]).
outerSchedule([schedule(Number1, TimePeriod1, Day1, Teacher1, Class1, Room1)|T1]) :- 
        % always get the first element of the List and do recursion, transfer rest of the list
        % to innerSchedule
        innerSchedule([schedule(Number1, TimePeriod1, Day1, Teacher1, Class1, Room1)], T1),
    outerSchedule(T1).

innerSchedule([], []).
innerSchedule([_], []).
innerSchedule([schedule(Number1, TimePeriod1, Day1, Teacher1, Class1, Room1)], [schedule(Number2, TimePeriod2, Day2, Teacher2, Class2, Room2)|T2]) :-
          % get the first element of the T1 transferred from outerSchedule and compare each other
          % constraint for same room
          same_room_constraints(schedule(Number1, TimePeriod1, Day1, Teacher1, Class1, Room1), schedule(Number2, TimePeriod2, Day2, Teacher2, Class2, Room2)),
          % constraint for time relation: specifically, before and after 
          time_relation_class(schedule(Number1, TimePeriod1, Day1, Teacher1, Class1, Room1), schedule(Number2, TimePeriod2, Day2, Teacher2, Class2, Room2)),
          % constraint for teacher preparing time, all the teacher should have 2 hours to prepare for the next class if he or she have class before 
          teacherprepare_constraints(schedule(Number1, TimePeriod1, Day1, Teacher1, Class1, Room1), schedule(Number2, TimePeriod2, Day2, Teacher2, Class2, Room2)),
          innerSchedule([schedule(Number1, TimePeriod1, Day1, Teacher1, Class1, Room1)], T2).

% to constraint if 2 classes are in the same room.
onlyforSameDay([]).
onlyforSameDay([schedule(_, _, _, _, Class1, _), schedule(_, _, _, _, Class2, _), schedule(_, _, _, _, Class3, _), schedule(_, _, _, _, Class4, _), schedule(_, _, _, _, Class5, _)| DayList]) :-
    sameday(ClassA, ClassB),
    ClassSet = {ClassA, ClassB},
    (Class1 in ClassSet) #==> ((Class2 in ClassSet)#/\(Class1#\=Class2))#\/
                              ((Class3 in ClassSet)#/\(Class1#\=Class3))#\/
                              ((Class4 in ClassSet)#/\(Class1#\=Class4))#\/
                              ((Class5 in ClassSet)#/\(Class1#\=Class5)),
    (Class2 in ClassSet) #==> ((Class1 in ClassSet)#/\(Class2#\=Class2))#\/
                              ((Class3 in ClassSet)#/\(Class2#\=Class3))#\/
                              ((Class4 in ClassSet)#/\(Class2#\=Class4))#\/
                              ((Class5 in ClassSet)#/\(Class2#\=Class5)),
    (Class3 in ClassSet) #==> ((Class1 in ClassSet)#/\(Class3#\=Class1))#\/
                              ((Class2 in ClassSet)#/\(Class3#\=Class2))#\/
                              ((Class4 in ClassSet)#/\(Class3#\=Class4))#\/
                              ((Class5 in ClassSet)#/\(Class3#\=Class5)),
    (Class4 in ClassSet) #==> ((Class1 in ClassSet)#/\(Class4#\=Class1))#\/
                              ((Class2 in ClassSet)#/\(Class4#\=Class2))#\/
                              ((Class3 in ClassSet)#/\(Class4#\=Class3))#\/
                              ((Class5 in ClassSet)#/\(Class4#\=Class5)),
    (Class5 in ClassSet) #==> ((Class1 in ClassSet)#/\(Class5#\=Class1))#\/
                              ((Class2 in ClassSet)#/\(Class5#\=Class2))#\/
                              ((Class3 in ClassSet)#/\(Class5#\=Class3))#\/
                              ((Class4 in ClassSet)#/\(Class5#\=Class4)),
    onlyforSameDay(DayList).


% teacher preparing time constraint
% % constraint before
time_relation_class(schedule(_, TimePeriod1, Day1, _, Class1, _), schedule(_, TimePeriod2, Day2, _, Class2, _)) :-
            % before 
            classbefore(ClassA, ClassB),
            classafter(ClassC, ClassD),
            ((Class1 #= ClassA) #/\ (Class2 #= ClassB) #/\ (Day1 #= Day2)) #==> (TimePeriod1 #< TimePeriod2),
            ((Class2 #= ClassA) #/\ (Class1 #= ClassB) #/\ (Day1 #= Day2)) #==> (TimePeriod1 #> TimePeriod2),
            ((Class1 #= ClassC) #/\ (Class2 #= ClassD) #/\ (Day1 #= Day2)) #==> (TimePeriod1 #> TimePeriod2),
            ((Class1 #= ClassD) #/\ (Class2 #= ClassC) #/\ (Day1 #= Day2)) #==> (TimePeriod1 #< TimePeriod2).

same_room_constraints(schedule(_, _, _, _, Class1, Room1), schedule(_, _, _, _, Class2, Room2)) :-
    sameroom(ClassA, ClassB),
    ClassSet = {ClassA, ClassB},
    ((Class1 #\= Class2) #/\ (Class1 in ClassSet) #/\ (Class2 in ClassSet)) #==> (Room1 #= Room2).

% same teacher constraints 2 arguments 
same_teacher_constraints(schedule(_, _, _, Teacher1, Class1, _), schedule(_, _, _, Teacher2, Class2, _)) :-
    sameteacher(ClassA, ClassB),
    ClassSet = {ClassA, ClassB},
    ((Class1 #\= Class2) #/\ (Class1 in ClassSet) #/\ (Class2 in ClassSet)) #==> (Teacher1 #= Teacher2).

% teacher preparing time constraints
teacherprepare_constraints(schedule(_, TimePeriod1, Day1, Teacher1, _, _), schedule(_, TimePeriod2, Day2, Teacher2, _, _)) :- 
    % same day 
    ((Day1 #= Day2) #/\ (Teacher1 #= Teacher2) #/\ (TimePeriod1 #< TimePeriod2)) #==> ((TimePeriod1 + 2) #< (TimePeriod2)),
    ((Day1 #= Day2) #/\ (Teacher1 #= Teacher2) #/\ (TimePeriod1 #> TimePeriod2)) #==> ((TimePeriod1) #> (TimePeriod2 + 2)).
    % if the teacher appear, the time should be larger than the previous hour at least 2 

% for timetable printing
print_timetable( Timetable ) :-
    Timetable = [T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22,T23,T24,T25],
    write("                     Monday        Tuesday        Wednesday       Thursday       Friday"),
    nl,
    format("|~`-t~100||~n"),
    write('| 8AM  ~ 9AM    '), select_day(T1), select_day(T6),select_day(T11),select_day(T16),select_day(T21), write('         |'),
    nl,
    write('|                                                                                                   |'),
    nl,
    write('| 9AM  ~ 10AM   '), select_day(T2), select_day(T7),select_day(T12),select_day(T17),select_day(T22), write('      |'),
    nl,
    write('|                                                                                                   |'),
    nl,
    write('| 10AM ~ 11AM   '), select_day(T3), select_day(T8),select_day(T13),select_day(T18),select_day(T23), write('         |'),
    nl,
    write('|                                                                                                   |'),
    nl,
    write('| 11AM ~ 12AM   '), select_day(T4), select_day(T9),select_day(T14),select_day(T19),select_day(T24), write('         |'),
    nl,
    write('|                                                                                                   |'),
    nl,
    write('| 12AM ~ 1PM    '), select_day(T5), select_day(T10),select_day(T15),select_day(T20),select_day(T25), write('       |'),
    nl,
    format("|~`-t~100||~n").


select_day(schedule(_, _, _, Teacher1, Class1, Room1)) :-
    teacherNname(Teacher1,TeacherName),
    format("~a:C~a/R~a~t~8+ ", [TeacherName, Class1, Room1]).