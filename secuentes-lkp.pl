/* Convertir a forma disyuntiva negada*/
:- op(1,fx,neg).
:- op(2,xfy,or).
:- op(2,xfy,and).
:- op(2,xfy,implies).
:- op(2,xfy,dimplies).

fnd(F,F):- atom(F).

fnd(F1 or F2,G1 or G2):-
    fnd(F1,G1),
    fnd(F2,G2).

fnd(neg F1, neg G1):-
    fnd(F1,G1).

fnd(F1 and F2,neg((neg G1) or (neg G2))):-
    fnd(F1,G1),
    fnd(F2,G2).

fnd(F1 implies F2,(neg G1) or G2):-
    fnd(F1,G1),
    fnd(F2,G2).

fnd(F1 dimplies F2,neg(neg((neg G1 or G2)) or neg(neg G2 or G1))):-
    fnd(F1,G1),
    fnd(F2,G2).
/********************************************Reglas Lógicas************************************************************/
%Caso base
sq(L, R):- not(intersection(L,R,[])),!,nl,write('Caso base: '),write(L),write(' |- '),write(R),nl,!.

%Casos generales
%Negación 
%Negación Izquierda
sq([neg LH | LT], R) :- nl,write('Negación izquierda'),nl,write(LT),write(' |- '),write(LH),write(R),nl, sq(LT, [LH|R]),!.
%Negación
%Negación derecha
sq(L, [neg RH | RT]) :- nl,write('Negación derecha'  ),nl,write(RH),write(L),write(' |- '),write(RT),nl, sq([RH|L], RT),!.

%Disyunción
%Disyunción izquierda
sq([LH1 or LH2 | LT], R) :-
    nl,write('Disyunción izquierda A'),nl,write(LH1),write(' |- '),write(R),nl,sq([LH1 | LT], R),
    nl,write('Disyunción izquierda B'),nl,write(LH2),write(' |- '),write(R),nl,sq([LH2 | LT], R).
%Disyunción derecha
sq(L, [RH1 or RH2 | RT]) :-
    union([RH1,RH2],RT,RTunion),
    nl,write('Disyunción derecha'),nl,write(L),write(' |- '),write(RTunion),nl,sq(L,RTunion),!.
/*******************************************Reglas Estructurales******************************************************/
%Exchange
%Izquierda
sq(Gamma, Delta) :- not(atom_list(Gamma)), 
    sq_exchange(Gamma, Gamma_changed),
    write('Exchange Izquierdo'),nl, write(Gamma_changed),write(' |- '),write(Delta),nl, sq(Gamma_changed, Delta), !.
%Derecha
sq(Gamma, Delta) :- not(atom_list(Delta)), 
    sq_exchange(Delta, Delta_changed),
    write('Exchange Derecho'),nl, write(Gamma),write(' |- '),write(Delta_changed),nl,sq(Gamma, Delta_changed), !.
/*
 * atom_list/1
 * Verifica que los elementos de la lista están en la forma atómica 
*/
atom_list([]).
atom_list([H|T]) :- atom(H), atom_list(T).

/*sq_exchange/2
 * 
 * Se recibe una lista y el primer elemento pasa al final
 * sirve por igual para Izquierda y Derecha
 * Para probar sq_exchange: sq2([p or q, r, p, q], L).
*/
sq_exchange([H|T], L) :- append(T, [H], L).

/***************************Para hacer una prueba como si fuera una consulta con una sola fórumula**********************/
sequence(F):- fnd(F, G), nl,write(' |- '),write([G]),nl, sq([],[G]).

/* 
 * Fórumulas para consultas:
 * Válidas: 
 * sequence( p implies (q implies (q implies p) )).
 * sequence(a implies b or b implies a).
 * sequence(neg (a implies b) implies a ).
 * 
 * No válida 
 * sequence((p implies q) dimplies ( neg q implies p)).
 * 
 * */




















