:- module(stat_util,
          [average/2,
           stddev/2]).


average(L,A) :-
        length(L,N),
        N > 0,
        sumlist(L,T),
        A is T/N.

stddev(L,SD) :-
        length(L,N),
        N > 1,
        average(L,A),
        maplist([X,D]>>(D is (X-A)**2),L,Ds),
        sumlist(Ds,TD),
        SD is sqrt(TD/(N-1)).
    
