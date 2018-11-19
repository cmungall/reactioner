:- module(bitvec_util,
          [bitvec_positions/2,
           bitvec_positions/3]).

:- use_module(library(tabling)).



%! bitvec_positions(+AV:int,?AL:list)
% 
% Translates an integer representation of a bit vector of
% to indexes
bitvec_positions(AV,AL) :-
        bitvec_positions(AV,AL,16).

% for speed we use a sliding windown
% to turn the vector into bit positions
bitvec_positions(AV,AL,Window) :-
        Mask is 2**Window -1,
        bitvec_positions(AV,ALx,0,Window,Mask),
        flatten(ALx,AL).

%% bitvec_positions(+AV:int,?AL:list,+Pos,+Window,+Mask) is det
% 
% Mask must = Window^2 -1 (not checked)
% shifts AV down Window bits at a time. If there are any bits in the window,
% use bitvec_positions_lo/2 to get the attribute list from this window.
% note resulting list must be flattened.
% todo: difference list impl?
bitvec_positions(0,[],_,_,_) :- !.
bitvec_positions(AV,AL,Pos,Window,Mask) :-
        !,
        NextBit is AV /\ Mask,
        AVShift is AV >> Window,
        NextPos is Pos+Window,
        (   NextBit=0
        ->  bitvec_positions(AVShift,AL,NextPos,Window,Mask)
        ;   bitvec_positions_lo(NextBit,ALNew,Pos),
            AL=[ALNew|AL2],
            bitvec_positions(AVShift,AL2,NextPos,Window,Mask)).
        
% as bitvec_positions/2, but checks one bit at a time
:- table bitvec_positions_lo/2.
bitvec_positions_lo(AV,AL) :-
        bitvec_positions_lo(AV,AL,0).

bitvec_positions_lo(0,[],_) :- !.
bitvec_positions_lo(AV,AL,Pos) :-
        NextBit is AV /\ 1,
        AVShift is AV >> 1,
        NextPos is Pos+1,
        (   NextBit=1
        ->  AL=[Pos|AL2]
        ;   AL=AL2),
        !,
        bitvec_positions_lo(AVShift,AL2,NextPos).
