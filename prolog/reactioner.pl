/** <module> reactioner

  Reaction Entity Recognition, Parsing and Inference

  Basic Data Structure:

  - Reaction = DirectionOperator(Side, Side)
  - Side = [ElQ1, ..., ElQn]
  - ElQ = Chemical - Number
  - Chemical = Cls / ClsLabel

  
  
*/

:- encoding(utf8).

:- module(reactioner,
          [op(700,xfy,<=>),
           catalytic_activity/1,
           catalytic_activity/2,
           index_chebi/0,
           chemical_elqs/2,
           sum_all_elqs/2,
           diff_all_elqs/2,
           no_parse/2,
           chebi_no_parse/2,
           reaction_signature/2,
           identical_signature/2,
           identical_signature/4,
           rhea2xref/3,
           rhea2xref_db/3,
           cls_rhea_xref_uri/2,
           new_rhea_match/5,
           check_rhea/5,
           compare_rhea_chebi_names/6,
           rhea_derived_synonym/4,
           rhea_derived_synonym/5,
           defn_reaction/4,
           mf_reaction/2,
           mf_reaction/5,
           chemterm_lexmatch/3,
           reaction_tmap/3,
           reaction_tmap/4,
           grouping_outlier/2,
           grouping_outlier/3,
           learn_grouping/3]).

:- op(700,xfy,<=>).

:- use_module(library(reactioner/learner)).
:- use_module(library(reactioner/rhea_wrapper),[]).
:- use_module(library(reactioner/stat_util)).
:- use_module(library(sparqlprog/dataframe)).
:- use_module(library(sparqlprog/obo_util)).

:- use_module(library(index_util)).
:- use_module(library(rdf_matcher)).
:- use_module(library(tabling)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_sandbox)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(dcg_util)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/owl_util)).

%:- multifile sandbox:safe_primitive/1.
%sandbox:safe_primitive(reactioner:term_best_match(_,_,_,_)).

solutions(X,G,L) :- setof(X,G^G,L),!.
solutions(_,_,[]).

:- rdf_register_prefix(rh, 'http://rdf.rhea-db.org/').
:- rdf_register_prefix('CHEBI','http://purl.obolibrary.org/obo/CHEBI_').
:- rdf_register_prefix('GO','http://purl.obolibrary.org/obo/GO_').
:- rdf_register_prefix('Orphanet','http://www.orpha.net/ORDO/Orphanet_').
:- rdf_register_ns(oio,'http://www.geneontology.org/formats/oboInOwl#').
:- rdf_register_ns(def, 'http://purl.obolibrary.org/obo/IAO_0000115').
:- rdf_register_ns(ca, 'http://purl.obolibrary.org/obo/GO_0003824').
:- rdf_register_ns(noparse, 'http://x.org/noparse#').
:- rdf_register_ns(modified_chemical, 'http://x.org/modified_chebi#').

%! index_chebi is det.
%
%   materialize index for CHEBI ontology metadata
index_chebi :-
        materialize_index(basic_annot(+,+,-,-)).

%! catalytic_activity(?Activity) is nondet
%! catalytic_activity(?Activity, ?TextDef:atom) is nondet
%
%   true if Activity is a GO URI for a catalytic activity
catalytic_activity(X) :-
        rdfs_subclass_of(X,ca:''),
        \+ rdf_global_id(ca:'',X).

catalytic_activity(X, Def) :-
        catalytic_activity(X),
        rdf(X,def:'',DefLit),        
        ensure_atom(DefLit,Def).


%! no_parse(?Activity,?Def:atom) is nondet
%
%   true if Def is text def for Activity, and Def cannot be parsed
no_parse(X,Def) :-
        catalytic_activity(X),
        has_def(X,Def),
        \+ candidate_mf_reaction(X,_,_,_,_).

chebi_no_parse(X,N) :-
        rdf(X,rdfs:label,N),
        rdf_global_id(noparse:_,X).

% definition for a class as an atom
has_def(X,Def) :-
        rdf(X,def:'',DefLit),        
        ensure_atom(DefLit,Def).


%% mf_reaction(?GoCls, ?GoDef, ?ReactionExpr, ?Score, ?Balance) is nondet
%
%    find best candidate reaction for GO def
mf_reaction(X,Def,R,S,B) :-
        candidate_mf_reaction(X,Def,R,S,B),
        \+ ((candidate_mf_reaction(X,_,_R2,S2,_),
             S2 > S)).

% shortcut for above
mf_reaction(X,R) :-
        mf_reaction(X,_Def,R,_S,_B).


:- table candidate_mf_reaction/5.
%! candidate_mf_reaction(?Actvity, ?Def:atom, ?ReactionExpr, ?Score, ?Balanced) is nondet
candidate_mf_reaction(X,Def,R,S,B) :-
        has_def(X,Def),
        defn_reaction(Def,R,S,B).

:- table defn_reaction/4.
%! defn_reaction(+Def:atom, ?ReactionExpr, ?Score, ?Balanced) is nondet
%
%   parse a text def to a reaction.
%
%   The parse is two-part
%    1. The text is parsed using a DCG
%    2. The elements of the parse tree are mapped to chemical IDs
%
%   Both steps are non-deterministic, so this goal may succeed
%   multiple times with different parse trees and different 
%   chebi IDs.
%   
%   Each such parse is scored, CHEBI labels are scored more highly
defn_reaction(Def,R,S,B) :-
        atom_codes(Def,Codes),
        phrase(godef(R1), Codes),
        match_chemicals(R1,R,S1),
        balance(R,B,Penalty),
        S is S1-Penalty.


%mf_reaction_det(X,R) :- mf_reaction(X,R),!.



% finds reaction balance        
balance(React,B,Penalty) :-
        reaction_lr(React, L, R),
        maplist([C/_, Q]>>chemical_elqs(C,Q),L,QL),
        maplist([C/_, Q]>>chemical_elqs(C,Q),R,QR),
        sum_all_elqs(QL,SumL),
        sum_all_elqs(QR,SumR),
        diff_all_elqs([SumL,SumR],B),
        maplist([_-Q,Q]>>true,B,Qs),
        sumlist(Qs,Penalty),
        !.
balance(_,'?',-50).

%% sum_all_elqs(+ListOfElQs:list, ?ElQs:list)
%
% E.g. [[h-2,o-1],[o-2]] -> [h-2,o-3]
sum_all_elqs(XL,X) :-
        agg_all_elqs(+,XL,[],X).

%% diff_all_elqs(+ListOfElQs:list, ?ElQs:list)
diff_all_elqs(XL,X) :-
        agg_all_elqs(-,XL,[],X).

%% agg_all_elqs(+Operator, +ListOfElQs:list, +Accumulator:list, ?ElQs:list)
agg_all_elqs(_,[],X,X).
agg_all_elqs(Op,[X|XL],Acc,Final) :-
        agg_elqs(Op,X,Acc,X2),
        agg_all_elqs(Op,XL,X2,Final).

agg_elqs(_Op,[],Y,Y).
agg_elqs(Op,[C-N1|L],Y,Z) :-
        select(C-N2,Y,Y2),
        !,
        OpTerm =.. [Op,N1,N2],
        N is OpTerm,
        agg_elqs(Op,L,[C-N|Y2],Z).
agg_elqs(Op,[X|L],Y,Z) :-
        agg_elqs(Op,L,[X|Y],Z).


%% match_chemicals(+ReactionOrSide, ?ReactionOrSideMatched, ?Score) is nondet
match_chemicals(React, React2, Score) :-
        reaction_lr(React, L, R, Op),
        match_chemicals(L,L2,ScoreL),
        match_chemicals(R,R2,ScoreR),
        Score is ScoreL + ScoreR,
        reaction_lr(React2, L2, R2, Op).

match_chemicals(TL, ClsLabelList, Score) :-
        TL=[_|_],
        maplist([Tok-N, Score-(ClsLabel-N)]>>match_chemical_expr(Tok,ClsLabel,Score),TL,Pairs),
        maplist([Score-_,Score]>>true,Pairs,Scores),
        maplist([_-ClsLabel,ClsLabel]>>true,Pairs,ClsLabelList),
        sumlist(Scores,TotScore),
        length(Scores,N),
        Score is TotScore/N.

match_chemical_expr(Tok / Mod, Cls/Label, Score) :-
        !,
        % matches modified expression
        atom_codes(ModA, Mod),
        match_chemical(Tok, BaseCls/Label1, Score),
        create_expression(BaseCls,ModA,Cls),
        concat_atom([Label1,ModA],' ',Label).

match_chemical_expr(Tok, Cls/Label, Score) :-
        match_chemical(Tok, Cls/Label, Score).
            
% basic match
match_chemical(Tok, Cls/Label, Score) :-
        atom_codes(Term,Tok),
        basic_annot(Cls,P,Term,_),
        pred_score(P,Score),
        get_label(Cls, Label).

% no parse
match_chemical(Tok, URI/Term, -100) :-
        atom_codes(Term,Tok),
        \+ basic_annot(_,_,Term,_),
        rdf_global_id(noparse:Term,URI),
        rdf_assert(URI, rdfs:label, Term@en).


% some parses result in chemical terms like "foo (modifier)"
% we create a dynamic expression for these
create_expression(BaseCls,Mod,Cls) :-
        concat_atom([BaseCls, ' (',Mod,')'],A),
        rdf_global_id(modified_chemical:A,Cls).
                                % TODO - assert equivalence axiom

tokenize_chemical(N,Toks) :-
        atom_codes(N,Codes),
        phrase(chemtokens(Toks),Codes).

/*
chemids_lexmatch(L1,L2,S) :-
        solutions(S1,(member(X,L1),
                      aggregate(max(S1),((member(Y,L2),chem_cls_pair_lexmatch(X,Y
*/

chemterm_lexmatch(N1,N2,S) :-
        tokenize_chemical(N1,Toks1),
        tokenize_chemical(N2,Toks2),
        setof(X,(member(X,Toks1),member(X,Toks2)),Both),
        setof(X,(member(X,Toks1);member(X,Toks2)),Union),
        length(Both,LenBoth),
        length(Union,LenUnion),
        LenUnion > 0,
        S is LenBoth / LenUnion.

get_label(Cls,Label) :-
        rdf(Cls,rdfs:label,LabelLit),
        ensure_atom(LabelLit,Label),
        !.
get_label(Cls,Cls).

% chebi syns are undistinguished
pred_score(label,10) :- !.
pred_score(exact,8) :- !.
pred_score(related,8) :- !.
pred_score(_,-1) :- !.



%% chemical_elqs(+C,?ElQs:list) is semidet
%
%  E.g. CHEBI:water -> [H-2, O-1]
%
% Note semideterministic assumption depends on
% assumption that CHEBI formula property is functional
chemical_elqs(C,ECs) :-
        rdf(C,'http://purl.obolibrary.org/obo/chebi/formula',FLit),
        ensure_atom(FLit,F),
        string_codes(F,Codes),
        phrase(formula(ECs),Codes).

:- table inchikey/2.
inchikey(C,K) :- rdf(C,'http://purl.obolibrary.org/obo/chebi/inchikey',Lit), ensure_atom(Lit,K).

%% reaction_signature(+Reaction, ?Signature) is det
% 
% calculate an InChI key signature for reaction
%
:- table reaction_signature/2.
reaction_signature(React,S) :-
        (   var(React)
        ->  mf_reaction(_,_,React,_,_)
        ;   true),
        reaction_lr(React, LS, RS),
        side_signature(LS, LSig),
        side_signature(RS, RSig),
        concat_atom([LSig,RSig],'==',S).

reaction_lr(React,L,R) :- reaction_lr(React,L,R,_).
reaction_lr(L=R,L,R,=).
reaction_lr(L<=>R,L,R,<=>).
reaction_lr(L->R,L,R,->).


side_signature(Qs,S) :-
        maplist([C/_-N, NK] >> (inchikey(C,K),concat_atom([N,K],':',NK)),Qs,Keys),
        sort(Keys,SKeys),
        concat_atom(SKeys,'.',S).

identical_signature(A1,A2,R1,R2) :-
        mf_reaction(A1,R1),
        reaction_signature(R1,S),
        mf_reaction(A2,R2),
        R1\=R2,
        reaction_signature(R2,S).
identical_signature(R1,R2) :-
        reaction_signature(R1,S),
        reaction_signature(R2,S),
        R1\=R2.


reaction_sides(Re,Op,L,R) :-
        Re =.. [Op,L,R].

reaction_participants(Re,Ps) :-
        reaction_sides(Re,_,L,R),
        side_participants(L,Ps1),
        side_participants(R,Ps2),
        append(Ps1,Ps2,Ps).

reaction_participant(Re,P) :-
        reaction_participants(Re,Ps),
        member(P,Ps).

side_participants(Side,Ps) :-
        findall(C, member(C/_-_,Side), Ps).



% ----------------------------------------
% matching rhea
% ----------------------------------------

rhea2xref(R,X,Base) :-
        rdf(R,rdfs:seeAlso,U),
        atom_concat(Base,X,U).
rhea2xref(R,X,Base) :-
        Base = 'http://purl.uniprot.org/enzyme/',
        rdf(R,rh:ec,U),
        atom_concat(Base,X,U).
rhea2xref(R,X,Base) :-
        Base = 'http://purl.uniprot.org/uniprot/',
        rhea2uniprot(N,X),
        atom_number(A,N),
        rdf_global_id(rh:A,R).
rhea2xref(R,X,Base) :-
        Base = 'http://purl.obolibrary.org/obo/GO_',
        cls_rhea_xref_uri(X,R),
        owl:class(X).

rhea2xref_db(R,X,DB) :-
        rhea2xref(R,X,Base),
        base_prefix(Base,DB).
base_prefix('http://identifiers.org/biocyc/META',metacyc).
base_prefix('http://identifiers.org/biocyc/ECO',ecocyc).
base_prefix('http://identifiers.org/kegg.reaction/',kegg).
base_prefix('http://identifiers.org/enzyme/',ec).
base_prefix('http://purl.uniprot.org/uniprot/',uniprot).
base_prefix('http://purl.obolibrary.org/obo/GO_',go).



%! new_rhea_match(?Activity, ?ReactionExpr, ?Xref, ?Score, ?M) is nondet
new_rhea_match(A,Re,X,S,M) :-
        catalytic_activity(A),
        \+ cls_rhea_xref_uri(A,_),
        rhea_match(A,Re,X,S,M,matched).

% e.g "RHEA:123" -> URI
expand_xref(X,URI) :-
        ensure_atom(X,X2),
        concat_atom(['RHEA',Local],':',X2),
        rdf_global_id(rh:Local,URI).

cls_rhea_xref_uri(C,X) :-
        rdf(C,oio:hasDbXref,XS^^_),
        expand_xref(XS,X).

%! check_rhea(?GoCls, ?ReactionExpr, ?RheaXref, ?Match, ?Info) is nondet
%! check_rhea(+GoCls, ?ReactionExpr, +RheaXref, ?Match, ?Info) is det
%
%  for any GoClas-RheaXref pair (where the xref must be assigned in the ontology),
%  validate the xref. First parse the definition for the GO Class, then try and
%  match the participants.
%
%  Match = matched | nomatch | no_reaction_parse
%
%  if nomatch, then Info = list of all rhea participants
%  if match, then Info = list of any assumed CHEBI synonyms (for unparsed elements)
check_rhea(C,Re,X,M,Info) :-
        cls_rhea_xref_uri(C,X),
        best_rhea_match(C,Re,X,_S,M,Info).

best_rhea_match(C,Re,X,S,M,Info) :-
        rhea_match(C,Re,X,S,M,Info),
        \+ ((rhea_match(C,_,X2,S2,_,_),
             X2\=X,
             S2 > S)),
        !.
best_rhea_match(C,Re,_,0,[],nomatch) :-
        mf_reaction(C,Re),
        !.
best_rhea_match(_,no_reaction,_,0,[],no_reaction_parse) :-
        !.


:- table rhea_match/5.
%! rhea_match(+Cls, ?ReactionExpr, ?Xref, ?Score, ?Match, ?Type) is nondet
rhea_match(C,Re,X,S,M,Type) :-
        %cls_rhea_xref_uri(C,X),
        candidate_mf_reaction(C,_,Re,S1,_),
        align_reaction(Re,X,M,Type),
        (   Type=matched
        ->  S is S1 + 10
        ;   S=S1).

%! align_reaction(+ReactionExpr, ?Xref, ?Match, ?Type) is nondet
%
%  given a reaction expression, find a matching RHEA reaction, where the participants
%  strictly match
align_reaction(Re,X,M,Type) :-
        reaction_participants(Re,Ps),
        rhea:reaction(X),
        setof(XP,rhea:reaction_chebi_participant(X,XP),XPs),
        %debug(rhea,'Attempting to match ~w == ~w',[Ps,XPs]),
        strict_match_participants_det(Ps,XPs,M,Type),
        !.

strict_match_participants_det(Ps,XPs,M,Type) :-
        strict_match_participants(Ps,XPs,M,Type),
        !.
strict_match_participants_det(_,XPs,XPs,nomatch) :-
        !.

%align_reaction(_,_,[],nomatch).


strict_match_participants([],[],[],matched) :- !.
strict_match_participants(_,[],[],left_more_than_right) :- !.
strict_match_participants([],_,[],right_more_than_left) :- !.
strict_match_participants([P|Ps],XL,Ms2,S) :-
        match_participant(P,XL,XL2,M),
        strict_match_participants(Ps,XL2,Ms,S),
        append(M,Ms,Ms2).

match_participant(P,L,L2,[]) :-
        % exact match
        select(P,L,L2),!.
match_participant(P,L,L2,[Term=P2]) :-
        rdf_global_id(noparse:Term,P),
        % for a non-parsed term, find the best match
        setof(S-Px,chemterm_lexmatch_id(Term,Px,L,S),Pairs),
        reverse(Pairs,[_-P2|_]),
        select(P2,L,L2).

best_chemterm_lexmatch_id(Term,X,L,S) :-
        setof(S-P,chemterm_lexmatch_id(Term,P,L,S),Pairs),
        reverse(Pairs,[S-X|_]).
        
chemterm_lexmatch_id(Term,X,L,S) :-
        member(X,L),
        basic_annot(X,_,Term2,_),
        chemterm_lexmatch(Term,Term2,S).

setif(G,T,T,_) :- G,!.
setif(_,F,_,F).


%! rhea_derived_synonym(ChebiCls:atom, RheaName:atom, Info:info, Score:float) :-
rhea_derived_synonym(Cls,N,Info,Score) :-
        rhea_derived_synonym(_R,Cls,N,Info,Score).
rhea_derived_synonym(R,Cls,N,Info,Score) :-
        rhea_label_chebi(R,N,Cls),
        \+ basic_annot(Cls,_,N,_),
        setif((basic_annot(XCls,_,N,_)),
              Info,ambiguous(XCls),newsyn),
        setif(best_chemterm_lexmatch_id(N,_,[Cls],Score),
              Score,Score,0).

rhea_label_chebi(R,N,Cls) :-
        rhea:reaction_participant(R,P),
        rdf(P,rh:compound,C),
        rdf(C,rh:chebi,Cls),
        rdf(C,rh:name,NLit),
        ensure_atom(NLit,N1),
        (   atom_concat('a ',N,N1)
        ->  true
        ;   N=N1).


compare_rhea_chebi_names(RN,Pred,Xs,Cls,Ambigs,AmbigsRelations) :-
        rhea_label_chebi(_,RN,Cls),
        compare_label(Cls,RN,Pred,Xs),
        (   Pred=label
        ->  Ambigs=[]
        ;   solutions(P2+C2,(basic_annot(C2,P2,RN,_),C2\=Cls),Ambigs)),
        findall(R,((member(_+AmbigC,Ambigs),
                    relationship_to(AmbigC,Cls,R))),
                AmbigsRelations).

relationship_to(A,A,identical_to) :- !.
relationship_to(A,B,direct_subClassOf) :-
        subClassOf(A,B),
        !.
relationship_to(A,B,indirect_subClassOf) :-
        rdfs_subclass_of(A,B),
        !.
relationship_to(A,B,R) :-
        rdfs_subclass_of(A,AZ),
        owl_some(AZ,R,BZ),
        rdfs_subclass_of(B,BZ),
        !.
relationship_to(A,B,direct_superClassOf) :-
        subClassOf(B,A),
        !.
relationship_to(A,B,indirect_superClassOf) :-
        rdfs_subclass_of(B,A),
        !.
relationship_to(B,A,R) :-
        rdfs_subclass_of(A,AZ),
        owl_some(AZ,R,BZ),
        rdfs_subclass_of(B,BZ),
        !.
relationship_to(_,_,no_relationship) :- !.



% reification on subject and target
so_reif_xref(S,T,X) :-
        rdf(Ax,owl:annotatedSource,S),
        rdf(Ax,owl:annotatedTarget,T),
        rdf(Ax,oio:hasDbXref,X).
compare_label(Cls,N,Pred,Xs) :-
        basic_annot(Cls,Pred,N,_),
        !,
        atom_string(N,S),
        solutions(X,so_reif_xref(Cls,S,X),Xs).
compare_label(_,_,nomatch,[]).




% ----------------------------------------
% transformations
% ----------------------------------------

% find mappings between elements on L and elements on R
reaction_tmap(Re,M,S) :-
        reaction_sides(Re,_,L,R),
        reaction_tmap(L,R,M,S).

reaction_tmap(L,R,M,S) :-
        solutions(S-M,reaction_tmap_combo(L,R,M,S),Pairs),
        reverse(Pairs,[S-M|_]).
        
reaction_tmap_combo(L,R,M,S) :-
        debug(tmap, 'L= ~w',[L]),
        debug(tmap, 'R= ~w',[R]),
        maplist({R}/[X,X-Combo] >> powerset_member_nonempty(R,Combo), L, Pairs),
        debug(tmap, 'Pairs= ~w',[Pairs]),
        solutions(X-Y,(member(X-Ys,Pairs),member(Y,Ys)),M),
        \+ many2many(M),
        score_tmap(M,L,R,S).

powerset_member_nonempty(Side,Combo) :-
        oset_power(Side,PSet),
        member(Combo,PSet),
        Combo\=[].
        

many2many(M) :-
        member(X-Y,M),
        member(X-Y2,M),
        Y2\=Y,
        member(X2-Y,M),
        X2\=X.

score_tmap(M,L,R,S) :-
        debug(tmap, 'Scoring tmap: ~w',[M]),
        findall(S,score_tmap_member(M,L,R,S),Ss),
        average(Ss,S).
score_tmap_member(M,L,_R,S) :-
        member(X,L),
        solutions(Y,member(X-Y,M),Ys),
        score_transform([X],Ys,S).
score_tmap_member(M,_L,R,S) :-
        member(Y,R),
        solutions(X,member(X-Y,M),Xs),
        score_transform(Xs,[Y],S).

wvar(W1,W2,WDiff) :-
        TW is W1 + W2,
        TW > 0,
        !,
        WDiff is abs((W1-W2)/TW).
wvar(_,_,0).

score_transform([],_,-1) :- !.
score_transform(_,[],-1) :- !.
score_transform(L1,L2,S) :-
        sumweights(L1,W1),
        sumweights(L2,W2),
        wvar(W1,W2,WDiff),
        findall(Sim,(member(X/_-_,L1),member(Y/_-_,L2),chemsim(X,Y,Sim)),Sims),
        average(Sims,AvgSim),
        S is AvgSim - WDiff.

chemsim(X,Y,1) :- subclass_of_some(X,_,Y),!.
chemsim(X,Y,1) :- subclass_of_some(Y,_,X),!.
chemsim(_,_,0).

sumweights(L,TW) :-
        solutions(W,(member(X,L),elq_weight(X,W)),Ws),
        sumlist(Ws,TW).

elq_weight(X/_-N,W) :-
        rdf(X,'http://purl.obolibrary.org/obo/chebi/mass',Str^^_),
        string_to_atom(Str,A),
        atom_number(A,W1),
        W is N*W1,
        !.
elq_weight(_, -999).  % arbitrary weight for no-parses



        

        
% ----------------------------------------
% learning
% ----------------------------------------

learner:instance_feature(I,F) :- catalytic_activity(I),mf_reaction(I,R),reaction_feature(R,F).

learn_grouping(C, Rule, FP) :-
        catalytic_activity(C),
        solutions(D,(rdfs_subclass_of(D,C),mf_reaction(D,_)),Targets),
        length(Targets,Len),
        Len > 5,
        Len < 100,
        get_label(C,Lbl),
        format('Learning: ~w "~w";  Cases: ~w~n',[C,Lbl,Len]),
        learn_posneg_id3path(Targets, Rule),
        FP=1.
%        learn_posneg_simple(Targets, Rule, FP).


%! grouping_outlier(?Cls,?OutlierCls)
%
%  for a GO grouping class, find all subclasses that are outliers
%  (their average similarity to other subclasses 
grouping_outlier(C,Out) :-
        grouping_outlier(C,Out,2).
grouping_outlier(C,Out,Thresh) :-
        catalytic_activity(C),
        solutions(D,(rdfs_subclass_of(D,C),mf_reaction(D,_)),Ds),
        length(Ds,N),
        N < 100,
        find_outliers(Ds,L,Thresh),
        member(Out,L).

        
        

% Each reaction can be represented as a vector
% each column in the vector is a feature; values are typically 1/0
% we use dummy encoding and expand such that each feature is of the form
% <SIDE>_<NUMBER>_<BaseFeature>
% base features may be CHEBI ancestors, IUPAC, etc
%
% E.g. l_2_CHEBI_1234
:- table reaction_feature/2.

reaction_feature(R,F) :-
        R =.. [Op,_,_],
        direction_label(Op,F).
reaction_feature(R,Prop) :-
        R =.. [_,Side,_],
        side_feature(Side,Prop1),
        possibly_concat([l_, Prop1], Prop1, Prop).
reaction_feature(R,Prop) :-
        R =.. [_,_,Side],
        side_feature(Side,Prop1),
        possibly_concat([r_, Prop1], Prop1, Prop).

side_feature(S,Prop) :-
        member(Cls/_ - N,S),
        chemical_feature(Cls,Prop1),
        possibly_concat([n,N,'_',Prop1],Prop1,Prop).

possibly_concat(L,_,Result) :- concat_atom(L,Result).
possibly_concat(_,Result,Result).



% todo: handle modifiers
chemical_feature(Cls,Cls).
chemical_feature(Cls,Anc) :-
        % TODO: SVF
        rdfs_subclass_of(Cls, Anc), rdf_is_iri(Anc).
chemical_feature(Cls,K) :- inchikey(Cls,K).


direction_label((=),undirected).
direction_label((<=>),bidi).
direction_label((->),left_to_right).


% ----------------------------------------
% grammar
% ----------------------------------------

reaction( Eqn ) --> participantqs(L), ` `,reaction_op(Op),` `, participantqs(R), {Eqn =.. [Op,L,R] }.
reaction_op( (=) ) --> `=`.
reaction_op( (<=>) ) --> `<=>`.
reaction_op( (->) ) --> `->`.

participantqs( [P|PL] ) --> participantq(P), ` + `,!, participantqs(PL).
participantqs( [P] ) --> participantq(P).
participantq(P-N) --> count(N), ` `, !, participant(P).
participantq(P-1) --> participant(P).

%participant(PQ) --> modifier_chemical(PQ),!.
participant(P/Q) --> chemterm(P),modifier(Q),!.
participant(P) --> chemterm(P).

chemterm([C|Cs]) -->
        nonsep_strict(C),greedy(nonsep, Cs), {\+((atom_codes(A,Cs),sub_atom(A,_,_,_,' + ')))}.


modifier_chemical( [C|Cs]/Q ) -->
        nonsep_strict(C), at_least(0,nonsep, Cs), `(`, at_least(0, nonsep, Q), `)`.

modifier( Q ) -->
        `(`, modterm(Q), `)`.

modterm(`in`) --> `in`.
modterm(`out`) --> `out`.
modterm(`n`) --> `n+1`.
modterm(`n`) --> `n`.


godef( R ) --> `Catalysis of the reaction: `, reaction(R), end_of_def.
godef( R ) --> `Enables the transfer of a solute or solutes from one side of a membrane to the other according to the reaction: `, reaction(R), end_of_def.

continue_until_period --> `.`, !.
continue_until_period --> [_], continue_until_period, !.

end_of_def --> `. `, !, continue_until_period.
end_of_def --> `.`, !.
end_of_def --> `; `, !, continue_until_period.

       

element_count(C-N) --> element(C),count(N),!.
formula( [P|PL] ) --> element_count(P), formula(PL),!.
formula( [P] ) --> element_count(P).
formula( [C-1] ) --> element(C).   % sometimes the final coefficient is not listed

element(E) --> [X],{atom_codes(E,[X])}.
count(1) --> `an`.
count(1) --> `a`.
count(N) --> greedy(digit, Toks),{Toks=[_|_],number_codes(N,Toks)}.

digit(X) --> [X],{X >= 0'0, X =< 0'9 }.

% todo: consider making stricter such that + can only end a term
nonsep_strict(X) --> [X],{\+sep([X]),X\=0'+}.
nonsep(X) --> [X],{\+sep([X])}.
sep(`.`).
%sep(` `).
sep(`=`).
sep(`>`).

% ----------------------------------------
% chemical name matching
% ----------------------------------------
chemtokens([T|L]) --> chemtoken(T),at_least(1,chemdelim),!,chemtokens(L).
chemtokens([T]) --> chemtoken(T),!.
chemtokens([]) --> [].


chemtoken(A) -->  greedy(non_chemdelim, T),{atom_codes(A,T),A\=''}.
non_chemdelim(X) --> [X],{\+is_chemdelim([X])}.
chemdelim(X) --> [X],{is_chemdelim([X])}.

is_chemdelim(` `).
is_chemdelim(`-`).
is_chemdelim(`[`).
is_chemdelim(`]`).
is_chemdelim(`(`).
is_chemdelim(`)`).




        
