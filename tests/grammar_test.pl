:- use_module(library(reactioner)).
:- use_module(library(rdf_matcher)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).

:- rdf_register_prefix(x,'http://example.org/x/').
:- rdf_register_prefix(y,'http://example.org/y/').
:- rdf_register_prefix(z,'http://example.org/z/').

:- debug(index).

:- begin_tests(grammar,
               [%setup(load_test_file_and_index),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file_and_index :-
        rdf_load('data/chebi_import.ttl.gz').




showall(G) :-
        forall(G,
               format('~q.~n',[G])).

t(C,Q,Q2) :-
        chemical_elqs(C,Q),
        diff_all_elqs([Q,Q],Q2).

xtest(chebi) :-
        showall(t(_,_,_)).

test(p) :-
        assertion( reactioner:phrase(participantq(`foo-bar`-1),`foo-bar`)).
test(ion) :-
        assertion( reactioner:phrase(participantq(`H+`-1),`H+`)).
test(ion_in) :-
        assertion( reactioner:phrase(modifier_chemical(`H+`/`in`),`H+(in)`)).

test(rnaq) :-
        assertion( reactioner:phrase(modifier_chemical( `RNA`/`n`),`RNA(n)`)).
test(rnaqn) :-
        assertion( reactioner:phrase(participantq(`RNA`/`n`-1),`RNA(n)`)).




test(pq) :-
        assertion( reactioner:phrase(participantq(`foo-bar`-15),`15 foo-bar`)).
test(space) :-
        assertion( reactioner:phrase(participantq(`1-phosphatidyl-1D-myo-inositol 3-phosphate`-1),`1-phosphatidyl-1D-myo-inositol 3-phosphate`) ).
test(a) :-
        assertion( reactioner:phrase(participantq(`1-phosphatidyl-1D-myo-inositol 3-phosphate`-1),`a 1-phosphatidyl-1D-myo-inositol 3-phosphate`) ).

test(bad_p) :-
        assertion( \+ reactioner:phrase(participantq(`a `),_)).
test(bad_p2) :-
        assertion( \+ reactioner:phrase(participantq(`a.b`),_)).
test(bad_p3) :-
        assertion( \+ reactioner:phrase(participantq(`3 a.`),_)).
test(p2) :-
        assertion( reactioner:phrase(participantq(`A`-1),`A`)).
test(participantqs) :-
        assertion( reactioner:phrase(participantqs([`A`-1,`B`-1]),`A + B`)).
test(participantqs2) :-
        assertion( reactioner:phrase(participantqs([`A`-1,`B`-2]),`1 A + 2 B`)).
test(bad_participantqs2) :-
        assertion( \+reactioner:phrase(participantqs([_]),`A + B`)).

test(participantq_1) :-
        assertion( reactioner:phrase(participantq(`glycine`-1),`glycine`)).
test(participantq_2) :-
        assertion( reactioner:phrase(participantq(`H(+)`-1),`H(+)`)).
test(participantq_3) :-
        assertion( reactioner:phrase(participantq(`succinyl-CoA`-1),`succinyl-CoA`)).
test(participantqs3) :-
        assertion( reactioner:phrase(participantqs([`glycine`-1, `H(+)`-1, `succinyl-CoA`-1]),`glycine + H(+) + succinyl-CoA`)).
test(participantqs3a) :-
        assertion( reactioner:phrase(participantqs([`glycine`-1, `succinyl-CoA`-1]),`glycine + succinyl-CoA`)).
test(participantqs3b) :-
        assertion( reactioner:phrase(participantqs([`glycine`-1, `succinyl`-1]),`glycine + succinyl`)).



test(reaction) :-
        assertion( reactioner:phrase(reaction([`pin`-1,`cushion`-1] = [`fish`-1,`chips`-1]),`pin + cushion = fish + chips`)).

test(reaction_bi) :-
        assertion( reactioner:phrase(reaction([`pin`-1,`cushion`-1] <=> [`fish`-1,`chips`-1]),`pin + cushion <=> fish + chips`)).
test(reaction_uni) :-
        assertion( reactioner:phrase(reaction([`pin`-1,`cushion`-1] -> [`fish`-1,`chips`-1]),`pin + cushion -> fish + chips`)).

test(reaction3) :-
        assertion( reactioner:phrase(reaction(_),`all-trans-hexaprenyl diphosphate + isopentenyl diphosphate = all-trans-heptaprenyl diphosphate + diphosphate`) ).

test(formula_h2o1) :-
        assertion( reactioner:phrase(formula(['H'-2,'O'-1]),`H2O1`)).
test(formula_h2o) :-
        assertion( reactioner:phrase(formula(['H'-2,'O'-1]),`H2O`)).


test(godef) :-
        assertion( reactioner:phrase(godef(_),`Catalysis of the reaction: UDP-glucose + D-fructose = UDP + sucrose.`) ).

test(godef2) :-
        assertion( reactioner:phrase(godef(_),`Catalysis of the reaction: ATP + H2O + betaine(out) = ADP + phosphate + glycine-betaine(in).`) ).

test(godef3) :-
        assertion( reactioner:phrase(godef(_),`Catalysis of the reaction: all-trans-hexaprenyl diphosphate + isopentenyl diphosphate = all-trans-heptaprenyl diphosphate + diphosphate.`) ).
test(godef4) :-
        assertion( reactioner:phrase(godef(_),`Catalysis of the reaction: (S)-2,3-epoxysqualene = lanosterol. This is a cyclization reaction that forms the sterol nucleus.`) ).
test(godef5) :-
        assertion( reactioner:phrase(godef(_),`Catalysis of the reaction: a 1-phosphatidyl-1D-myo-inositol 3-phosphate + ATP = a 1-phosphatidyl-1D-myo-inositol 3,5-bisphosphate + ADP + 2 H(+).`)).
test(godef6) :-
        assertion( reactioner:phrase(godef(_),`Catalysis of the reaction: a 3-beta-hydroxyl sterol + NADP+ = a 3-keto sterol + NADPH + H(+).`)).
test(godef7) :-
        assertion( reactioner:phrase(godef(_),`Catalysis of the reaction: UDP-galactose + 2-(2-hydroxyacyl)sphingosine = UDP + 1-(beta-D-galactosyl)-2-(2-hydroxyacyl)sphingosine.`)).






test(godef_gloss) :-
        assertion( reactioner:phrase(godef(_),`Catalysis of the reaction: ATP + H2O + betaine(out) = ADP + phosphate + glycine-betaine(in); blah blah blah.`) ).

test(chemtoken) :-
        showall( reactioner:phrase(chemtoken(_),`foo`) ),
        showall( reactioner:phrase(chemtokens(_),`a fatty acyl-[ACP]`) ),
        showall( reactioner:phrase(chemtokens(_),`a 2-acyl-sn-glycero-3-phosphoethanolamine`) ),
        showall( reactioner:phrase(chemtokens(['3','4',hydroxyphenyl,pyruvate]),`3-(4-hydroxyphenyl)pyruvate`) ),
        assertion( reactioner:phrase(chemtoken(foo),`foo`) ),
        assertion( reactioner:phrase(chemtokens([a,fatty,acyl,'ACP']),`a fatty acyl-[ACP]`) ),
        assertion( reactioner:phrase(chemtokens([a,'2',acyl,sn,glycero,'3',phosphoethanolamine]),`a 2-acyl-sn-glycero-3-phosphoethanolamine`) ),
        true.
test(chemmatch) :-
        Terms = ['a 2-acyl-sn-glycero-3-phosphoethanolamine',
                 'a 2-acyl-glycero-3-phosphoethanolamine',
                 'a 2-acyl-glycero',
                 'foo'],
        forall(member(X,Terms),
               forall((member(Y,Terms),X@=<Y),
                      showall(chemterm_lexmatch(X,Y,_)))).


:- end_tests(grammar).
    
