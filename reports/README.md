## GO Text Defs that cannot be parsed

 * [go-no-parse.tsv](go-no-parse.tsv)

Sample:

|class|class label|textdef|rhea|ec|metacyc
|---|---|---|---|---|---|
|GO:0009975|cyclase activity|Catalysis of a ring closure reaction.|||
|GO:0016787|hydrolase activity|Catalysis of the hydrolysis of various bonds, e.g. C-O, C-N, C-C, phosphoric anhydride bonds, e|c. Hydrolase is the systematic name for any enzyme of EC class 3.||EC:3|
|GO:0032451|demethylase activity|Catalysis of the removal of a methyl group from a substrate.|||
|GO:0140098|catalytic activity, acting on RNA|Catalytic activity that acts to modify RNA.|||
|GO:0019156|isoamylase activity|Catalysis of the hydrolysis of alpha-(1,6)-D-glucosidic branch linkages in glycogen, amylopect|n and their beta-limits dextrins.||EC:3.2.1.68|MetaCyc:3.2.1.68-RXN|MetaCyc:RXN-12280|MetaCyc:RXN-14380


## Unmatchable reaction participants

 * [chebi-no-match.tsv](chebi-no-match.tsv)

Sample:

|class|class label|def|unmatched_term|rhea_chebi_id|rhea_chebi_id label|info|score|rhea|ec|metacyc
|---|---|---|---|---|---|---|---|---|---|---|
|GO:0033972|proclavaminate amidinohydrolase activity|Catalysis of the reaction: amidinoproclavaminate + H(2)O = proclavaminate|+ urea.|H(2)O|||||||
|GO:0047812|D-amino-acid N-acetyltransferase activity|Catalysis of the reaction: acetyl-CoA + a D-amino acid = CoA + an N-acet|l-D-amino-acid.|D-amino acid|CHEBI:59871|D-alpha-amino acid zwitterion|newsyn|0.75|||
|GO:0050490|1,4-lactonase activity|Catalysis of the reaction: H2O + a 1,4-lactone = a 4-hydroxyacid.|4-hydroxyacid|CHEBI:13659||4-hydroxy monocarboxylic acid anion|newsyn|0.6666666666666666|||
|GO:0018815|3-methyl-5-hydroxy-6-(3-carboxy-3-oxopropenyl)-1H-2-pyridon hydratase-aldolase activity|Catalysis of the reaction: 3-methyl-5-hydroxy-6-(3-carboxy-3-oxopropenyl)-1H-2-pyridon + H2O = 2-oxobut-3-enanoate + 2,5,6-trihydroxy-3-methylpyridine.|3-methyl-5-hydroxy-6-(3-carboxy-3-oxopropenyl)-1H-2-pyridon|||||||^M|


## Suggested new CHEBI syns based on RHEA usage

 * [go-rhea-newsyns.tsv](go-rhea-newsyns.tsv)

Sample:

|class|class label|rhea_name|info|score|ambiguous_with|ambiguous_with label
|---|---|---|---|---|---|---|
|CHEBI:138951|3'-hydroxy-5'-unsubstituted flavanone|3'-hydroxy,5'-unsubstituted flavanone|newsyn|0.8||
|CHEBI:57930|nucleoside 5'-diphosphate(3-)|ribonucleoside 5'-diphosphate|ambiguous(http://purl.obolibrary.org/obo/CHEBI_37075)|0.75|CHEBI:37075|ribonucleoside 5'-diphosphate
|CHEBI:58608|1,2-diacyl-sn-glycerol 3-phosphate(2-)|1,2-diacyl-sn-glycero-3-phosphate|newsyn|0.8571428571428571||
|CHEBI:29067|carboxylic acid anion|carboxylate|newsyn|0.5||
|CHEBI:29067|carboxylic acid anion|carboxylate|newsyn|0.5||
|CHEBI:59871|D-alpha-amino acid zwitterion|D-amino acid|newsyn|0.75||
|CHEBI:85638|haloacetate(1-)|haloacetate|newsyn|0.5||
|CHEBI:65317|2'-deoxynucleoside 5'-monophosphate(2-)|2'-deoxyribonucleoside 5'-phosphate|ambiguous(http://purl.obolibrary.org/|bo/CHEBI_37016)|0.8|CHEBI:37016|2'-deoxyribonucleoside 5'-phosphate
|CHEBI:17615|1,2-diacyl-3-beta-D-galactosyl-sn-glycerol|1,2-diacyl-3-O-(beta-D-galactosyl)-sn-glycerol|newsyn|0.9||

## Align xreffed RHEA reaction to parsed GO def

 * [go-rhea-check.tsv](go-rhea-check.tsv)

Sample:

|class|class label|def|op|goleft|goright|rhea|rhea label|is_match|info|info label
|---|---|---|---|---|---|---|---|---|---|---|
|GO:0010280|UDP-L-rhamnose synthase activity|Catalysis of the reaction: UDP-D-glucose + NADPH + H+ = UDP-L-rhamnose + NADP+ + |2O.||||||||
|GO:0004383|guanylate cyclase activity|Catalysis of the reaction: GTP = 3',5'-cyclic GMP + diphosphate.|=|http://purl.obolibrary.org/obo/CHEBI_37565/GTP(4-)-1|http://purl.obolibrary.org/obo/CHEBI_57746/3',5'-cyclic GMP(1-)-1|http://purl.obolibrary.org/obo/CHEBI_33019/diphosphate(3-)-1|rh:13665|GTP = 3',5'-cyclic GMP + diphosphate|matched||^M|
|GO:0035439|halimadienyl-diphosphate synthase activity|Catalysis of the reaction: geranylgeranyl diphosphate = halima-5(6),13-dien-15-yl diphosphate.|=|http://purl.obolibrary.org/obo/CHEBI_57533/geranylgeranyl diphosphate(3-)-1|http://x.org/noparse#halima-5(6),13-dien-15-yl diphosphate/halima-5(6),13-dien-15-yl diphosphate-1|rh:25621|geranylgeranyl diphosphate = tuberculosinyl diphosphate|matched|halima-5(6),13-dien-15-yl diphosphate=http://purl.obolibrary.org/obo/CHEBI_58822|^M|
|GO:0046423|allene-oxide cyclase activity|Catalysis of the reaction: (9Z,13S,15Z)-12,13-epoxyoctadeca-9,11,15-trienoate = (15Z)-12-oxophyto-10,15-dienoate.|=|http://purl.obolibrary.org/obo/CHEBI_36438/(9Z,13S,15Z)-12,13-epoxyoctadeca-9,11,15-trienoate-1|http://purl.obolibrary.org/obo/CHEBI_57411/(15Z)-12-oxophyto-10,15-dienoate-1|rh:22592|(9Z,13S,15Z)-12,13-epoxyoctadeca-9,11,15-trienoate = (10Z,15Z)-12-oxophytodienoate|matched||^M|
|GO:0047768|carboxy-cis,cis-muconate cyclase activity|Catalysis of the reaction: 3-carboxy-2,5-dihydro-5-oxofuran-2-acetate = 3-carboxy-cis,cis-muconate.|=|http://purl.obolibrary.org/obo/CHEBI_57976/2-(carboxylatomethyl)-5-oxo-2,5-dihydrofuran-2-ide-3-carboxylate-1|http://purl.obolibrary.org/obo/CHEBI_15749/3-carboxy-cis,cis-muconic acid-1|rh:14977|3-carboxy-2,5-dihydro-5-oxofuran-2-acetate = 3-carboxy-cis,cis-muconate|nomatch|http://purl.obolibrary.org/obo/CHEBI_57496|http://purl.obolibrary.org/obo/CHEBI_57976|3-carboxy-cis,cis-muconate(3-)|2-(carboxylatomethyl)-5-oxo-2,5-dihydrofuran-2-ide-3-carboxylate^M|
