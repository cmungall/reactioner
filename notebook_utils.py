import pandas as pd

from matplotlib_venn import venn2, venn2_circles
from matplotlib_venn import venn3, venn3_circles
from matplotlib import pyplot as plt

def create_venn(df, c1, c2, c3, idcol=None, title=""):
    s1 = set()
    s2 = set()
    s3 = set()
    for i,r in df.iterrows():
        if idcol is not None:
            ix = r[idcol]
        else:
            ix = i
        if r[c1] != "":
            s1.add(ix)
        if r[c2] != "":
            s2.add(ix)
        if r[c3] != "":
            s3.add(ix)
    venn3([s1, s2, s3], set_labels = (c1, c2, c3))
    plt.title(title)

import collections
import itertools
def powerset(iterable):
    s = list(iterable)
    return itertools.chain.from_iterable(itertools.combinations(s, r) for r in range(len(s)+1))

def makebool(combo, cols):
    return [c in combo for c in cols]

# https://upsetplot.readthedocs.io/en/latest/api.html
import upsetplot as usp
def create_upset(df, cols, **args):
    ps = powerset(cols)
    counts_by_combo = collections.defaultdict(int)
    for i,r in df.iterrows():
        combo = [r[c] != "" and r[c] != False and r[c] != 0 for c in cols]
        counts_by_combo[tuple(combo)] += 1
 
    counts = []
    boolvecs = []
    for s in ps:
        boolvec = makebool(s, cols)
        c = counts_by_combo[tuple(boolvec)]
        if c > 0:
            boolvecs.append(boolvec)
            counts.append(c)
    multiindex = pd.MultiIndex.from_tuples(boolvecs, names=cols)
    #print(multiindex)
    upsetdata = pd.Series(counts, index=multiindex).sort_values(ascending=False)
    usp.plot(upsetdata, sort_by='cardinality', **args)

