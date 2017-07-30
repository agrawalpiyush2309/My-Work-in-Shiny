import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns;
sns.set_style("white",{'font.family':[u'serif']})



df = pd.DataFrame({'A': {0: 'a', 1: 'b', 2: 'c'},'B': {0: 'a', 1: 'a', 2: 'c'},'C': {0: 'c', 1: 'b', 2: 'c'}})



def sankey(df):
	# Grab all unique categories
	all_labels = pd.melt(df)["value"].unique()
	# Grab colors for unique categories
	pal = "hls"
    cls = sns.color_palette(pal, len(all_labels))
        for i,l in enumerate(all_labels):
            colorDict[l] = cls[i]

    # Determine widths of individual strips
    ns = defaultdict()
    for l in all_labels:
        myD = {}
        for l2 in all_labels:
        	for column in df.columns:
            	myD[l2] = len(df[(df.before==l) & (df.after==l2)])
        ns[l] = myD
