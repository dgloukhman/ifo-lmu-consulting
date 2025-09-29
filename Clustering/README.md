## CCF-based Clustering

This section contains the script for the CCF-based clustering. Individual series are clustered, according to their CCF with the main index.

- `CCFbased_Clustering.R` : This is the main script for the analysis. First the CCF of all individual level 2 series is calculated. Then they are clustered using Ward's method, with the loop going over different numbers of resulting clusters, which is rather a rough indication to see how 'early' meaningful clusters emerge. The actual chosen number of clusters then is probably rather a question of desired interpretability, as it usually affects the cluster size. 
- `clustering_utils.R` : All relevant function for the main script. 
- `Clustering_unused`: Old scripts which did not make it into the final analysis, but kept, just in case... 

`CCFbased_Clustering.R` can be run on its own. 