# -*- coding: utf-8 -*-
"""
Created on Mon Jul 29 09:51:19 2024

@author: sioz
"""

#Clustering and finding "right" K 

## Potential clustering algos:
    #1. Gaussian/Bayesian expectation maximization generalized mixture models
    #2. K means
    #3. Agglomerative/hiearchical (no need to pre-K but still useful to compare to find the opt cutoff)
    #4. DBSCAN/OPTICS (optics dont need pre k) (never used it, not clear on the algo)
    #5. Spectral (pretty good at finding non-linera clusters)
    #6. Mean-shift (again never used, so not really sure how the algo works)
    #7. Affinity propagation (no need for pre-K, requires damping factor tuning?)
        #Damping factor in the range [0.5, 1.0) is the extent to which the current value
        #is maintained relative to incoming values (weighted 1 - damping).
        #This in order to avoid numerical oscillations when updating these values (messages).
    #8. BIRCH (type of agglo clustering, sensitive to data order, needs branching factor and treshold[optimum trimming treshhol])

## Potential evaluation metrics:
    #1. Silhouette Score
    #2. Davis-Bouldin
    #3. Calinski-Harabasz
    #4. Elbow (graphical method)
    #5. Gap (also graphical method)
    #6. DBSCAN/OPTICS specific methods (f.e sklearn.metrics.cluster_optics_dbscan and sklearn.metrics.cluster_optics_xi)
 #NBClust in R offers a wider range of selection criteria but doesnt accommodate all alternative clustering algos

#packages
##util packages
import os
import pandas as pd
import numpy as np
import re
import warnings
##analysis packages
from sklearn.cluster import (KMeans,
                             AgglomerativeClustering,
                             OPTICS,#not used
                             DBSCAN,#not used
                             SpectralClustering,
                             MeanShift,AffinityPropagation,
                             Birch)#not used
from sklearn.metrics import (silhouette_score,davies_bouldin_score,calinski_harabasz_score)

#os.environ["OMP_NUM_THREADS"]=2

warnings.filterwarnings("ignore")#variables also dont show up in the variable explorer with this 

##Specifications:
##  - milesgranger::gap-stat -> python[version='>=3.4,<3.5.0a0|>=3.6,<3.7.0a0|>=3.5,<3.6.0a0']

## Your python: python=3.10
## Sikicem pythonin bu huyunu ama ya. Use other scores to find optimum Ks for now.

################
#Implementation#
################

##search function##

def optim_k(data,clustering_algorithm,search_bounds=None):
    
    def k_score(data,data_labels,algo,search_bounds,metric = None):
        
        if metric is None:
            metric = "euclidean"
            
        scores = {"clustering_algo":[algo],
                  "search_param":[search_bounds],
                  "silhouette_score":[round(silhouette_score(data, labels = data_labels,metric = metric),ndigits=2)],
                  "davies_bouldin_score":[round(davies_bouldin_score(data,labels = data_labels),ndigits=2)],
                  "calinsku_harabasz_score":[round(calinski_harabasz_score(data, labels=data_labels),ndigits=2)]}
        
        return scores
    
    
    if clustering_algorithm == "kmeans":
        clustering = KMeans(n_clusters=search_bounds,init="k-means++",max_iter=1000,algorithm="lloyd",random_state=5).fit(data)
        
        
    if clustering_algorithm == "Agglomerative":
        clustering = AgglomerativeClustering(n_clusters=search_bounds,
                                              metric = "euclidean",
                                              linkage = "ward"
                                              ).fit(data)
        
    if clustering_algorithm == "SpectralClustering":
       #"labels are assigned with cluster_qr, all else are hold at default"
        clustering = SpectralClustering(n_clusters = search_bounds,
                                        assign_labels = "cluster_qr").fit(data)
        
    if clustering_algorithm == "MeanShift":
        #Bandwith, kernel initialization seeds are calculated from the data.","Outlying samples are not coerced to a cluster",sep="\n"
        clustering = MeanShift(cluster_all=False,#outliers are not forced to a cluster this way,gets label -1  
                             max_iter=1000).fit(data) #small iteration size for prototyping, still comparatively slower than others so far
        
    
    
    
    if clustering_algorithm == "Affinity_propagation":
        
        #searching best damping parameter
        affinity_propscore = pd.DataFrame()
        for d in np.arange(0.5,1,0.1):
            
            clustering = AffinityPropagation(damping = d,max_iter=1000,convergence_iter=15,random_state=5).fit(data)
            
            if len(set(clustering.labels_))==1:
                k_scores = pd.DataFrame({"clustering_algo":[clustering_algorithm],
                          "search_param":[d],
                          "silhouette_score":[None],
                          "davies_bouldin_score":[None],
                          "calinsku_harabasz_score":[None]})
                affinity_propscore = affinity_propscore._append(k_scores,ignore_index=True)
            else:
                
                
                k_scores = k_score(data = data, data_labels = clustering.labels_, algo = clustering_algorithm, search_bounds = d)
                
                affinity_propscore = affinity_propscore._append(k_scores,
                                                                ignore_index= True)
        
        return(affinity_propscore)

        ### this should retun a larger dictionary rather than a data frame so that I can bind it to a df at the end 
        ### and return the whole thing.
    
    else:
        param_res = pd.DataFrame(k_score(data= data,
                            data_labels = clustering.labels_,
                            algo = clustering_algorithm,search_bounds= search_bounds))
        return(param_res)
        

        
        
##necessary utils for looping

if "clustering_data" not in os.getcwd():
    print("wrong working directory. Chaning it to data folder")
    os.chdir(os.path.normpath(r"C:\Users\sioz\Desktop\arbeid\prosjektene\boligpreferanse\data\survey\survey results\clustering_data"))
else:
    print("working directory is the data folder. Go on, git")

clustering_data_dir = [f for f in os.listdir(os.getcwd()) if ".csv" in f]

save_dir = os.path.normpath(r"C:\Users\sioz\Desktop\arbeid\prosjektene\boligpreferanse\data\survey\survey results\algo_test")


clus_algos = ["kmeans","Agglomerative","SpectralClustering","MeanShift","Affinity_propagation"]


for data in clustering_data_dir:
    
    clustering_data_name = re.sub("clustering_|data_|.csv","",data)
    
    clustering_data = pd.read_csv(data,sep = ",")
    
    print("searching optimum number of clusters and algorithms for:",clustering_data_name,sep = "\t")
    
    for algo in clus_algos:
        print("applying:", algo,sep="\t")
        save_name = clustering_data_name+"_optimKandA"+".csv"
        
        if os.path.exists(save_dir+"/"+save_name):
            mode_setting = "a"
            header_setting = False
        else:
            mode_setting = "w"
            header_setting = True
       
        #these dont require apriori K so they are one shot
        if algo in ["MeanShift","Affinity_propagation"]:
            optimal_K = optim_k(data= clustering_data, clustering_algorithm = algo)
            
            optimal_K.to_csv(save_dir+"/"+save_name,
                             sep = ",",
                             index = False,
                             header = header_setting,
                             mode = mode_setting)
        else:
            print("searching for optimum number of clusters(2-20) for:",algo,sep = "\t")
            for i in range(2,21):
                optimal_K = optim_k(data = clustering_data, clustering_algorithm = algo,search_bounds= i)
                
                if os.path.exists(save_dir+"/"+save_name):
                    mode_setting = "a"
                    header_setting = False
                else:
                    mode_setting = "w"
                    header_setting = True
                
                optimal_K.to_csv(save_dir+"/"+save_name,sep = ",",index = False,header=header_setting,mode=mode_setting)     
                