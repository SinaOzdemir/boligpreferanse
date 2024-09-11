# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import os
import pandas as pd
import numpy as np
from sklearn.cluster import (KMeans,AgglomerativeClustering,AffinityPropagation,SpectralClustering)
from sklearn.metrics import confusion_matrix,ConfusionMatrixDisplay

##Set the working directory##

save_dir = os.path.normpath(r"C:\Users\sioz\Desktop\arbeid\prosjektene\boligpreferanse\results\clustering results")

if "clustering_data" not in os.getcwd():
    print("working directory is different from data directory")
    
    os.chdir(os.path.normpath(r"C:\Users\sioz\Desktop\arbeid\prosjektene\boligpreferanse\data\survey\survey results\clustering_data"))
else:
    print("working directory points to data directory (Y)")


survey_data_path = os.path.normpath(r"C:\Users\sioz\Desktop\arbeid\prosjektene\boligpreferanse\data\survey\survey results\KNA135092_240702_raw.xlsx")    

survey_data = pd.read_excel(survey_data_path,sheet_name = "KNA135092_240702_values").drop(index = 0)

##fit clustering algorithm and get the clusters##

###KMeans###

#optimum selections for KMeans:
    ##n = 2
    ##source data: min-max normalization (named max)
    ##source data preprocessing: deep_encoder

data = pd.read_csv("clustering_robust_deep_encoder.csv")
col_names = data.columns.tolist()

kmeans_fit = KMeans(n_clusters=2,
                    init="k-means++",
                    max_iter=1000,
                    algorithm="lloyd",random_state=5).fit(data)


sample_to_center_distance = pd.DataFrame(kmeans_fit.transform(data)[:,:],columns=["d_cluster_0","d_cluster_1"]).assign(membership = kmeans_fit.labels_).rename_axis("sample_id")

sample_to_center_distance.to_excel(save_dir+"/kmeans_sample_to_center_dist.xlsx")

pd.DataFrame(kmeans_fit.cluster_centers_,columns = col_names).rename_axis("cluster_id").reset_index().melt(id_vars = "cluster_id").to_excel(save_dir+"/kmeans_2clust_centers.xlsx")

survey_data = survey_data.assign(kmeans_cluster = kmeans_fit.labels_)


###AgglomerativeClustering###

#optimum selections for Agglomerative clustering:
    ## n = 2,
    ## source data: power normalization
    ## source data preprocessing: deep_encoder
    
data = pd.read_csv("clustering_robust_deep_encoder.csv")
col_names = data.columns.tolist()

agglo_fit = AgglomerativeClustering(n_clusters=2,
                                      metric = "euclidean",
                                      linkage = "ward").fit(data)

survey_data = survey_data.assign(agglomerative_cluster = agglo_fit.labels_)

###Affinity propagation###

#Optimum selections for Affinity propagation:
    ## d = .65-.7,
    ## source data = min max normalization (named max)
    ## source data preprocessing: deep_encoder
    
data = pd.read_csv("clustering_quant_deep_encoder.csv")
col_names = data.columns.tolist()

affinityProp_fit = AffinityPropagation(damping = .6,max_iter=1000,convergence_iter=15,random_state=5).fit(data)

pd.DataFrame(affinityProp_fit.cluster_centers_,columns=col_names).assign(center_indices = affinityProp_fit.cluster_centers_indices_).rename_axis("cluster_id").reset_index().to_excel(save_dir+"/affinityProp_cluster_centers.xlsx")

survey_data = survey_data.assign(AffinityProp_cluster = affinityProp_fit.labels_)

###Spectral clustering###

#Optimum selections for spectral clustering:
    ## n = 2
    ## source data = power
    ## source processing = deep_encoder
    
data = pd.read_csv("clustering_robust_deep_encoder.csv")
col_names = data.columns.tolist()

SpecClust_fit = SpectralClustering(n_clusters = 2,
                                assign_labels = "cluster_qr").fit(data)

survey_data = survey_data.assign(SpecClust_cluster = SpecClust_fit.labels_)

survey_data.to_excel(save_dir+"/survey_data_w_clusters.xlsx")
## Check the agreement between algorithms##
ConfusionMatrixDisplay(confusion_matrix(survey_data["kmeans_cluster"], survey_data["agglomerative_cluster"],normalize="all")).plot()
ConfusionMatrixDisplay(confusion_matrix(survey_data["kmeans_cluster"], survey_data["SpecClust_cluster"],normalize="all")).plot()
ConfusionMatrixDisplay(confusion_matrix(survey_data["agglomerative_cluster"], survey_data["SpecClust_cluster"],normalize="all")).plot()

