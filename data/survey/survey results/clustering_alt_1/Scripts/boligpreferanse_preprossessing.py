# -*- coding: utf-8 -*-
"""
Created on Tue Jul 23 12:07:38 2024

@author: sioz
"""

import pandas as pd
from sklearn.preprocessing import (MinMaxScaler,RobustScaler,PowerTransformer,QuantileTransformer)
import numpy as np
import os as os

#check if the working directory points to data folder

os.getcwd()
data_path = os.path.normpath(r"C:\Users\sioz\Desktop\arbeid\prosjektene\boligpreferanse\data\survey\survey results")
os.chdir(data_path)
os.getcwd()

survey_data = pd.read_excel("./KNA135092_240702_raw.xlsx", sheet_name = 0)[1:]

colnames = survey_data.columns.tolist()

clusteringVars = [x for x in colnames if x.startswith("Q10") or x.startswith("Q20") or x.startswith("Q9")]

dfWclvars = survey_data[clusteringVars].copy()

#invert the values for "where do you live" question
#the current scale is
# 1: city senter (sentrumnært i by)
# 2: suburban(sentrumnært i tettsted)
# 3: village (i bygd)
# 4: outback (i landlig/ spredtbygd område)
# 5: other (annen)
dfWclvars["Q9"] = dfWclvars["Q9"].apply(lambda x: 6-x)

del survey_data
#initialize rescalers and transformers

def scaler(data, mode = "minmax", colnames = None):
    #initialize scalers, transformers
    mm_scaler = MinMaxScaler(copy=True)
    r_scaler = RobustScaler(with_centering=True,with_scaling=True,copy=True)
    #power tranformation strictly applied with box-cox tranformation here 
    #because the data used here has no negative values
    p_transform = PowerTransformer(method = "box-cox",standardize=True,copy=True)
    #quantile transformation is initialized strictly with normal distribution output 
    #because it is what is needed for the further analysis
    q_transform = QuantileTransformer(output_distribution="normal")
    
    if mode == "minmax":
        data = pd.DataFrame(mm_scaler.fit_transform(data),columns=colnames)
    if mode == "robust":
        data = pd.DataFrame(r_scaler.fit_transform(data),columns = colnames)
    if mode == "power":
        data = pd.DataFrame(p_transform.fit_transform(data),columns = colnames)
    if mode == "quant":
        data = pd.DataFrame(q_transform.fit_transform(data))
    
    return data




scaling_modes = ["minmax","robust","power","quant"]

for modus in scaling_modes:
    transformed_data = scaler(dfWclvars,mode=modus,colnames = clusteringVars)
    data_name = "clustering_data_"+ modus +".csv"
    transformed_data.to_csv(data_name,index=False)
    