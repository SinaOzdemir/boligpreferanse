#clustering algo and parameter search results#
##Will be impelemented in python/pandas later##


# setup -------------------------------------------------------------------


library(pacman)

p_load(char = c("here","tidyverse"))



# data --------------------------------------------------------------------


data_dirs<- list.files(path = "C:/Users/sioz/Desktop/arbeid/prosjektene/boligpreferanse/data/survey/survey results/algo_test",
                        pattern = "*.csv",
                        full.names = T)



# prototyping -------------------------------------------------------------
## Silhouette score: [-1,1] higher values are better 
## Davies-bouldin index: the average ratio of within cluster distance to between cluster distance, smaller values often indicate a better fit
## Calinski-Harabaz: aka variance ratio criterion, measures the ratio of the sum of between-cluster dispersion and within-cluster dispersion. A higher score indicates better clustering.


df_name<- str_split_i(data_dirs[1],pattern= "/",i = -1) %>% 
  gsub("_optimKandA.csv","",.)

source_data<- str_extract(string = df_name,pattern = "max|min_max|power|quant|robust")

source_processing<- str_extract(string = df_name, pattern = "mini_encoder|mini_vencoder|deep_encoder|deep_vencoder")

test_df<- read.table(file = data_dirs[1],header = T, sep = ",",na.strings = "") %>% 
  mutate(source_data = source_data) %>% 
  drop_na(silhouette_score,davies_bouldin_score) %>% 
  mutate(source_processing = source_processing) %>%
  mutate(across(everything(),~gsub("\\]|\\[","",.x))) %>% 
  mutate(across(search_param:calinsku_harabasz_score,~as.numeric(.x))) %>% 
  filter(search_param <=10) 


search_results<- data.frame()

for (i in data_dirs) {
  df_name<- str_split_i(i,pattern= "/",i = -1) %>% 
    gsub("_optimKandA.csv","",.)
  
  print(paste0("working on the results for: ",df_name))
  
  source_data<- str_extract(string = df_name,pattern = "max|min_max|power|quant|robust")
  
  source_processing<- str_extract(string = df_name, pattern = "mini_encoder|mini_vencoder|deep_encoder|deep_vencoder")
  
  test_df<- read.table(file = i,header = T, sep = ",",na.strings = "") %>% 
    mutate(source_data = source_data) %>% 
    drop_na(silhouette_score,davies_bouldin_score) %>% 
    mutate(source_processing = source_processing) %>%
    mutate(across(everything(),~gsub("\\]|\\[","",.x))) %>% 
    mutate(across(search_param:calinsku_harabasz_score,~as.numeric(.x)))
  
  search_results<- rbind(search_results,test_df)
}

write.table(x = search_results,
            file = "C:/Users/sioz/Desktop/arbeid/prosjektene/boligpreferanse/data/survey/survey results/algo_test/search_results.csv",
            sep = ",",col.names = T,
            row.names = F,fileEncoding = "UTF-8")



# find optimum ------------------------------------------------------------


optimum_a<- search_results %>% 
  filter(search_param<=20) %>% 
  ungroup() %>% 
  group_by(clustering_algo) %>% 
  filter(silhouette_score == max(silhouette_score)) %>% 
  filter(davies_bouldin_score == min(davies_bouldin_score))

write.xlsx(x = optimum_a,file = "C:/Users/sioz/Desktop/arbeid/prosjektene/boligpreferanse/data/survey/survey results/algo_test/optimum_parameters.xlsx")