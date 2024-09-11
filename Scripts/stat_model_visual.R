## Extracting results from statistical model##


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("here","tidyverse","openxlsx"))

get_mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

# results from clustering -------------------------------------------------
clustered_data<- read.xlsx(xlsxFile = here("results","clustering results","survey_data_w_clusters.xlsx"))


## Cluster center visualisation##

### Kmeans ###

#sample distance to cluster & their ids

sample_distance = read.xlsx(xlsxFile = here("results","clustering results","kmeans_sample_to_center_dist.xlsx")) %>% 
  mutate(sample_id = sample_id+1)#because python starts indexing from 0 but R from 1....

# 
# closest_members_k0 = sample_distance %>%
#   filter(membership == 0) %>%
#   slice_min(order_by = d_cluster_0,n = 10) %>%
#   pull(sample_id)
# 
# closest_memrbers_k1 = sample_distance %>%
#   filter(membership == 1) %>%
#   slice_min(order_by = d_cluster_1, n = 10) %>%
#   pull(sample_id)
# 
# test = clustered_data[c(closest_members_k0,closest_memrbers_k1),] %>%
#   mutate(age_groups = case_when(age>=20 &age<=29~ "20-29 ar",
#                                 age>=30 & age<=59~ "30-59 ar",
#                                 age>=60~"over 60",.default="Other")) %>%
#   mutate(gender = factor(gender,levels = c(1,2),labels = c("Mann","Kvinne")))
# 
# 
# vis_data = test %>% 
#   group_by(kmeans_cluster) %>%
#   summarise(across(starts_with("Q20"),~round(mean(as.numeric(.x))),.names = "{.col}_avg")) %>% 
#   pivot_longer(cols = starts_with("Q20"),names_to = "vars",values_to = "val")
# 
# vis_data %>% 
#   ggplot(aes(x = vars, y = val))+
#   geom_point(aes(size = val,color = as.factor(kmeans_cluster),shape = as.factor(kmeans_cluster)))
# 
#based on the observations close to centroids, k means do not differentiate between groups very well

##testing with the whole data

kmeans_whole<- clustered_data %>% 
  group_by(kmeans_cluster) %>% 
  summarise(across(matches("Q20"),~round(mean(as.numeric(.x))))) %>% 
  pivot_longer(cols = matches("Q20"),names_to = "vars", values_to = "val") %>% 
  mutate(algo = "kmeans") %>% 
  rename(cluster_id = kmeans_cluster)

kmeans_whole %>% 
  ggplot(aes(x = vars, y = val))+
  geom_point(aes(size = val,color = as.factor(cluster_id),shape = as.factor(cluster_id)))

#the main difference is distance to:
    # schools
    # city life
    # cheap real estate
    # distance to work 
#is sligtly more important for cluster 1, otherwise they agree on the other characteristics


### Agglomerative ###

agglomerative = clustered_data %>% 
  group_by(agglomerative_cluster) %>% 
  summarise(across(matches("Q20"),~round(mean(as.numeric(.x))))) %>% 
  pivot_longer(matches("Q20"),names_to = "vars",values_to = "val") %>% 
  mutate(algo = "agglomerative") %>% 
  rename(cluster_id = agglomerative_cluster)


### Affinity propagation ###

affinity = clustered_data %>% 
  group_by(AffinityProp_cluster) %>% 
  summarise(across(matches("Q20"),~round(mean(as.numeric(.x))))) %>% 
  pivot_longer(matches("Q20"),names_to = "vars",values_to = "val") %>% 
  mutate(algo = "affinity") %>% 
  rename(cluster_id = AffinityProp_cluster)


### Spectral clustering ###



spectral = clustered_data %>% 
  group_by(SpecClust_cluster) %>% 
  summarise(across(matches("Q20"),~round(mean(as.numeric(.x))))) %>% 
  pivot_longer(matches("Q20"),names_to = "vars",values_to = "val") %>% 
  mutate(algo = "Spectral") %>% 
  rename(cluster_id = SpecClust_cluster) %>% 
  mutate(cluster_id = case_when(cluster_id == 0 ~1,.default = 0))



clustering_results<- rbind(kmeans_whole,agglomerative,affinity,spectral)


clustering_results %>% 
  filter(algo == "kmeans") %>% 
  filter(vars != "Q20r8") %>% 
  mutate(vars = case_when(vars == "Q20r1" ~ "Naer Famile og venner",
                          vars == "Q20r2" ~ "Naer butikker og tjenester",
                          vars == "Q20r3" ~ "Naer skole/barnehage",
                          vars == "Q20r4" ~ "Naer natur omrade",
                          vars == "Q20r5" ~ "Naer byliv",
                          vars == "Q20r6" ~ "Mindre avhengighet av bil",
                          vars == "Q20r7" ~ "Gunstig boligpris",
                          vars == "Q20r9" ~ "Tilpasser til funksjonsniva")) %>% 
  mutate(cluster_id = case_when(cluster_id == 0 ~"Klynge 1",
                                cluster_id == 1 ~"Klynge 2"
                                )) %>% 
  ggplot(aes(x = vars, y = val))+
  geom_point(aes(color = as.factor(cluster_id)),size = 4)+
  theme_bw()+
  guides(color = guide_legend(title = ""))+
  labs(x = "Omrade karakteristikk", y= "Viktighet")+
  coord_flip()
  
  
## demographic characteristics

cluster_demogs<-clustered_data %>% 
  mutate(Q4 = case_when(is.na(Q4)~0,.default = Q4)) %>% 
  select(kmeans_cluster,age_group,Q1,Q2,Q3) %>% 
  pivot_longer(age_group:Q3,names_to = "vars",values_to = "vals") %>% 
  group_by(kmeans_cluster,vars,vals) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(kmeans_cluster,vars) %>% 
  mutate(cluster_sum = sum(n)) %>% 
  ungroup() %>% 
  mutate(within_clust_perc = round(n/cluster_sum,2)*100) %>% 
  mutate(cluster_perc = round(cluster_sum/401,2)*100)
  


# results from logistic regression ----------------------------------------
model<- readRDS(here("results","regression results","mlogit2_demografi.RDS"))
## P-values ##
p_value<- function(coef,se){
  z_score = coef/se
  p_value <- (1-pnorm(abs(z),0,1))*2
  return(p_value)
}
summary(model)
p_value(coef = summary(model)$coefficients,se = summary(model)$standard.errors)

## Marginal probabilities ##

## Predicted probabilities ##



