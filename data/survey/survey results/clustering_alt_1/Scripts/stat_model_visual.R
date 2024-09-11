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


closest_members_k0 = sample_distance %>% 
  filter(membership == 0) %>% 
  slice_min(order_by = d_cluster_0,n = 10) %>% 
  pull(sample_id)

closest_memrbers_k1 = sample_distance %>% 
  filter(membership == 1) %>% 
  slice_min(order_by = d_cluster_1, n = 10) %>% 
  pull(sample_id)

test = clustered_data[c(closest_members_k0,closest_memrbers_k1),] %>% 
  mutate(age_groups = case_when(age>=20 &age<=29~ "20-29 ar",
                                age>=30 & age<=59~ "30-59 ar",
                                age>=60~"over 60",.default="Other")) %>% 
  mutate(gender = factor(gender,levels = c(1,2),labels = c("Mann","Kvinne")))


vis_data = test %>% 
  group_by(kmeans_cluster) %>%
  summarise(across(starts_with("Q20"),~round(mean(as.numeric(.x))),.names = "{.col}_avg")) %>% 
  pivot_longer(cols = starts_with("Q20"),names_to = "vars",values_to = "val")

vis_data %>% 
  ggplot(aes(x = vars, y = val))+
  geom_point(aes(size = val,color = as.factor(kmeans_cluster),shape = as.factor(kmeans_cluster)))

#based on the observations close to centroids, k means do not differentiate between groups very well

##testing with the whole data

kmeans_whole<- clustered_data %>% 
  group_by(kmeans_cluster) %>% 
  summarise(across(matches("Q20|Q10"),~round(mean(as.numeric(.x))))) %>% 
  pivot_longer(cols = matches("Q20|Q10"),names_to = "vars", values_to = "val") %>% 
  mutate(algo = "kmeans") %>% 
  rename(cluster_id = kmeans_cluster)

kmeans_whole %>% 
  ggplot(aes(x = vars, y = val))+
  geom_point(aes(size = val,color = as.factor(kmeans_cluster),shape = as.factor(kmeans_cluster)))

#the main difference is distance to:
    # schools
    # city life
    # cheap real estate
    # distance to work 
#is sligtly more important for cluster 1, otherwise they agree on the other characteristics


### Agglomerative ###

agglomerative = clustered_data %>% 
  group_by(agglomerative_cluster) %>% 
  summarise(across(matches("Q20|Q10"),~round(mean(as.numeric(.x))))) %>% 
  pivot_longer(matches("Q20|Q10"),names_to = "vars",values_to = "val") %>% 
  mutate(algo = "agglomerative") %>% 
  rename(cluster_id = agglomerative_cluster)


### Affinity propagation ###

affinity = clustered_data %>% 
  group_by(AffinityProp_cluster) %>% 
  summarise(across(matches("Q20|Q10"),~round(mean(as.numeric(.x))))) %>% 
  pivot_longer(matches("Q20|Q10"),names_to = "vars",values_to = "val") %>% 
  mutate(algo = "affinity") %>% 
  rename(cluster_id = AffinityProp_cluster)


### Spectral clustering ###



spectral = clustered_data %>% 
  group_by(SpecClust_cluster) %>% 
  summarise(across(matches("Q20|Q10"),~round(mean(as.numeric(.x))))) %>% 
  pivot_longer(matches("Q20|Q10"),names_to = "vars",values_to = "val") %>% 
  mutate(algo = "Spectral") %>% 
  rename(cluster_id = SpecClust_cluster)



clustering_results<- rbind(kmeans_whole,agglomerative,affinity,spectral)

clustering_results %>% 
  ggplot(aes(x=vars, y = val))+
  geom_point(aes(color = as.factor(cluster_id)),size = 4)+
  theme_bw()+
  facet_wrap(~algo)

#I think spectral clustering and k means are the most meaningfull ones

clustering_results %>% 
  filter(algo == "Spectral") %>% 
  filter(grepl(vars,pattern = "Q20")) %>% 
  ggplot(aes(x = vars, y = val))+
  geom_point(aes(color = as.factor(cluster_id)),size = 4)+
  theme_bw()


clustering_results %>% 
  filter(algo == "kmeans") %>% 
  filter(grepl(vars,pattern = "Q20")) %>% 
  filter(vars%in%c("Q20r3","Q20r5","Q20r7","Q20r8")) %>% 
  mutate(vars = case_when(vars == "Q20r3" ~"Avstand til skole/barnehage",
                          vars == "Q20r5" ~"Avstand til byliv",
                          vars == "Q20r7" ~"Gunstig boligpris",
                          vars == "Q20r8" ~"Avstand til arbeidsplass")) %>% 
  mutate(cluster_id = case_when(cluster_id == 0 ~"Gruppe 1",
                                cluster_id == 1 ~"Gruppe 2"
                                )) %>% 
  ggplot(aes(x = vars, y = val))+
  geom_point(aes(color = as.factor(cluster_id)),size = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  guides(color = guide_legend(title = ""))+
  labs(x = "Omrade karakteristikk", y= "Viktighet")

  
kmeans_modes <- clustered_data %>% 
  group_by(kmeans_cluster) %>% 
  summarise(age = get_mode(age_group),
            gender = get_mode(gender),
            work = get_mode(Q1),
            family = get_mode(Q3))


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



