#Multinominal logistic regression for boområde

# setup -------------------------------------------------------------------

##packages
library(pacman)

p_load(char = c("here","tidyverse","nnet","openxlsx","Hmisc","scales"))

##data
integer_data<- read.xlsx(xlsxFile = here("data","survey","survey results","KNA135092_240702_raw.xlsx"),sheet = 1)[-1,]

## Data examination----
demografi_variabler<- c("age","gender","zipcode","Q1","Q3","Q4")#Nas in Q4 can be replaced with 0 to indicate there is no kid living with the respondent
#Need to add age,gender and zip code
bolig_variabler<- c("Q5","Q9","Q14")
preferanse_variabler<-c(paste0("Q10r",seq(1,8,by = 1)),paste0("Q20r",seq(1,9,by = 1)),"weight")

regression_variables<- c(demografi_variabler,bolig_variabler,preferanse_variabler)

regression_data<- integer_data %>% 
  select(all_of(regression_variables)) %>% 
  mutate(across(where(is.character),~as.numeric(.x)))

meta_data<- Hmisc::contents(regression_data)["contents"] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Var_id")

stat_summary<- regression_data %>% 
  drop_na() %>% 
  summarise(across(everything(),list(min = min,max = max, mean = mean,sd = sd))) %>% 
  pivot_longer(everything(),names_to = "var_fun",values_to = "values") %>% 
  mutate(Var_id = str_split_i(var_fun, pattern = "_", i = 1)) %>% 
  mutate(metric =str_split_i(var_fun, pattern = "_", i = 2)) %>% 
  select(-var_fun) %>% 
  left_join(.,meta_data,by = "Var_id")

#Drop Q4, focus on Q3 instead.
#Merger answers to Q14 with Q9,
## if Q14 is Na, replace Q14 with Q9
## based on the assumption that if they are not planning to move, they are happy with the location thus its their "ideal"


# data manipulation -------------------------------------------------------


analysis_data<- regression_data %>% 
  mutate(y_outcome = Q14) %>% #make outcome variable
  mutate(y_outcome = case_when(is.na(y_outcome)~Q9,.default = y_outcome)) %>% # Flatten Q9 and Q14
  mutate(y_outcome = factor(y_outcome, levels = c(1,2,3,4),ordered = F))# convert the outcome variable to factor for logistic model

table(analysis_data$y_outcome) %>% 
  proportions() %>% 
  round(.,2)#check to see which one of the levels make sense to use as reference level
##sentrum nært i by: 28%
##sentrum nært i tettsted: 33%
##i bygd: 21%
##i spredtbygd område: 19%
###so any one of these can statistically be a reference category.Theoretically it might make more sense to set the reference category to least central
###location (i spredtbygd område)

analysis_data<- analysis_data %>% 
  mutate(y_outcome = relevel(y_outcome, ref = 4))# change the reference level to "sentrum nært i by", reference level is the comparison group

#if nnet:multinom will be used predictors should be rescaled to [0,1] for convergence (also fits it through a neural network for some dumb reason)
#MASS polr fits proportional odds model (ordinal logistic regression), so not entirely appropriate for the task
#Mlogit package needs the data in its "own format" shaped through mlogit.data(). Not sure why,and I prefer the data to be in tidy format
#VGAM::vglm could be a good alternative since it provides the model through vector generalized additive models but couldnt install VGAm

# since we gonna use nnet, "preferences" (i.e Q20s) will be rescaled to 0-1


#OBS need to assign reference categories to predictor variables in factor form for better inference
analysis_data_nnet<- analysis_data %>% 
  mutate(across(contains("Q20"),~as.numeric(rescale(.x,to = c(0,1),from = range(.x,na.rm = T,finite = T))))) %>% 
  mutate(across(c(Q1,Q3),~as.factor(.x))) %>% 
  mutate(Q1 = relevel(Q1, ref = 5)) %>% 
  mutate(Q3 = relevel(Q3, ref = 6)) %>% 
  mutate(Q4 = case_when(is.na(Q4)~0,.default = Q4)) %>% 
  mutate(age_groups = case_when(age>=20 & age<30 ~ "20-29",
                                age>=30 & age<60 ~ "30-59",
                                age>=60 ~ "over 60",.default = "other")) %>% 
  mutate(gender = case_when(gender == 1 ~ 0,
                            gender == 2 ~ 1)) %>% 
  mutate(gender = factor(gender,levels = c(0,1),labels = c("Mann","Kvinne"), ordered = F)) %>% 
  mutate(gender = relevel(gender, ref = "Mann")) %>% 
  mutate(zipcode = factor(zipcode,ordered = F))

# modelling ---------------------------------------------------------------
##Given a set of candidate models for the data, the preferred model is the one with the minimum AIC value##
##Empty model:

model1_fit <- nnet::multinom(formula = y_outcome~1,data = analysis_data_nnet,weights = weight)

saveRDS(model1_fit,file = here("results","regression results","mlogit1_empty.RDS"))

model2_demografi<- nnet::multinom(formula = y_outcome~Q1+Q3+Q4+gender+age_groups,data = analysis_data_nnet, weights = weight)

saveRDS(model2_demografi,file = here("results","regression results","mlogit2_demografi.RDS"))

summary(model2_demografi)

#this model has more predictors than others so I couldnt be arsed with typing each one

model_formula = as.formula(paste("y_outcome",paste(grep(preferanse_variabler,pattern = "Q20",value = T),collapse = "+"),sep = "~"))

model3_preferences<- nnet::multinom(formula = model_formula,data = analysis_data_nnet,weights = weight)

summary(model3_preferences)

saveRDS(model3_preferences,file = here("results","regression results","mlogit3_preferences.RDS"))

model_formula = as.formula(paste0("y_outcome","~",#predicted variable
                                  paste(grep(preferanse_variabler, pattern = "Q20",value = T),collapse = "+"),#first set of predictors
                                  "+",paste(demografi_variabler[-3],collapse = "+")))#second set of predictors


model4_comprehensive = nnet::multinom(formula = model_formula,data = analysis_data_nnet,weights = weight)


saveRDS(model4_comprehensive,file = here("results","regression results","mlogit4_comprehensive.RDS"))


# model_summary ---------------------------------------------------------

model4_coefficients=as.data.frame(summary(model4_comprehensive)$coefficients) %>% 
  rownames_to_column(var = "y_category") %>% 
  pivot_longer(`(Intercept)`:Q4,names_to = "predictor",values_to = "coefficient")

model4_standard_errors = as.data.frame(summary(model4_comprehensive)$standard.errors) %>% 
  rownames_to_column((var = "y_category")) %>% 
  pivot_longer(`(Intercept)`:Q4,names_to = "predictor",values_to = "se")

model4_results_df = left_join(model4_coefficients,model4_standard_errors,by = c("y_category","predictor"))


quick_pvalue = function(coef,se){
  z_value = coef/se
  p_value = (1-pnorm(abs(z_value),0,1))*2
  return(p_value)
}

model4_results_df$p_value = quick_pvalue(coef = model4_results_df$coefficient,se = model4_results_df$se)

model4_graph =model4_results_df %>% 
  mutate(p_star = case_when(p_value <=0.001 ~ "***",
                            p_value <=0.05 ~ "**",
                            p_value <=0.1 ~ "*",.default = "")) %>% 
  mutate(odds = round(exp(coefficient)-1,3)) %>% 
  mutate(y_category = case_when(y_category == 1 ~"sentrumnaert i by",
                                y_category == 2 ~"sentrumnaert i tettsted",
                                y_category == 3 ~"i bygd"))

model4_graph %>% 
  filter(grepl(pattern = "Q20",x = predictor)) %>% 
  filter(odds<10) %>% 
  mutate(predictor = case_when(predictor == "Q20r1" ~"Naer familie",
                               predictor == "Q20r2" ~"Naer butikker / tjenester",
                               predictor == "Q20r3" ~"Naer skole / barnehage",
                               predictor == "Q20r4" ~"Naer natur",
                               predictor == "Q20r5" ~"Naer byliv",
                               predictor == "Q20r6" ~"Miljovennlig transport mulighet",
                               predictor == "Q20r7" ~"Gunstig eindompris",
                               predictor == "Q20r8" ~"Naer arbeidsted",
                               predictor == "Q20r9"~"Tilpasset til funksjonsniva")) %>% 
  ggplot(aes(x = predictor,y = odds))+
  geom_point(size = 4,color = "steelblue",show.legend = F)+
  geom_text(aes(label = p_star),nudge_y = -0.6)+
  geom_hline(yintercept = 0, color = "red")+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  facet_wrap(~y_category)+
  labs(x = "Prediktorer", y = "Oddsforhold",subtitle = "Referanse kategori: i spredtbygd omrade",caption = "***: p-verdi<0.001\n**: p-verdi<0.05\n*: p-verdi<0.1")


# simulation --------------------------------------------------------------

###its gonna be a litt too complicated to simulate data about this. Maybe on the 4th version I can include it in the report or in the technical appendix.

## empirically common scenarios

label_data<- read.xlsx(xlsxFile = here("data","survey","survey results","KNA135092_240702_raw.xlsx"),sheet = 2)[-1,] %>% 
  mutate(Q4 = case_when(is.na(Q4)~"0",.default = Q4))

vannlig_scenarier <- xtabs(~age_group+gender+Q1+Q3+Q4,data = label_data) %>% 
  proportions(margin = 1)

write.xlsx(vannlig_scenarier,file = here("data","simulering","empiriske_simulering_scenarier1.xlsx"))
###Drafted up simulation scenarios in excel
###and saved it as empiriske_simulering_scenarier2.xlsx



simulation_scenarios<- read.xlsx(xlsxFile = here("data","simulering","empiriske_simulering_scenarier2.xlsx"),sheet = 3) %>% 
  mutate(Q1 = case_when(Q1 == "Jobber" ~"1",
                        Q1 == "Pensjonist"~"3",
                        .default = Q1)) %>% 
  mutate(gender = case_when(gender == "Mann" ~ "0",
                            gender == "Kvinne" ~"1",.default = gender)) %>% 
  mutate(Q3 = case_when(Q3=="Samboer/ektefelle uten barn"~"4",
                        Q3 == "Bor alene uten barn" ~ "1",
                        Q3 == "Samboer/ektefelle med barn"~"3",.default = Q3)) %>% 
  mutate(across(age:Q20r9,~as.numeric(.x))) %>% 
  mutate(across(c(Q1,Q3),~as.factor(.x))) %>% 
  mutate(gender = factor(gender,levels = c(0,1),labels = c("Mann","Kvinne"), ordered = F)) %>% 
  mutate(across(contains("Q20"),~as.numeric(rescale(.x,to = c(0,1),from = range(.x,na.rm = T,finite = T)))))
  
sim_data<- simulation_scenarios %>% select(-specs)

pred_class<-predict(model4_comprehensive,newdata = sim_data,type = "class")
pred_prob<- predict(model4_comprehensive,newdata = sim_data,type = "prob")

pred_probs_df<- as.data.frame(pred_prob) %>% 
  mutate(pred_class = pred_class)

saveRDS(pred_probs_df,file=here("data","simulering","predicted_omrade.RDS"))

simulation_results<- cbind(simulation_scenarios,pred_probs_df)


write.xlsx(simulation_results,file = here("results","simulation results","simulation_results.xlsx"))
#added labels for område probability variables and the predicted class


# simulation results visualisation ----------------------------------------

sim_results<- read.xlsx(here("results","simulation results","simulation_results.xlsx"),sheet = 1) %>% 
  select(scenario,spredtbygd:pred_class)

sim_graph_data<- cbind(read.xlsx(xlsxFile = here("data","simulering","empiriske_simulering_scenarier2.xlsx"),sheet = 3), sim_results)

## scenario 1

### kari og ole nordman

s1_responses<- sim_graph_data %>% 
  filter(scenario == 1) %>% 
  select(specs,contains("Q20")) %>% 
  mutate(specs = case_when(grepl(pattern = "kvinne",specs,ignore.case = T)~"Kari Nordmann",
                           grepl(pattern = "Mann",specs,ignore.case = T)~"Ole Nordmann",.default = specs)) %>% 
  pivot_longer(Q20r1:Q20r9,names_to = "preferences", values_to = "value") %>% 
  mutate(preferences = case_when(preferences == "Q20r1" ~ "nær familie/venner",
                                 preferences == "Q20r2" ~ "nær butikker/tjenester",
                                 preferences == "Q20r3" ~"nær skole/barnehage",
                                 preferences == "Q20r4"~"nær natur",
                                 preferences == "Q20r5" ~ "nær byliv",
                                 preferences == "Q20r6" ~ "Alternativ reisemidler",
                                 preferences == "Q20r7" ~ "Lave boligpris",
                                 preferences == "Q20r8" ~ "Nær arbeidssted",
                                 preferences == "Q20r9" ~ "Tilpasser til funksjonsnivået")) %>% 
  mutate(y_labs = case_when(value == 1 ~"1 - Ikke viktig",
                            value == 3 ~ "3-Nøytral",
                            value == 5 ~ "5-veldig viktig",.default = as.character(value)))

s1_responses %>% 
  ggplot(aes(x = preferences, y = value))+
  geom_col(fill = "steelblue")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "preferansene", y = "verdi", subtitle = "Ole og Kari Nordmanns preferanser")+
  coord_flip()+
  facet_wrap(~specs)

s1_results<- sim_graph_data %>% 
  filter(scenario == 1) %>% 
  select(specs,spredtbygd:bygd) %>% 
  mutate(specs = case_when(grepl(pattern = "kvinne",specs,ignore.case = T)~"Kari Nordmann",
                           grepl(pattern = "Mann",specs,ignore.case = T)~"Ole Nordmann",.default = specs)) %>% 
  pivot_longer(spredtbygd:bygd,names_to = "sted", values_to = "prob") %>% 
  mutate(prob = round(prob,2)*100)

s1_results %>% 
  ggplot(aes(x = sted, y = prob))+
  geom_col(fill = "steelblue") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Sted", y = "Sannsynlighet")+
  facet_wrap(~specs)


## Scenario 2

## oskar Hansen

s2_responses <- sim_graph_data %>% 
  filter(scenario == 2) %>% 
  select(specs,contains("Q20")) %>% 
  mutate(specs = case_when(grepl(pattern = "kvinne",specs,ignore.case = T)~"Kari Nordmann",
                           grepl(pattern = "Mann",specs,ignore.case = T)~"Oskar Hansen",.default = specs)) %>% 
  pivot_longer(Q20r1:Q20r9,names_to = "preferences", values_to = "value") %>% 
  mutate(preferences = case_when(preferences == "Q20r1" ~ "nær familie/venner",
                                 preferences == "Q20r2" ~ "nær butikker/tjenester",
                                 preferences == "Q20r3" ~"nær skole/barnehage",
                                 preferences == "Q20r4"~"nær natur",
                                 preferences == "Q20r5" ~ "nær byliv",
                                 preferences == "Q20r6" ~ "Alternativ reisemidler",
                                 preferences == "Q20r7" ~ "Lave boligpris",
                                 preferences == "Q20r8" ~ "Nær arbeidssted",
                                 preferences == "Q20r9" ~ "Tilpasser til funksjonsnivået")) %>% 
  mutate(y_labs = case_when(value == 1 ~"1 - Ikke viktig",
                            value == 3 ~ "3-Nøytral",
                            value == 5 ~ "5-veldig viktig",.default = as.character(value)))

s2_responses %>% 
  ggplot(aes(x = preferences, y = value))+
  geom_col(fill = "steelblue")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "preferansene", y = "verdi",subtitle = "Oskar Hansens preferanser")+
  coord_flip()

s2_result<- sim_graph_data %>% 
  filter(scenario == 2) %>% 
  select(specs,spredtbygd:bygd) %>% 
  mutate(specs = case_when(grepl(pattern = "kvinne",specs,ignore.case = T)~"Kari Nordmann",
                           grepl(pattern = "Mann",specs,ignore.case = T)~"Oskar Hansen",.default = specs)) %>% 
  pivot_longer(spredtbygd:bygd,names_to = "sted", values_to = "prob") %>% 
  mutate(prob = round(prob,2)*100)


s2_result %>% 
  ggplot(aes(x = sted, y = prob))+
  geom_col(fill = "steelblue") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Sted", y = "Sannsynlighet")


##Scenario 3

### Maja Johansen

s3_responses <- sim_graph_data %>% 
  filter(scenario == 3) %>% 
  select(specs,contains("Q20")) %>% 
  mutate(specs = case_when(grepl(pattern = "kvinne",specs,ignore.case = T)~"Kari Nordmann",
                           grepl(pattern = "Mann",specs,ignore.case = T)~"Oskar Hansen",.default = specs)) %>% 
  pivot_longer(Q20r1:Q20r9,names_to = "preferences", values_to = "value") %>% 
  mutate(preferences = case_when(preferences == "Q20r1" ~ "nær familie/venner",
                                 preferences == "Q20r2" ~ "nær butikker/tjenester",
                                 preferences == "Q20r3" ~"nær skole/barnehage",
                                 preferences == "Q20r4"~"nær natur",
                                 preferences == "Q20r5" ~ "nær byliv",
                                 preferences == "Q20r6" ~ "Alternativ reisemidler",
                                 preferences == "Q20r7" ~ "Lave boligpris",
                                 preferences == "Q20r8" ~ "Nær arbeidssted",
                                 preferences == "Q20r9" ~ "Tilpasser til funksjonsnivået")) %>% 
  mutate(y_labs = case_when(value == 1 ~"1 - Ikke viktig",
                            value == 3 ~ "3-Nøytral",
                            value == 5 ~ "5-veldig viktig",.default = as.character(value)))

s3_responses %>% 
  ggplot(aes(x = preferences, y = value))+
  geom_col(fill = "steelblue")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "preferansene", y = "verdi",subtitle = "Maja Johansens preferanser")+
  coord_flip()

s3_result<- sim_graph_data %>% 
  filter(scenario == 3) %>% 
  select(specs,spredtbygd:bygd) %>% 
  pivot_longer(spredtbygd:bygd,names_to = "sted", values_to = "prob") %>% 
  mutate(prob = round(prob,2)*100)


s3_result %>% 
  ggplot(aes(x = sted, y = prob))+
  geom_col(fill = "steelblue") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Sted", y = "Sannsynlighet")


## Scenario 4
### Anne og Kjell Olsen

s4_responses <- sim_graph_data %>% 
  filter(scenario == 4) %>% 
  select(specs,contains("Q20")) %>% 
  mutate(specs = case_when(grepl(pattern = "kvinne",specs,ignore.case = T)~"Anne Olsen",
                           grepl(pattern = "Mann",specs,ignore.case = T)~"Kjell Olsen",.default = specs)) %>% 
  pivot_longer(Q20r1:Q20r9,names_to = "preferences", values_to = "value") %>% 
  mutate(preferences = case_when(preferences == "Q20r1" ~ "nær familie/venner",
                                 preferences == "Q20r2" ~ "nær butikker/tjenester",
                                 preferences == "Q20r3" ~"nær skole/barnehage",
                                 preferences == "Q20r4"~"nær natur",
                                 preferences == "Q20r5" ~ "nær byliv",
                                 preferences == "Q20r6" ~ "Alternativ reisemidler",
                                 preferences == "Q20r7" ~ "Lave boligpris",
                                 preferences == "Q20r8" ~ "Nær arbeidssted",
                                 preferences == "Q20r9" ~ "Tilpasser til funksjonsnivået")) %>% 
  mutate(y_labs = case_when(value == 1 ~"1 - Ikke viktig",
                            value == 3 ~ "3-Nøytral",
                            value == 5 ~ "5-veldig viktig",.default = as.character(value)))


s4_responses %>% 
  ggplot(aes(x = preferences, y = value))+
  geom_col(fill = "steelblue")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "preferansene", y = "verdi", subtitle = "Anne og Kjell Olsens preferanser")+
  coord_flip()+
  facet_wrap(~specs)

s4_results<- sim_graph_data %>% 
  filter(scenario == 4) %>% 
  mutate(specs = case_when(grepl(pattern = "kvinne",specs,ignore.case = T)~"Anne Olsen",
                           grepl(pattern = "Mann",specs,ignore.case = T)~"Kjell Olsen",.default = specs)) %>% 
  select(specs,spredtbygd:bygd) %>% 
  pivot_longer(spredtbygd:bygd,names_to = "sted", values_to = "prob") %>% 
  mutate(prob = round(prob,2)*100)


s4_results %>% 
  ggplot(aes(x = sted, y = prob))+
  geom_col(fill = "steelblue") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Sted", y = "Sannsynlighet")+
  facet_wrap(~specs)


##sim scenario 5
### ANDREAS Pedersen og Ida Eriksen

s5_responses <- sim_graph_data %>% 
  filter(scenario == 5) %>% 
  select(specs,contains("Q20")) %>% 
  mutate(specs = case_when(grepl(pattern = "kvinne",specs,ignore.case = T)~"Ida Eriksen",
                           grepl(pattern = "Mann",specs,ignore.case = T)~"Andreas Pedersen",.default = specs)) %>% 
  pivot_longer(Q20r1:Q20r9,names_to = "preferences", values_to = "value") %>% 
  mutate(preferences = case_when(preferences == "Q20r1" ~ "nær familie/venner",
                                 preferences == "Q20r2" ~ "nær butikker/tjenester",
                                 preferences == "Q20r3" ~"nær skole/barnehage",
                                 preferences == "Q20r4"~"nær natur",
                                 preferences == "Q20r5" ~ "nær byliv",
                                 preferences == "Q20r6" ~ "Alternativ reisemidler",
                                 preferences == "Q20r7" ~ "Lave boligpris",
                                 preferences == "Q20r8" ~ "Nær arbeidssted",
                                 preferences == "Q20r9" ~ "Tilpasser til funksjonsnivået")) %>% 
  mutate(y_labs = case_when(value == 1 ~"1 - Ikke viktig",
                            value == 3 ~ "3-Nøytral",
                            value == 5 ~ "5-veldig viktig",.default = as.character(value)))

s5_responses %>% 
  ggplot(aes(x = preferences, y = value))+
  geom_col(fill = "steelblue")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "preferansene", y = "verdi", subtitle = "Preferansene til Ida Eriksen og Andreas Pedersen")+
  coord_flip()+
  facet_wrap(~specs)

s5_results<- sim_graph_data %>% 
  filter(scenario == 5) %>% 
  mutate(specs = case_when(grepl(pattern = "kvinne",specs,ignore.case = T)~"Ida Eriksen",
                           grepl(pattern = "Mann",specs,ignore.case = T)~"Andreas Pedersen",.default = specs)) %>% 
  select(specs,spredtbygd:bygd) %>% 
  pivot_longer(spredtbygd:bygd,names_to = "sted", values_to = "prob") %>% 
  mutate(prob = round(prob,2)*100)


s5_results %>% 
  ggplot(aes(x = sted, y = prob))+
  geom_col(fill = "steelblue") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Sted", y = "Sannsynlighet")+
  facet_wrap(~specs)
