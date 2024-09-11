integer_data<- read.xlsx(xlsxFile = here("data","survey","survey results","KNA135092_240702_raw.xlsx"),sheet = 2)[-1,]


table(integer_data$Q14,useNA = "no") %>% 
  prop.table() %>% 
  as.data.frame() %>% 
  mutate(Freq = round(Freq*100)) %>% 
  ggplot(aes(x = Var1,y = Freq))+
  geom_col(aes(fill = Freq),show.legend = F)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(x = "Område", y= "Prosent",subtitle = "Ønskende bo område")+
  scale_y_continuous(breaks = seq(0,100,by = 10))+
  scale_fill_gradient(low = "#fce6a9", high = "#7a0e04")


table(integer_data$Q9,useNA = "no") %>% 
  prop.table() %>% 
  as.data.frame() %>% 
  mutate(Freq = round(Freq*100)) %>% 
  ggplot(aes(x = Var1,y = Freq))+
  geom_col(aes(fill = Freq),show.legend = F)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(x = "Område", y= "Prosent",subtitle = "Dagens bo område")+
  scale_y_continuous(breaks = seq(0,100,by = 10))+
  scale_fill_gradient(low = "#fce6a9", high = "#7a0e04")
