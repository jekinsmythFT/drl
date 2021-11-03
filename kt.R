install.packages("car")
library(car)




kt_reg <- kt %>% 
  select(nps,
         digital_exp,
         willingess_to_pay, 
         keep_indie, 
         aligned_values, 
         quality_news, 
         daily_routine, 
         replaceability, 
         mission, 
         helps_job, 
         quality_news, 
         informed,
         news_sources,
         important_to_sup,
         nps_recode) %>% 
  mutate(across(2:12, ~as.numeric(.x))) 

kt_reg %>% 
  drop_na() %>% 
  cor()

kt_model <- lm(formula = willingess_to_pay ~ digital_exp
                   + nps
                   + keep_indie
                   + aligned_values
                   + quality_news
                   + daily_routine
                   + replaceability
                   + mission
                   + helps_job
                   + quality_news
                   + informed
               + important_to_sup
                   + news_sources,
                   data = kt_reg)

summary(kt_model)
vif(kt_model)

kt %>% 
  count(replaceability)


kt %>% 
  count(news_sources) %>% 
  mutate(prop = n / sum(n))

kt_reg %>% 
  group_by(willingess_to_pay) %>% 
  summarise(n = n(),
            aligned_values = mean(aligned_values, na.rm = T),
            keep_indie = mean(keep_indie, na.rm = T),
            important_to_sup = mean(important_to_sup, na.rm = T),
            news_sources = mean(news_sources, na.rm = T),
            replaceability  = mean(replaceability , na.rm = T))


kt %>% 
  select(age, gender, willingess_to_pay) %>% 
  mutate(willingess_to_pay = as.numeric(willingess_to_pay)) %>% 
  group_by(age) %>% 
  summarise(willingess_to_pay = mean(willingess_to_pay, na.rm = T),
            n = n()) %>% 
  drop_na() %>% 
  {  barplot(height = .$willingess_to_pay, names = .$age, width = .$n, col = "#F4B400", ylim = c(0,5))}


kt %>% 
  select(age, gender, news_sources) %>% 
  mutate(news_sources = as.numeric(news_sources)) %>% 
  group_by(age) %>% 
  summarise(news_sources = mean(news_sources, na.rm = T),
            n = n()) %>% 
  drop_na() %>% 
  {  barplot(height = .$news_sources, names = .$age, width = .$n, col = "#F4B400", ylim = c(0,3))}

kt %>% 
  group_by(sub_a) %>% 
  summarise(n = n(),
            willingess_to_pay = mean(as.numeric(willingess_to_pay), na.rm = T))
