

ara_reg <- ara %>% 
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

ara_reg %>% 
  drop_na() %>% 
  cor()

ara_reg %>% 
  group_by(digital_exp) %>% 
  summarise(n = n(),
            nps = mean(nps_recode, na.rm = T))

ara_model <- lm(formula = willingess_to_pay ~ digital_exp
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
                    data = ara_reg)

summary(ara_model)
vif(ara_model)


ara_model_nps <- lm(formula = nps ~ digital_exp
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
                        data = ara_reg)

summary(ara_model_nps)
vif(ara_model_nps)

ara %>% 
  group_by(important_to_sup) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n))

ara %>% 
  select(one_improv) %>% 
  drop_na()

ara %>% 
  count(news_sources) %>% 
  mutate(prop = n / sum(n))

ara_reg %>% 
  group_by(willingess_to_pay) %>% 
  summarise(n = n(),
            mission = mean(aligned_values, na.rm = T),
            quality_news = mean(keep_indie, na.rm = T),
            important_to_sup = mean(important_to_sup, na.rm = T),
            news_sources = mean(news_sources, na.rm = T),
            replaceability  = mean(replaceability , na.rm = T))


ara %>% 
  select(age, gender, willingess_to_pay) %>% 
  mutate(willingess_to_pay = as.numeric(willingess_to_pay)) %>% 
  group_by(age) %>% 
  summarise(willingess_to_pay = mean(willingess_to_pay, na.rm = T),
            n = n()) %>% 
  drop_na() %>% 
  {  barplot(height = .$willingess_to_pay, names = .$age, width = .$n, col = "#F4B400", ylim = c(0,5))}


ara %>% 
  select(age, gender, news_sources) %>% 
  mutate(news_sources = as.numeric(news_sources)) %>% 
  group_by(age) %>% 
  summarise(news_sources = mean(news_sources, na.rm = T),
            n = n()) %>% 
  drop_na() %>% 
  {  barplot(height = .$news_sources, names = .$age, width = .$n, col = "#F4B400", ylim = c(0,3))}

ara %>% 
  group_by(sub_c) %>% 
  summarise(n = n(),
            willingess_to_pay = mean(as.numeric(willingess_to_pay), na.rm = T),
            nps = mean(nps_recode, na.rm = T))


ara_reg %>% 
  select(nps, nps_recode, informed, digital_exp, quality_news) %>% 
  pivot_longer(3:5) %>% 
  group_by(name, value) %>% 
  mutate(nps_o = mean(nps_recode, na.rm = T)) %>% 
  summarise(nps = mean(nps, na.rm = T),
            nps_o = mean(nps_o)) %>% 
  drop_na() %>% 
  ggplot(aes(value, nps, col = name, label = round(nps_o, 2))) +
  geom_smooth(se = F) +
  geom_label() 



ara_reg %>% 
  select(nps, nps_recode, informed, digital_exp, quality_news) %>% 
  pivot_longer(3:5) %>% 
  group_by(name, value) %>% 
  mutate(nps_o = mean(nps_recode, na.rm = T)) %>% 
  ungroup() %>% 
  {    ggplot(., aes(value, nps, col = name, label = round(nps_o, 2))) +
      geom_jitter(alpha = 0.4) +
      geom_smooth(se = F, method = lm) +
      geom_label(data = subset(., name == "digital_exp"), aes(y = 3)) +
      geom_label(data = subset(., name == "informed"), aes(y = 2)) +
      geom_label(data = subset(., name == "quality_news"), aes(y = 1)) 
  }



