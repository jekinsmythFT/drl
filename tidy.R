library(readr)
ds <- read_csv("ds.csv")

library(tidyverse)

ds <- ds %>% 
  mutate(willingess_to_pay = factor(willingess_to_pay, levels = c("Extremely unwilling",
                                                                  "Somewhat unwilling",
                                                                  "Neither willing nor unwilling",
                                                                  "Somewhat willing",
                                                                  "Extremely willing")),
         digital_exp = factor(digital_exp, levels = c("Very difficult to use",
                                                      "Somewhat difficult to use",
                                                      "Neither easy nor hard",
                                                      "Somewhat easy to use",
                                                      "Very easy to use"))) %>% 
  mutate(across(c(keep_indie, aligned_values, daily_routine, replaceability, mission, helps_job, quality_news, informed), ~factor(.x, 
                                                                                                                                  levels = c("Strongly Disagree",
                                                                                                                                  "Somewhat Disagree",
                                                                                                                                  "Neither agree nor Disagree",
                                                                                                                                  "Somewhat agree",
                                                                                                                                  "Strongly agree")))) %>% 
  mutate(important_to_sup = as.numeric(factor(important_to_sup, levels =  c("Strongly disagree",
                                                                 "Somewhat disagree",
                                                                 "Neither agree nor disagree",
                                                                 "Somewhat agree",
                                                                 "Strongly agree")))) %>% 
  mutate(nps_recode = if_else(nps <= 6, -100,
                              if_else(nps >= 9, 100, 0))) 

ds <- ds %>% 
  mutate(replaceability = factor(replaceability, levels = rev(c("Strongly Disagree",
                                                                "Somewhat Disagree",
                                                                "Neither agree nor Disagree",
                                                                "Somewhat agree",
                                                                "Strongly agree"))))

kt <- ds %>% 
  filter(publisher == "Khaleej Times") %>% 
  select(-c(value_most_b, value_most_c, value_most_d, sub_b, sub_c, sub_d))


correio<- ds %>% 
  filter(publisher == "Correio da Manhã") %>% 
  select(-c(value_most_a, value_most_a_other, countries, value_most_c, 
         value_most_d, sub_a, sub_c, sub_d))


ara <- ds %>% 
  filter(publisher == "diari ARA") %>% 
  select(-c(value_most_b, value_most_a_other, countries, value_most_a, 
         value_most_d, sub_b, sub_a, sub_d))


sud  <- ds %>% 
  filter(publisher == "Gazzetta del Sud") %>% 
  select(-c(value_most_b, value_most_a_other, countries, value_most_c, 
         value_most_a, sub_b, sub_c, sub_a))
