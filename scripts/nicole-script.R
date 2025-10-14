library(tidyverse)
library(janitor)
library(gt)
library(nnet)

#imports
background <- read_csv('data/background-clean.csv')
interest <- read_csv('data/interest-clean.csv')
metadata <- read_csv('data/survey-metadata.csv')

#data clean
all_data <- background %>% inner_join(interest, by = join_by(response_id)) %>% 
  rename(c(domain_yn = dom.x, domain_type = dom.y, research_or_industry = do_you_have_any_preference_regarding_working_on_an_industry_project_or_a_research_lab_project))
all_data$lang[all_data$response_id == 29] <- "Python"

r_pref <- all_data %>% filter(lang=="R")
py_pref <- all_data %>% filter(lang =="Python")
no_pref <- all_data %>% filter(lang =="No preference")

#eda
all_data %>% tabyl(lang, type) %>% adorn_percentages() %>% adorn_pct_formatting() %>% gt()

all_data <- all_data %>% mutate(area_image = ifelse(str_detect(area, "Analysis or classification of images"), 1, 0),
                                area_dlnn = ifelse(str_detect(area, "Deep learning and neural networks"), 1, 0),
                                area_spatial = ifelse(str_detect(area, "Spatial statistics or time series analysis"), 1, 0),
                                area_dataeng = ifelse(str_detect(area, "Data acquisition and engineering"), 1, 0),
                                area_nlp = ifelse(str_detect(area, "Natural language processing and analysis of text"), 1, 0),
                                area_software = ifelse(str_detect(area, "Model deployment and software or web integrations"), 1, 0),
                                area_vis = ifelse(str_detect(area, "Data visualization and interactive dashboards"), 1, 0),
                                area_pred = ifelse(str_detect(area, "Predictive modeling, generally"), 1, 0),
                                area_stat = ifelse(str_detect(area, "Statistical models and inference, generally"), 1, 0),
                                area_database = ifelse(str_detect(area, "Databases"), 1, 0),
                                area_algo = ifelse(str_detect(area, "Algorithms"), 1, 0))

area_vars <- grep("^area_", names(all_data), value = TRUE)

formula_str <- paste("lang ~", paste(area_vars, collapse = " + "))
formula <- as.formula(formula_str)

model_mn <- multinom(formula, data = all_data)

(summary(model_mn))

#####

binary_data <- all_data %>% filter(lang!= "No preference") %>% mutate(lang = ifelse(lang == "Python", 1, 0))

model_glm <- glm(formula, data = binary_data)

(summary(model_glm))
