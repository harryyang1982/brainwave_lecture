library(readxl)
library(tidyverse)

resum <- read_csv("~/Documents/R/resum.csv")

resum_tidy <- resum %>% 
  gather(category, value, `higher_education_2008`:`total_pop_2017`)

resum_tidy_na <- resum_tidy %>% 
  mutate(value = ifelse(value != ":", value, NA),
         `CITIES/TIME` = ifelse(`CITIES/TIME` != ":", `CITIES/TIME`, NA)) 

resumm <- resum_tidy_na %>% 
  filter(!is.na(value) & !is.na(`CITIES/TIME`)) %>% 
  group_by(`CITIES/TIME`) %>% 
  count()
