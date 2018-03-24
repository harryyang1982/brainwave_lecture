library(tidyverse)

ggplot(mpg) +
  geom_histogram(aes(cty))

ggplot(mpg) +
  geom_bar(aes(cty))

ggplot(mpg) +
  geom_col(aes(displ, cty, fill = class))

ggplot(mpg) +
  geom_col(aes(displ, cty, fill = class), position = "dodge")

mpg %>% 
  filter(class %in% c("suv", "2seater", "midsize")) %>% 
  ggplot() +
  geom_col(aes(displ, cty, fill = class), position = "dodge")

card_purchase <- read_csv("https://github.com/harryyang1982/brainwave_lecture/raw/master/card_purchase_total.csv")

sum_purchase <- card_purchase %>% 
  group_by(`FEST_NM(축제명)`,`YYYYMMDD(년월일)`) %>% 
  summarise(purchase = mean(`SUM_MONEY(매출_금액합)`))

ggplot(sum_purchase) +
  geom_col(aes(`YYYYMMDD(년월일)`, purchase, fill = `FEST_NM(축제명)`), position = "dodge")

sum_age_purchase <- card_purchase %>% 
  group_by(`AGE_CD(연령대별코드)`) %>% 
  summarise(purchase = mean(`SUM_MONEY(매출_금액합)`))

sum_age_purchase

ggplot(sum_age_purchase) +
  geom_col(aes(`AGE_CD(연령대별코드)`, purchase, fill = purchase))