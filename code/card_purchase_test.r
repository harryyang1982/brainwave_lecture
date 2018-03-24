library(tidyverse)

card <- read_csv("card_purchase_total.csv")

glimpse(card)


card %>% 
  group_by(`FEST_NM(축제명)`, `AGE_CD(연령대별코드)`, `GEN_CD(성별코드)`) %>% 
  summarise(purchase_count = mean(`SUM_COUNT(매출_건수합)`),
            purchase_value = mean(`SUM_MONEY(매출_금액합)`))

card %>% 
  filter(`FEST_NM(축제명)` == "김치") %>% 
  ggplot(aes(`AGE_CD(연령대별코드)`, `SUM_MONEY(매출_금액합)`, fill = `GEN_CD(성별코드)`)) +
  geom_col(position = "dodge") +
  labs(x = "연령대", y = "매출액", title = "연령에 따른 매출")
