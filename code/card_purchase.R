library(tidyverse)

card <- read_csv("card_purchase.csv")

# str(card$`SUM_MONEY(매출_금액합)`)
# 
# card$`SUM_MONEY(매출_금액합)` <- sample(seq(5000, 100000, 500), nrow(card), replace = T)
# card$`SUM_COUNT(매출_건수합)` <- sample(seq(1, 10, 1), nrow(card), replace = T)
# 
# write_csv(card, "card_purchase.csv")
# 


card2 <- read_csv("card_purchase2.csv")
card3 <- read_csv("card_purchase3.csv")

card2$`SUM_MONEY(매출_금액합)` <- sample(seq(5000, 50000, 500), nrow(card2), replace = T)
card2$`SUM_COUNT(매출_건수합)` <- sample(seq(1, 10, 1), nrow(card2), replace = T)

card3$`SUM_MONEY(매출_금액합)` <- sample(seq(2000, 100000, 500), nrow(card3), replace = T)
card3$`SUM_COUNT(매출_건수합)` <- sample(seq(1, 10, 1), nrow(card3), replace = T)

card_sum <- bind_rows(card, card2, card3)

write_csv(card_sum, "card_purchase_total.csv")
