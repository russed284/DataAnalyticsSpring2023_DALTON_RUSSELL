
library(tidyverse)
library(vader)

df_news = read_csv('Guardians_Russia_Ukraine.csv')

df_news %>% head()

vscores <- df_news$articles %>% lapply(get_vader)
df_news <- df_news %>% mutate(
  compound = vscores %>% sapply(function(v) {as.numeric(v["compound"])} ),
  pos = vscores %>% sapply(function(v) {as.numeric(v["pos"])} ),
  neu = vscores %>% sapply(function(v) {as.numeric(v["neu"])} ),
  neg = vscores %>% sapply(function(v) {as.numeric(v["neg"])} ),
)


df_news %>% arrange(desc(compound)) %>% head(10)
df_news %>% arrange(compound) %>% head(10)

write.csv(df_news, "C:\\Users\\Owner\\newssent.csv", row.names=FALSE)
