library(tidyverse)
library(vader)

df_newst = read_csv('NYT_Russia_Ukraine.csv')

df_newst %>% head()

vscores <- df_newst$articles %>% lapply(get_vader)
df_newst <- df_newst %>% mutate(
  compound = vscores %>% sapply(function(v) {as.numeric(v["compound"])} ),
  pos = vscores %>% sapply(function(v) {as.numeric(v["pos"])} ),
  neu = vscores %>% sapply(function(v) {as.numeric(v["neu"])} ),
  neg = vscores %>% sapply(function(v) {as.numeric(v["neg"])} ),
)


df_newst %>% arrange(desc(compound)) %>% head(10)
df_newst %>% arrange(compound) %>% head(10)

write.csv(df_newst, "C:\\Users\\Owner\\nytsent.csv", row.names=FALSE)
