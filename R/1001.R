library(dplyr)
library(ggplot2)
library(flexclust)

rm(list=ls())
setwd("/Users/bean418/PCRL/data")
dir()
dat <- read.csv("MS_1_1.csv")

# Intensity를 기준으로 상위 5개 항목 내림차순 구성
df_desc <- dat %>%
  group_by(SMILES) %>%
  arrange(desc(Intensity)) %>%
  slice_head(n = 5) %>% 
  ungroup()

# 가중치
df_desc <- df_desc %>%
  group_by(SMILES) %>%
  mutate(weight = seq(5, 1, -1) / 15) %>%
  ungroup()

clustering_data <- df_desc %>% select(m.z, Intensity)

# 클러스터 수 설정 (화합물의 수)
n_center <- length(unique(df_desc$SMILES))

# weightedKmeans
set.seed(123)
wkm_result <- kcca(clustering_data, k = n_center, weights = df_desc$weight)

df_desc$cluster <- predict(wkm_result)

# ggplot
ggplot(df_desc, aes(x = m.z, y = Intensity, color = factor(cluster), size = weight)) +
  geom_point(alpha = 0.6) +
  labs(title = "Weighted K-means Clustering Result",
       x = "m/z",
       y = "Intensity",
       color = "Cluster",
       size = "Weight") +
  theme_minimal()
