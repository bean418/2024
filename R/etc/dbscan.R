library(dbscan)

rm(list=ls())
setwd("/Users/bean418/PCRL/data")
dat <- read.csv("MS_1_1.csv")

df_desc <- dat %>%
  group_by(SMILES) %>%
  arrange(desc(Intensity)) %>%
  slice_head(n = 5) %>% 
  ungroup()

clustering_data <- df_desc %>% 
  select(m.z, Intensity) %>%
  scale()

# DBSCAN 수행
result = c()
for(i in seq(from=0.01, to=0.3, by=0.001)){
  dbscan_result <- dbscan(clustering_data, eps = i, minPts = 3)
  n_clu <- length(unique(dbscan_result$cluster))
  if(n_clu > 100){
    cat("# of cluster is", n_clu, '\n')
  }
  result = c(result, n_clu)
}
seq(from=0.01, to=0.3, by=0.001)[48]
result
max(result)


dbscan_result <- dbscan(clustering_data, eps = 0.057, minPts = 3)
# 결과를 원본 데이터에 추가
df_desc$cluster <- dbscan_result$cluster

ggplot(df_desc, aes(x = m.z, y = Intensity, color = factor(cluster))) +
  geom_point(aes(size = weight)) +
  facet_wrap(~SMILES) +
  labs(title = "Clustering Results by SMILES",
       x = "m/z", y = "Intensity", color = "Cluster") +
  theme_minimal()
