library(dplyr)
library(ggplot2)

rm(list=ls())
setwd("/Users/bean418/PCRL/data")
dir()
dat <- read.csv("MS_1_1.csv")

# Preprocessing1
# Intensity
# remove under 200
df_200 <- dat %>% filter(Intensity > 200)

# Preprocessing2
# Intensity
# descending 5
df_desc <- dat %>%
  group_by(SMILES) %>%
  arrange(desc(Intensity)) %>%
  slice_head(n = 5) %>% 
  ungroup()

# Preprocessing3
# Preprocessing1
# Intensity
# remove under 20, 10, etc.
# ------------- ing

# k-means clustering
# center의 개수 = 화합물의 개수
n_center = length(unique(df_desc$SMILES))

# 필요한 열만 선택 (예: m/z와 Intensity)
clustering_data <- df_desc %>% select(m.z, Intensity)

# k-means 클러스터링 수행 (예: 3개의 클러스터)
set.seed(123) # 재현성을 위해 시드 설정
kmeans_result <- kmeans(clustering_data, centers = n_center)

# 결과 확인
print(kmeans_result)

# 클러스터 할당 결과를 원본 데이터에 추가
df_desc$cluster <- kmeans_result$cluster

# 클러스터별 데이터 요약
cluster_summary <- df_desc %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    avg_mz = mean(m.z),
    avg_intensity = mean(Intensity)
  )

print(cluster_summary)

# 시각화 (ggplot2 패키지 사용)


ggplot(df_desc, aes(x = m.z, y = Intensity, color = factor(cluster))) +
  geom_point() +
  labs(title = "K-means Clustering Result",
       x = "m/z",
       y = "Intensity",
       color = "Cluster") +
  theme_minimal()
