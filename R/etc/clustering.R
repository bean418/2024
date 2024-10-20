library(dplyr)
library(ggplot2)

rm(list=ls())
setwd("/Users/bean418/PCRL/data")
dir()
dat <- read.csv("MS_1_1.csv")

# Preprocessing
df <- dat %>%
  group_by(SMILES) %>%
  arrange(desc(Intensity)) %>%
  slice_head(n = 5) %>% 
  ungroup()
df <- df %>%
  group_by(SMILES) %>% 
  mutate(weight = seq(5, 1, -1) / 15) %>% 
  ungroup()
df <- df %>% select(-Adjusted.Intensity, -Intensity)

# preprocessing2
# generate column to m.z product weights
df <- df %>% mutate(product = m.z*weight)

n = length(unique(df$File.Title));n

prod <- c()
for(i in 1:n){
  val <- sum(df$product[(1+5*(i-1)) : (5*i)])
  prod <- c(prod, val)
}
compound <- unique(df$File.Title)

df_DR <- data.frame(compound, prod);df_DR

# clustering
clustering_data <- df_DR %>% select(prod)

set.seed(123)
kmeans_result <- kmeans(clustering_data, centers = 10)

df_DR$cluster_result <- kmeans_result$cluster


# plotting
ggplot(df_DR, aes(x = prod, y = 0, color = factor(cluster_result))) +
  geom_point(size = 4) +
  # geom_text(aes(label = compound), vjust = -1, hjust = 0, angle = 90, size = 3) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(title = "K-means Clustering of Compounds",
       x = "Product of m/z and Weight",
       y = "",
       color = "Cluster") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom")
