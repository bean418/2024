# numeric
# Acetone
nr_at = nrow(at)-1
rn_at = rownames(at);rn_at
cn_at = colnames(at);cn_at
ncol(at)
# NH3
nr_nh3 = nrow(nh3)-1
rn_nh3 = rownames(nh3)
cn_nh3 = colnames(nh3)
ncol(nh3)


# Acetone
if(ncol(at)==9){
  numeric_data <- as.numeric(unlist(at[2:nrow(at), -2]));numeric_data
  df_ac = as.data.frame(matrix(numeric_data,
                               byrow = F,
                               nrow=nr_at))
} else{
  numeric_data <- as.numeric(unlist(at[2:nrow(at), ]));numeric_data
  df_ac = as.data.frame(matrix(numeric_data,
                               byrow = F,
                               nrow=nr_at))
}

# NH3
if(ncol(nh3)==9){
  numeric_data <- as.numeric(unlist(nh3[2:nrow(nh3), -2]));numeric_data
  df_nh3 = as.data.frame(matrix(numeric_data,
                                byrow = F,
                                nrow=nr_nh3))
} else{
  numeric_data <- as.numeric(unlist(nh3[2:nrow(nh3), ]));numeric_data
  df_nh3 = as.data.frame(matrix(numeric_data,
                                byrow = F,
                                nrow=nr_nh3))
}


# rounding
df_ac[, c(1, 3, 5, 7)] <- lapply(df_ac[, c(1, 3, 5, 7)], function(x) (round(x / 100) * 100))
df_nh3[, c(1, 3, 5, 7)] <- lapply(df_nh3[, c(1, 3, 5, 7)], function(x) (round(x / 100) * 100))

# ploting
for(i in 1:ncol(df_ac)-1){
  x_ac=c()
  x_ac=unique(df_ac[,i])
  
  y_ac=c()
  cnt = 1
  for(j in x_ac){
    y_ac[cnt] = mean(df_ac[,i+1][df_ac[,i] == j])
    cat(i)
    cnt = cnt + 1
  }
  plot(x_ac, y_ac)
  filename <- sprintf("myplot_%d.png", i)
  png(filename=filename, width=300,height=600,unit="px",bg="transparent")
  
  i = i+1
}
dev.off()
