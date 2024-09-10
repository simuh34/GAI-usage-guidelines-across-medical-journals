library(openxlsx)
library(dplyr)
library(reshape2)
library(ggplot2)

# database1
df <- read.xlsx("D:/R project/GAL/Database 1 - top journals 20240720.xlsx",colNames = FALSE)
colnames(df) <- df[2, ]
df <- df[-c(1:2), ]
s_df <- df[, c('Impact factor ', 'SJR')] 
names(s_df) <- c("IF", "SJR") 
s_df$IF <- as.numeric(s_df$IF)
s_df$SJR <- as.numeric(s_df$SJR)
s_df <- na.omit(s_df[, c("IF", "SJR")])

#Pearson correlation coefficient
cor(s_df$IF, s_df$SJR, use = "complete.obs")
## cor = 0.789
cat("Pearson correlation coefficient(TOP JOURNAL): ", cor(s_df$IF, s_df$SJR, use = "complete.obs"))


#Linear Regression
model <- lm(IF ~ SJR, data = s_df)
summary(model) 

# database2
df2 <- read.xlsx("D:/R project/GAL/Database 2 - whole journals 240720.xlsx",colNames = FALSE,)
colnames(df2) <- df2[2, ]
df2 <- df2[-c(1:2), ]
s_df2 <- df2[, c('Impact factor ', 'SJR')] 
names(s_df2) <- c("IF", "SJR") 
s_df2$IF <- as.numeric(s_df2$IF)
s_df2$SJR <- as.numeric(s_df2$SJR)
s_df2 <- na.omit(s_df2[, c("IF", "SJR")])

#Pearson correlation coefficient
cor(s_df2$IF, s_df2$SJR, use = "complete.obs")
## cor = 0.925


# database 2
## conclude all values
df2 <- read.xlsx("D:/R project/GAL/Database 2 - whole journals 240720.xlsx",colNames = FALSE)
colnames(df2) <- df2[2, ]
df2 <- df2[-c(1:2), ]
s_df2 <- df2[, c('Impact factor ', 'SJR')] 
names(s_df2) <- c("IF", "SJR") 
s_df2$IF <- as.numeric(s_df2$IF)
s_df2$SJR <- as.numeric(s_df2$SJR)
s_df2 <- na.omit(s_df2[, c("IF", "SJR")])

#Pearson correlation coefficient
cor(s_df2$IF, s_df2$SJR, use = "complete.obs")
cat("Pearson correlation coefficient(TOP JOURNAL): ", cor(s_df2$IF, s_df2$SJR, use = "complete.obs"))
## cor = 0.928

## convert unofficial value into missing
df2 <- read.xlsx("D:/R project/GAL/Database 2 - whole journals 240720.xlsx",colNames = FALSE)
colnames(df2) <- df2[2, ]
df2 <- df2[-c(1:2), ]
s_df2 <- df2[, c('Impact factor ', 'SJR',"IF Y/N")] 
names(s_df2) <- c("IF", "SJR","IF Y/N") 
s_df2$IF <- as.numeric(s_df2$IF)
s_df2$SJR <- as.numeric(s_df2$SJR)
s_df2$IF[s_df2$"IF Y/N" == 1] <- NA

#Pearson correlation coefficient
cor(s_df2$IF, s_df2$SJR, use = "complete.obs")
## cor = 0.921

## correlation between 4 factor
df_1 <- read.xlsx("D:/R project/GAL/Database 1 - top journals 20240805.xlsx",startRow = 1)
df_2 <- read.xlsx("D:/R project/GAL/Database 2 - whole journals 20240805.xlsx",startRow = 1)
df_corr1 <- data.frame(
  Impactfactor = as.numeric(df_1$Impact.factor),
  SJR = as.numeric(df_1$SJR),
  CiteScore = as.numeric(df_1$CiteScore),
  Eigenefactor =as.numeric( df_1$Eigenefactor)
)
df_corr1 <- na.omit(df_corr1)
corre_df1 <- cor(df_corr1)
cor_long <- melt(corre_df1)

df_2 <- na.omit(df_2)
df_corr2 <- data.frame(
  Impactfactor = as.numeric(df_2$Impact.factor),
  SJR = as.numeric(df_2$SJR),
  CiteScore = as.numeric(df_2$CiteScore),
  Eigenefactor =as.numeric( df_2$Eigenefactor)
)
corre_df2 <- cor(df_corr2)
cor_long2 <- melt(corre_df2)

# p value
## df1
p_result <- cor.test(df_corr1$Impactfactor, df_corr1$SJR)  
print(p_result$p.value)  
p_result <- cor.test(df_corr1$Impactfactor, df_corr1$CiteScore)  
print(p_result$p.value)
p_result <- cor.test(df_corr1$Impactfactor, df_corr1$Eigenefactor)  
print(p_result$p.value)
p_result <- cor.test(df_corr1$SJR, df_corr1$CiteScore)  
print(p_result$p.value)
p_result <- cor.test(df_corr1$SJR, df_corr1$Eigenefactor)  
print(p_result$p.value)
p_result <- cor.test(df_corr1$CiteScore, df_corr1$Eigenefactor)  
print(p_result$p.value)
## df2
p_result <- cor.test(df_corr2$Impactfactor, df_corr2$SJR)  
print(p_result$p.value)  
p_result <- cor.test(df_corr2$Impactfactor, df_corr2$CiteScore)  
print(p_result$p.value)
p_result <- cor.test(df_corr2$Impactfactor, df_corr2$Eigenefactor)  
print(p_result$p.value)
p_result <- cor.test(df_corr2$SJR, df_corr2$CiteScore)  
print(p_result$p.value)
p_result <- cor.test(df_corr2$SJR, df_corr2$Eigenefactor)  
print(p_result$p.value)
p_result <- cor.test(df_corr2$CiteScore, df_corr2$Eigenefactor)  
print(p_result$p.value)

# heatmap
## top journal
cor_long <- cor_long[upper.tri(corre_df1, diag = TRUE),]
plot1 <- ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#2c7fb8", high = "#e31a1c", mid = "white", 
                       midpoint = 0.5, limit = c(0, 1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(size = 12),legend.position = "none",plot.title = element_text(hjust = 0.5)) + 
  coord_fixed() +
  labs(x = "", y = "")+ggtitle("Top SJR journals")

## whole spectrum
cor_long2 <- cor_long2[upper.tri(corre_df2, diag = TRUE),]
plot2 <- ggplot(cor_long2, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#2c7fb8", high = "#e31a1c", mid = "white", 
                       midpoint = 0.5, limit = c(0, 1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(size = 12),legend.position = "none",plot.title = element_text(hjust = 0.5)) + 
  coord_fixed() +
  labs(x = "", y = "")+ggtitle("Whole-range SJR journals")

##extract plot
combined_plot <- wrap_plots(plot1, plot2)
print(combined_plot)
ggsave("D:\\R project\\GAL\\correlation_plot_conbined.png", plot = combined_plot, width = 20, height = 6, dpi = 300)
