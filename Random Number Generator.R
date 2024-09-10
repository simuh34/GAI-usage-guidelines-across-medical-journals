library(dplyr)
df <- read.csv("D:/Chrome/Method 1-V3-20231130.xlsx - 1-200.csv")
df <- df[!is.na(df$SJR), ]


ranking <- 1:7174  
select_ranking <- function(data, step = 100, n = 2) {
  split_data <- split(data, ceiling(seq_along(data) / step))
  result <- lapply(split_data, function(segment) {
    if (length(segment) >= n) {
      sample(segment, n)
    } else {
      segment  
    }
  })
  return(result)
}
selected_journal  <-  select_ranking(ranking)
selected_journal  <-  selected_journal %>% unlist() %>% as.data.frame %>%  `names<-`(c("rank_n"))




