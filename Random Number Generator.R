library(dplyr)

# random number generator
ranking <- 1:6974  
select_ranking <- function(data, step = 100, n = 2) {
  split_data <- split(data, ceiling(seq_along(data) / step))
  result <- lapply(split_data, function(segment) {
    if (length(segment) >= n) {
      sample(segment, n)
    } else {
      sample(segment, min(length(segment), n))  
    }
  })
  return(result)
}
selected_journal  <-  select_ranking(ranking)
selected_journal  <-  selected_journal %>% unlist() %>% as.data.frame %>%  `names<-`(c("rank_n"))

#Post-Stratification Weighting 
# total 
total_population <- 6974
group_size <- 100
sample_per_group <- 2
total_groups <- total_population %/% group_size
remaining_samples <- total_population %% group_size
full_group_samples <- total_groups * sample_per_group
weight_full_group <- (sample_per_group / group_size)

if (remaining_samples > 0) {
  weight_partial_group <- (2 / remaining_samples)  
} else {
  weight_partial_group <- 0  
}

cat("Weight for full groups:", weight_full_group, "\n")
cat("Weight for partial group:", weight_partial_group, "\n")
sample_weights <- c(rep(weight_full_group, full_group_samples))

if (remaining_samples >= 2) {
  sample_weights <- c(sample_weights, rep(weight_partial_group, 2)) 
}
print(sample_weights)
