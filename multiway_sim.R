library(wildclusterboot)
library(dgpmc)

formula = Y ~ W
x_interest = 'W'
clusterby = ~ G + H
boot_dist = 'two_pt'
boot_reps = 399
H0 = 1

stat_args <- list(data = data, formula = formula, x_interest = x_interest,
                  clusterby = clusterby, boot_dist = boot_dist,
                  boot_reps = boot_reps, H0 = H0)

stat_func <- function(data, formula, x_interest, clusterby, boot_dist, boot_reps, bootby = clusterby, H0 = 0){
  
  model <- lm(data = data, formula = formula)
  
  p <- t_wild_cluster_boot(data = data, model = model, x_interest = x_interest,
                           clusterby = clusterby, boot_dist = boot_dist,
                           boot_reps = boot_reps, H0 = H0)
  
  return(p)
  
}

num_dims <- 2
groups <- c(10, 10)

rand_args <- list(num_dims = num_dims, groups = groups)
rand_func <- multiway_DGP

reps <- 100
names <- c('p')
progbar <- TRUE
cores <- 3

mc_result <- dgpmc(reps = reps, stat_func = stat_func, stat_args = stat_args,
                   rand_func = rand_func, rand_args = rand_args,
                   names = names, progbar = progbar, cores = cores)

results_table <- cbind(clustetby = as.character(clusterby)[2], boot_dist = boot_dist,
                       num_dims = num_dims, H = groups[1], G = groups[2], mc_result$stat)

db_name <- 'multiway.db'
table_name <- 'twoway_cluster'

conn <- DBI::dbConnect(RSQLite::SQLite(), db_name)
DBI::dbWriteTable(conn = conn, name = table_name, value = results_table, append = TRUE)
