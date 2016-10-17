library(dgpmc)
library(wildclusterboot)

formula = Y ~ W
x_interest = 'W'
clusterby = ~ G + H
boot_dist = 'two_pt'
boot_reps = 399
H0 = 1
num_dims <- 2
groups <- c(10, 10)

stat_args <- list(formula = formula, x_interest = x_interest,
                  clusterby = clusterby, boot_dist = boot_dist,
                  boot_reps = boot_reps, H0 = H0, groups = groups)

rand_args <- list(num_dims = num_dims, groups = groups)


stat_func <- function(data, formula, x_interest, clusterby, boot_dist, boot_reps, bootby = clusterby, H0 = 0, groups){
  
  #Create model
  model <- lm(data = data, formula = formula)
  beta <- coef(model)[x_interest]
  
  #Get OLS pval
  ols_se <- summary(model)$coefficients[,'Std. Error']
  ols <- 2*pt(abs((beta - H0)/ols_se[x_interest]), df = df.residual(model), lower.tail = FALSE)
  
  #Created combined groups
  comb_data <- cbind(data, HG = paste0(data[,'H'], data[,'G']))
  
  #Get CRVE_HG pval
  crve_HG_se <- clustered_se(data = comb_data, model = model, clusterby = 'HG')
  crve_HG <- 2*pt(abs((beta - H0)/crve_HG_se[x_interest]), df = length(unique(comb_data[,'HG'])) - 1, lower.tail = FALSE)
  
  #Get CRVE_G pval
  crve_G_se <- clustered_se(data = comb_data, model = model, clusterby = 'G')
  crve_G <- 2*pt(abs((beta - H0)/crve_G_se[x_interest]), df = length(unique(comb_data[,'G'])) - 1, lower.tail = FALSE)
  
  #Get CRVE_H pval
  crve_H_se <- clustered_se(data = comb_data, model = model, clusterby = 'H')
  crve_H <- 2*pt(abs((beta - H0)/crve_H_se[x_interest]), df = length(unique(comb_data[,'H'])) - 1, lower.tail = FALSE)
  
  #Get multiway p_vals
  multiway_se <- clustered_se(data = data, model = model, clusterby = clusterby)
  multiway_comb <- 2*pt(abs((beta - H0)/multiway_se[x_interest]), df = length(unique(comb_data[,'HG'])) - 1, lower.tail = FALSE)
  multiway_min <- 2*pt(abs((beta - H0)/multiway_se[x_interest]), df = min(groups) - 1, lower.tail = FALSE)
  
  #Get multiwayboot pval
  multiwayboot <- t_wild_cluster_boot(data = data, model = model, x_interest = x_interest,
                                      clusterby = clusterby, boot_dist = boot_dist,
                                      boot_reps = boot_reps, H0 = H0)
  
  return(c(ols, crve_HG, crve_G, crve_H, multiway_comb, multiway_min, multiwayboot))
  
}


rand_func <- multiway_DGP

reps <- 1000
names <- c('ols', 'crve_HG', 'crve_G', 'crve_H', 'multiway_comb', 'multiway_min', 'multiwayboot')
progbar <- TRUE
cores <- 3
lecuyer <- TRUE
seed <- 42

mc_result <- dgpmc(reps = reps, stat_func = stat_func, stat_args = stat_args,
                   rand_func = rand_func, rand_args = rand_args, names = names,
                   progbar = progbar, lecuyer = lecuyer, seed = seed, cores = cores)

results_table <- cbind(clustetby = as.character(clusterby)[2], boot_dist = boot_dist,
                       num_dims = num_dims, H = groups[1], G = groups[2], mc_result$stat)

db_name <- 'multiway.db'
table_name <- 'twoway_cluster'

conn <- DBI::dbConnect(RSQLite::SQLite(), db_name)
DBI::dbWriteTable(conn = conn, name = table_name, value = results_table, append = TRUE)