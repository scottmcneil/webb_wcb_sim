library(wildclusterbootsim)

H0 <- 1
boot_dist <- 'six_pt'
boot_reps <- 399
db <- 'wildclusterbootsim.db'

t <- 5
G <- 5
ng <- 30
reps <- 100
rho <- 1

parameters_table <- 'parameters'
p_table <- 'p_values'

parameter_list <- list(boot_dist = boot_dist, t = t, G = G, ng = ng, rho = rho)
db_write_param(db = db, table = parameters_table, parameters = parameter_list)

con <- con <- dbConnect(RSQLite::SQLite(), db)
parameters <- dbReadTable(conn = con, name = parameters_table)
dbDisconnect(conn = con)

apply(X = parameters,
      MARGIN = 1,
      FUN = db_write_mc,
      db = db,
      table = p_table,
      reps = reps,
      dgp = dgp,
      formula = formula,
      bootby = bootby,
      clusterby = clusterby,
      x_interest = x_interest,
      H0 = H0,
      cores = cores)

con <- con <- dbConnect(RSQLite::SQLite(), db)
p_values <- dbReadTable(conn = con, name = 'p_values')
colMeans(p_values < 0.05)

dbRemoveTable(conn = con, name = parameters_table)
dbRemoveTable(conn = con, name = 'p_values')
dbDisconnect(conn = con)
