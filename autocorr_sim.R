library(wildclusterbootsim)

#Specify your database name
db <- 'wildclusterbootsim.db'

#Specify database table names
parameters_table <- 'parameters'
p_table <- 'p_values'

#Specify all your parameters
H0 <- 1
boot_dist <- 'six_pt'
boot_reps <- 399
t <- 5
G <- 5
ng <- 30
reps <- 100
rho <- 1

#Create parameters table (only need to run once)
parameter_list <- list(boot_dist = boot_dist, boot_reps = boot_reps, t = t, G = G, ng = ng, rho = rho)
db_write_param(db = db, table = parameters_table, parameters = parameter_list)

#Get values from parameters table
con <- con <- dbConnect(RSQLite::SQLite(), db)
parameters <- dbReadTable(conn = con, name = parameters_table)
dbDisconnect(conn = con)

#Run your simulation on each row of parameters table
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

#Get all the p-values and check rejection frequencies
con <- con <- dbConnect(RSQLite::SQLite(), db)
p_values <- dbReadTable(conn = con, name = 'p_values')
colMeans(p_values < 0.05)
dbDisconnect(conn = con)

#REMOVE TABLES - ONLY USE IF YOU'RE STARTING OVER
con <- con <- dbConnect(RSQLite::SQLite(), db)
dbRemoveTable(conn = con, name = parameters_table)
dbRemoveTable(conn = con, name = 'p_values')
dbDisconnect(conn = con)
