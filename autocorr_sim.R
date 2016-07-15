library(wildclusterbootsim)
library(DBI)
library(dplyr)
library(foreign)

###### Specify Parameters ######

#Specify DGP function
dgp <- autocorr_dgp

#Specify dgp parameters
t <- c(5, 6)
G <- 5
ng <- 30
rho <- 1
lambda <- 1
gamma <- 1

#Create list of dgp_args
dgp_args <- list(t = t, G = G, ng = ng, rho = rho, lambda = lambda, gamma = gamma)

#Specify bootstrap parameters
formula <- 'Y ~ X'
reps <- 100
x_interest <- 'X'
clusterby <- c('G', 'tG')
boot_dist <- 'six_pt'
boot_reps <- 399
bootby <- c('G', 'tG', 'i')
H0 <- 1
cores <- 3
progbar = TRUE
save_type = 'db'
save_file <- 'wildclusterbootsim.db'

###### Run Simulation ######

muli_wild_cluster_boot_mc(dgp = dgp, dgp_args = dgp_args, formula = formula, reps = reps,
                          x_interest = x_interest, clusterby = clusterby, boot_dist = boot_dist, boot_reps = boot_reps,
                          bootby = bootby,  H0 = H0, cores = cores, progbar = progbar,
                          save_type = save_type, save_file = save_file)

###### Check results

#Get data
con <- dbConnect(RSQLite::SQLite(), 'firstwildclusterbootsim.db')
p_values <- dbReadTable(conn = con, name = 'p_values')
dbDisconnect(conn = con)

#Check counts
p_values %>%
  group_by_(.dots = names(dgp_args)) %>%
  summarise(n = n())

#Get boot combination names
boot_names <- names(p_values)[!names(p_values) %in% names(dgp_args)]

#Function for getting values less than alpha
less_than_alpha <- function(x) x < 0.05

#Get rejection frequencies
p_values %>%
  mutate_each_(funs = funs(less_than_alpha), vars = boot_names) %>%
  group_by_(.dots = names(dgp_args)) %>%
  summarise_each(funs(mean))

#Write results to Stata file
write.dta(p_values, "~/p_values.dta")
