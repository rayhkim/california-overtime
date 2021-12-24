library(data.table)
library(ggplot2)
library(dplyr)
library(lmtest)
library(synthdid)
library(here)

source('utils.R')
source('getcontrols.R')

ca_data = read.csv(here("../data", "california.csv"))
clusters = get_pruned_clusters(ca_data)

synthdid = function(dep_var,
                   start_year, 
                   end_year) {
  
  df = get_synth_data(ca_data, start_year, end_year, dep_var)
  df = df %>%
    group_by(YEAR, ID_STR) %>%
    filter(n() >= 30)
  # df = filter(df, ID_STR %in% clusters)
  
  # Aggregate by year and unit
  df = setDT(df)[,lapply(.SD, weighted.mean, WEIGHT, na.rm=T), by=.(YEAR, ID_STR)]
  
  df = remove_id(df)
  
  df$TREAT = ifelse((df$YEAR >= 2019) & (df$ID_STR == "1-6050"), 1, 0)
  
  setup = panel.matrices(df, 
                         unit='ID_STR', 
                         time='YEAR',
                         outcome='DEPVAR',
                         treatment='TREAT')
  return(setup)
}

setup = synthdid(dep_var = 'LOG_EARNINGS',
                 start_year = 2007, 
                 end_year = 2021)

tau.sc = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)

estimates = list(tau.sc, tau.hat)
names(estimates) = c('Synthetic Control', 'Synthetic DiD')
synthdid_plot(estimates, se.method='placebo')

synthdid_units_plot(estimates, se.method='placebo')

print(unlist(estimates))










