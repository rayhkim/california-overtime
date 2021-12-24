library(data.table)
library(ggplot2)
library(dplyr)
library(lmtest)
library(Synth)
library(SCtools)
library(here)

source('utils.R')
source('getcontrols.R')

ca_data = read.csv(here("../data", "california.csv"))
clusters = get_pruned_clusters(ca_data)

synth_control = function(dep_var,
                         start_year, 
                         end_year) {
  
  df = get_synth_data(ca_data, start_year, end_year, dep_var)
  df = filter(df, ID_STR %in% clusters)
  
  treated_code = filter(df, ID_STR == '1-6050')$ID[1]
  
  # Aggregate by year and unit
  df = setDT(df)[,lapply(.SD, weighted.mean, WEIGHT, na.rm=T), by=.(YEAR, ID, ID_STR)]
  
  df = remove_id(df)
  
  # Set control variables
  control_codes = setdiff(unique(df$ID), treated_code)
  
  # Pre-treatment period
  opt_window = start_year:2018
  
  # Set predictors as all except last pre-treatment outcome variable, which is done manually
  sp_predictors = list()
  for (i in 1:(length(opt_window)-1)) {
    sp_predictors[[i]] = list('DEPVAR', opt_window[i], 'mean')
  }
  dataprep.out <- dataprep(foo = df,
                           dependent = 'DEPVAR',
                           predictors = 'DEPVAR',
                           predictors.op = 'mean',
                           time.predictors.prior = 2018,
                           special.predictors = sp_predictors,
                           unit.variable = 'ID',
                           unit.names.variable = 'ID_STR',
                           time.variable = 'YEAR',
                           treatment.identifier = treated_code,
                           controls.identifier = control_codes,
                           time.optimize.ssr = opt_window,
                           time.plot = start_year:end_year)
  return(dataprep.out)
}

plot_path = function(df, title, x, y) {
  df$DATE = make_date(df$YEAR, 1, 1)
  p = ggplot(df, aes(x=DATE)) +
    geom_line(size=2, aes(y=TREAT)) +
    geom_line(size=2, linetype='dashed', aes(y=CONTROL)) +
    scale_x_date(date_breaks = "years" , date_labels = "%Y") +
    geom_vline(xintercept=make_date(2018,1,1), linetype='dashed') +
    labs(title = title, x = x, y = y, color="Group") +
    theme(text=element_text(family="serif"),
          plot.margin = unit(c(1, 1, 1, 1), "cm"))
  return(p)
}

start_year = 2007
end_year = 2019
dataprep.out = synth_control(dep_var = 'EARNINGS',
                             start_year = start_year, 
                             end_year = end_year)
synth.out <- synth(dataprep.out, Sigf.ipop=3, strategy='multiprocess')

df = data.frame(YEAR = start_year:end_year)
df$TREAT = dataprep.out$Y1plot
df$CONTROL = dataprep.out$Y0plot%*% synth.out$solution.w
plot_path(df, "Evolution of Hours for Treated vs Synthetic Control", "Year", "Hours")

# local({
#   on.exit(dev.off())
#   tikz('sc-hours-gaps.tex',
#        width = 8, 
#        height = 6)
#   # Plot paths of treated and synthetic
#   print(p1)
# })

# Get result tables
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
) 
print(synth.tables)


# Create placebos and test
tdf <- generate.placebos(dataprep.out, synth.out, Sigf.ipop = 3, strategy='multiprocess')
ratio <- mspe.test(tdf, discard.extreme = T, mspe.limit=1)
ratio$p.val
plot_placebos(tdf, discard.extreme = T, mspe.limit=1,xlab)
mspe.plot(tdf, discard.extreme = T, mspe.limit=1)

