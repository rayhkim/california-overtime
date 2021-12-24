source('utils.R')

prune = function(data, dep_var, start_year, end_year) {
  # Remove units that don't have enough observations
  df = get_synth_data(data, start_year, end_year, dep_var)
  df = df %>%
    group_by(YEAR, ID_STR) %>%
    filter(n() >= 30)
  # Aggregate by year and unit
  df = setDT(df)[,lapply(.SD, weighted.mean, WEIGHT, na.rm=T), by=.(YEAR, ID_STR)]
  df = filter(df, DEPVAR > 0)
  df = remove_id(df)
  
  return(df$ID_STR)
}

get_pruned_clusters = function(data) {
  controls = prune(data, 'MINWAGE1', 2014, 2018)
  controls = intersect(controls, prune(data, 'MINWAGE2', 2014, 2018))
  return(controls)
}

