library(data.table)
library(ggplot2)
library(dplyr)
library(sandwich)
library(lmtest)
library(lubridate)
library(stargazer)
library(ggpubr)
library(estimatr)
library(here)

source('utils.R')
source('getcontrols.R')

ca_data = read.csv(here("../data", "california.csv"))
clusters = get_pruned_clusters(ca_data)

add_controls = function(df) {
  df$HIGHSCHOOL = ifelse(df$EDUC >= 73, 1, 0)
  df$MARRIED = ifelse(df$MARST == 1, 1, 0)
  df$MALE = ifelse(df$SEX == 1, 1, 0)
  df$WHITE = ifelse(df$RACE == 100, 1, 0)
  df$HISPANIC = ifelse(df$HISPAN != 0, 1, 0)
  df$CITY = ifelse(df$METRO == 2, 1, 0)
  df$SUBURB = ifelse(df$METRO > 2, 1, 0)

  df = dplyr::select(df, DEPVAR, WEIGHT, YEAR, MONTH, ID, IND, OCC,
                     AGE, HIGHSCHOOL, MARRIED,
                     MALE, WHITE, HISPANIC,
                     CITY, SUBURB)
  return(df)
}

find_mean = function(df) {
  df$TREAT = ifelse(df$ID == '1-6050', 1, 0)
  df$DEPVAR = df$DEPVAR * df$WEIGHT
  df = setDT(dplyr::select(df,-ID))[,lapply(.SD, sum, na.rm=T), by=.(YEAR, TREAT)]
  df$DEPVAR = df$DEPVAR / df$WEIGHT
  return(df)
}

did_fe = function(df, leads, lags) {
  df = dplyr::select(df, DEPVAR, YEAR, ID, WEIGHT)
  for (i in leads:1) {
    name = sprintf("Dm%d", i)
    df[name] = ifelse((df$ID == '1-6050') & (df$YEAR == 2018-i), 1, 0)
  }
  for (i in 1:lags) {
    name = sprintf("D%d", i)
    df[name] = ifelse((df$ID == '1-6050') & (df$YEAR == 2018+i), 1, 0)
  }
  df$ID = factor(df$ID)
  df = within(df, ID <- relevel(ID, ref=2))
  df$YEAR = factor(df$YEAR)
  
  reg = lm_robust(DEPVAR ~ .-WEIGHT,
                  data=df, weights=WEIGHT)
  return(reg)
}
did_controls_fe = function(df, leads, lags) {
  df$LOG_AGE = log(df$AGE)
  df = dplyr::select(df, DEPVAR, LOG_AGE, 
                     HIGHSCHOOL, MARRIED, 
                     MALE, WHITE, CITY, 
                     SUBURB, YEAR, ID, WEIGHT)
  if (leads > 0) {
  for (i in leads:1) {
    name = sprintf("Dm%d", i)
    df[name] = ifelse((df$ID == '1-6050') & (df$YEAR == 2018-i), 1, 0)
  }
  }
  if (lags > 0) {
  for (i in 1:lags) {
    name = sprintf("D%d", i)
    df[name] = ifelse((df$ID == '1-6050') & (df$YEAR == 2018+i), 1, 0)
  }
  }
  df$ID = factor(df$ID)
  df = within(df, ID <- relevel(ID, ref=2))
  df$YEAR = factor(df$YEAR)
  
  reg = lm_robust(DEPVAR ~ .-WEIGHT,
                  data=df, weights=WEIGHT)
  return(reg)
}

# Update this function
did_placebo = function(df, leads, treat_year) {
  df$LOG_AGE = log(df$AGE)
  df = dplyr::select(df, DEPVAR, LOG_AGE, 
                     HIGHSCHOOL, MARRIED, 
                     MALE, WHITE, CITY, 
                     SUBURB, YEAR, ID, WEIGHT)
  df$ID = factor(df$ID)
  df$YEAR = factor(df$YEAR)
  values = c()
  for (c in clusters) {
    for (i in leads:0) {
      name = sprintf("DID%d", i)
      df[name] = ifelse((df$ID == c) & (df$YEAR == treat_year-i), 1, 0)
    }
    reg = lm_robust(DEPVAR ~ .-WEIGHT,
                    data=df, weights=WEIGHT)
    values = c(values, sprintf("%s: %0.5f", c, tail(reg$coefficients,1)))
  }
  return(values)
}

plot_data = function(dep_var, 
                     title, x, y) {
  df = get_did_data(ca_data,
                start_year = 2014,
                end_year = 2020,
                dep_var = dep_var,
                clusters = test)
  df = find_mean(df)
  df$DATE = make_date(df$YEAR, 1, 1)
  p = ggplot(df, aes(y=DEPVAR, x=DATE, color=factor(TREAT))) +
    geom_line(size=2) +
    scale_x_date(date_breaks = "years" , date_labels = "%Y") +
    scale_color_discrete(labels = c("Control", "Treatment")) +
    labs(title = title, x = x, y = y, color="Group") +
    geom_vline(xintercept = make_date(2018, 1, 1), linetype="dashed") +
    theme(text=element_text(family="serif"),
          plot.margin = unit(c(1, 1, 1, 1), "cm"))
  return(p)
}

# 22-9620

test = c('22-9620', '1-6050')

did_test = function(df) {
  df$POST = ifelse(df$YEAR >= 2019, 1, 0)
  df$TREAT = ifelse(df$ID == '1-6050', 1, 0)
  df$DID = df$POST * df$TREAT
  df = dplyr::select(df, DEPVAR, POST, TREAT, DID, WEIGHT)
  
  reg = lm_robust(DEPVAR ~ .-WEIGHT,
                  data=df, weights=WEIGHT)
  return(reg)
}

did_test_control = function(df) {
  df$LOG_AGE = log(df$AGE)
  df$POST = ifelse(df$YEAR >= 2019, 1, 0)
  df$TREAT = ifelse(df$ID == '1-6050', 1, 0)
  df$DID = df$POST * df$TREAT
  df = dplyr::select(df, DEPVAR, LOG_AGE, 
                     HIGHSCHOOL, MARRIED, 
                     MALE, WHITE, CITY, 
                     SUBURB, POST, TREAT, DID, WEIGHT)
  
  reg = lm_robust(DEPVAR ~ .-WEIGHT,
                  data=df, weights=WEIGHT)
  return(reg)
}
  
df = get_did_data(ca_data,
              start_year = 2015,
              end_year = 2021,
              dep_var = 'LOG_EARNINGS',
              clusters = test)
df = add_controls(df)
summary(did_fe(df, 3, 3))
summary(did_controls_fe(df, 3, 3))
summary(did_test(df))
summary(did_test_control(df))


# use this
# https://www.frbsf.org/economic-research/files/wp09-08bk.pdf






p1 = plot_data("LOG_HOURWAGE", "Evolution of Log-Transformed Hourly Wage from 2016-2020", "Year", "log(hourwage)")
p2 = plot_data("MINWAGE", "Evolution of Proportion at all Min. Wages from 2016-2020", "Year", "minwage")
p3 = plot_data("MINWAGE2", "Evolution of Proportion at O25 Min. Wage from 2016-2020", "Year", "minwageo25")
p4 = plot_data("MINWAGE1", "Evolution of Proportion at U25 Min. Wage from 2016-2020", "Year", "minwageu25")

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


local({
  on.exit(dev.off())
  tikz('did-plots.tex',
       width = 16, 
       height = 10)
  print(ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2))
})

# df$HOURS = "kink"
# df[((df$IND == 1) & (df$DEPVAR <= 60)) |
#      ((df$IND != 1) & (df$DEPVAR <= 40)),]$HOURS = "below"
# df[((df$IND == 1) & (df$DEPVAR > 60)) |
#      ((df$IND != 1) & (df$DEPVAR > 40)),]$HOURS = "above"
# df$HOURS = factor(df$HOURS)
#
# train = filter(df, IND == 1, YEAR %in% 2016:2017)
# test = filter(df, YEAR == 2018)
#
# set.seed(42)
# min_obs = min(table(train$HOURS))
# rf = randomForest(HOURS~.-DEPVAR-UNIT-WEIGHT-YEAR,train,
#                   ntree=1000,
#                   sampsize=rep(min_obs, 2),
#                   strata=train$HOURS,
#                   replace=TRUE)
# print(rf)
# pred = predict(rf, filter(test, IND == 1))
# table(observed = filter(test, IND == 1)$HOURS, predicted = pred)




