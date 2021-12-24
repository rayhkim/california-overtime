library(data.table)
library(ggplot2)
library(dplyr)
library(seas)
library(lubridate)
library(reshape2)
library(knitr)
library(kableExtra)
library(here)

ind_groups = list(c(170,180,290),
                  190:280,
                  370:490,
                  770,
                  2470:2590,
                  2670:2990,
                  3070:3291,
                  3365:3390,
                  c(3470, 3490),
                  3570:3690,
                  3770:3875,
                  3895,
                  3960:3990,
                  1070:1290,
                  c(1370, 1390),
                  1470:1790,
                  1870:1990,
                  c(2070,2090),
                  2170:2290,
                  2370:2390,
                  4070:4590,
                  4670:5790,
                  6070:6390,
                  570:690,
                  6470:6490,
                  c(6570,6590),
                  6670,
                  6675,
                  c(6680,6690),
                  c(6692,6695),
                  c(6770,6780),
                  6870:6970,
                  6990,
                  7070,
                  7080:7190,
                  7270:7490,
                  7570,
                  7580:7780,
                  7790,
                  7860:7890,
                  8190,
                  7970:8180,
                  8370:8470,
                  8560:8590,
                  c(8660,8670),
                  c(8680,8690),
                  8770:8890,
                  8970:9090,
                  9160:9190,
                  9290,
                  9370:9590,
                  9890)

ca_data = read.csv(here("data", "california.csv"))
farmer_data = read.csv(here("data", "farmers.csv"))

season_to_date = function(df) {
  df$END = as.Date(x = integer(0), origin = "1970-01-01")
  for (y in unique(df$YEAR)) {
    for (s in unique(df$SEASON)) {
      m = switch(s, "DJF"=3, "MAM"=6, "JJA"=9, "SON"=12)
      df$END[(df$YEAR == y) & (df$SEAS == s)] = 
        make_date(y, m, 1)
    }
  }
  df$START = df$END %m-% months(3)
  df$END = df$END %m-% days(1)
  df$MID = df$START + (df$END - df$START) / 2
  return(df)
}



### Hours worked
df = filter(ca_data,
            (YEAR >= 2007) &
            (IND > 0) &
            (AHRSWORK1 != 999) &
            (ASECFLAG %in% c(NA, 2)))
df$date = as.Date(sprintf("%d-%02d-01", df$YEAR, df$MONTH), 
                  format="%Y-%m-%d")
df$SEASON <- mkseas(x = df, width = "DJF")
df$YEAR[df$MONTH == 12] = df$YEAR[df$MONTH == 12] + 1

for (i in 1:52) {
  df$IND[df$IND %in% ind_groups[[i]]] = i
}
df = setDT(df)[,lapply(.SD, weighted.mean, WTFINL, na.rm=T), by=.(YEAR, SEASON, IND)]

df = season_to_date(df)

ggplot(df, aes(x=MID, y=AHRSWORK1, group=IND)) +
  geom_rect(aes(xmin=START, xmax=END, ymin=-Inf, ymax=Inf, fill=SEASON)) +
  geom_line(aes(color=factor(IND))) + 
  # geom_point(aes(shape=factor(IND)), size=2, show.legend = FALSE) +
  scale_fill_manual(name="Season",
                    values = c("#bbbbbb", "#cccccc", "#dddddd", "#eeeeee"),
                    labels = c("Winter", "Spring", "Summer", "Fall")) +
  labs(title = "Average weekly hours worked in California agricultural industries by year and season",
       x = "Year",
       y = "Weekly hours worked") +
  theme(text=element_text(family="serif")) +
  geom_vline(xintercept=make_date(2019, 1, 1), linetype=5) +
  scale_x_date(date_breaks = "years" , date_labels = "%Y")
                  

# 370:490
# 770
# 2470:2590
# 3070:3291
# 3960:3990
# 1470:1790
# 2170:2290





### Histogram of hours worked
df = filter(ca_data, (IND %in% c(170, 180, 290)) &
              (OCC == 6050) &
              (YEAR >= 2014) &
              (YEAR <= 2020) &
              (AHRSWORK1 > 0) &
              (AHRSWORK1 != 999) &
              (ASECFLAG %in% c(NA, 2)))

df$HOURS = cut(-df$AHRSWORK1, -c(-1, 40, 45, 50, 55, 60, 168), right=FALSE)

local({
  on.exit(dev.off())
  tikz('foo.tex')
  p = ggplot(df, aes(x=factor(YEAR), fill=HOURS)) +
    geom_bar(position="fill") +
    scale_fill_manual(name="Intervals",
                      values = c("#000000", "#333333", "#555555", "#777777", "#999999", "#bbbbbb"),
                      labels = c("(60, 168]", "(55, 60]", "(50, 55]", "(45, 50]", "(40, 45]", "(0, 40]")) +
    scale_x_discrete(labels = 2014:2020) + 
    labs(title = "Hours worked per week in California agriculture",
         x = "Year",
         y = "") +
    theme(text=element_text(family="serif"))
  print(p)
})











workers = dplyr::filter(ca_data, YEAR %in% 2018:2019,
                        IND %in% c(ind_groups[[1]], ind_groups[[46]]),
                        AHRSWORK1 != 999,
                        ASECFLAG %in% c(NA, 2))
workers$IND = ifelse(workers$IND %in% ind_groups[[1]], 1, 0)

# CPS variables
workers.cps = dplyr::select(workers, ind=IND, hours=AHRSWORK1, cpswt=WTFINL)
workers.cps$log_age = log(workers$AGE)
workers.cps$highschool = ifelse(workers$EDUC >= 73, 1, 0)
workers.cps$married = ifelse(workers$MARST == 1, 1, 0)
workers.cps$notcitizen = ifelse(workers$CITIZEN == 5, 1, 0)
workers.cps$male = ifelse(workers$SEX == 1, 1, 0)
workers.cps$white = ifelse(workers$RACE == 100, 1, 0)

# CPS rotation group variables
workers.rot = dplyr::select(workers, ind=IND, rotwt=EARNWT)
workers.rot$log_hourlywage = log(workers$HOURWAGE)
workers.rot$log_hourlywage[workers.rot$log_hourlywage == log(999.99)] = NA
workers.rot$union = workers$UNION
workers.rot$union[workers$union == 0] = NA
workers.rot$union[workers$union == 1] = 0
workers.rot$union[workers$union > 1] = 1
workers.rot$log_weeklyearn = log(workers$EARNWEEK)
workers.rot$log_weeklyearn[workers.rot$log_weeklyearn == log(9999.99)] = NA

# Aggregate and weight data
workers.cps.agg = setDT(workers.cps)[,lapply(.SD, weighted.mean, cpswt, na.rm=T), by=.(ind)]
workers.rot.agg = setDT(workers.rot)[,lapply(.SD, weighted.mean, rotwt, na.rm=T), by=.(ind)]
workers.agg = merge(workers.cps.agg, workers.rot.agg, by='ind')
workers$ind = NA
workers.agg = t(workers.agg)

sample_size = kable(workers.agg, format = "latex",
                    align = "cc",
                    caption = "Summary Statistics by industry for CA 2018-2019",
                    col.names = c("Agriculture", 
                                  "Food services and drinking places")) %>%
  column_spec(2:3, width = "2cm", latex_valign = "b") %>%
  add_footnote("Note: The 2021 data is incomplete and ends in October", notation = 'none') %>%
  kable_styling(position="center", latex_options = "hold_position") 

print(sample_size)

fileConn<-file("summary-stats.tex")
writeLines(sample_size, fileConn)
close(fileConn)




