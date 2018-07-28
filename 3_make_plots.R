library(tidyverse)
library(tcltk)
library(MASS)
library(modelr)
library(lubridate)
library(broom)
library(dplyr)
library(dotwhisker)
library(jtools)
library(gdata)
library(zoo)

model = readRDS(file = "./models/lm.rds")
so_data = readRDS(file = "./dataframes/pre_cutoff.rds")

pred_val <- fitted(model) # predicted values
#this part is to show us just the variables and interactions at 0.05 level
coefficients <- round(summary(model)$coef, 4)
coefficients[coefficients[, 4] < .05, ]

#make some plots of model model coefficients
model_df = tidy(model)


'days of week'
weekdays_coeff_df <- filter(model_df,term %in% c('weekday1Mon','weekday2Tue','weekday3Wed','weekday4Thu','weekday5Fri','weekday6Sat','weekday7Sun'))
png('./plots/coeff_days_of_the_week.png', width = 1000, height = 600)
dwplot(weekdays_coeff_df)
garbage <- dev.off()

'weeks of month'
week_coeff_df     <- filter(model_df,term %in% c('week1first','week2second','week3third','week4fourth','week5fifth'))
png('./plots/coeff_weeks_of_the_month.png', width = 1000, height = 600)
dwplot(week_coeff_df)
garbage <- dev.off()

'holidays'
holiday_coeff_df  <- filter(model_df,grepl('holiday',term))
png('./plots/coeff_holidays.png', width = 1000, height = 600)
dwplot(holiday_coeff_df)
garbage <- dev.off()

'non-holiday events'
event_coeff_df  <- filter(model_df,term %in% c('event','anniversary','corset','tax_free_weekend'))
png('./plots/coeff_non_holiday_events.png', width = 1000, height = 600)
dwplot(event_coeff_df)
garbage <- dev.off()

'month week interaction'
png('./plots/interaction_month_1to4_week.png', width = 1000, height = 600)
suppressWarnings(cat_plot(model, pred = week, modx = month, modxvals = c('01','02','03','04'), geom = "line", interval = FALSE))
garbage <- dev.off()
png('./plots/interaction_month_5to8_week.png', width = 1000, height = 600)
suppressWarnings(cat_plot(model, pred = week, modx = month, modxvals = c('05','06','07','08'), geom = "line", interval = FALSE))
garbage <- dev.off()
png('./plots/interaction_month_9to12_week.png', width = 1000, height = 600)
suppressWarnings(cat_plot(model, pred = week, modx = month, modxvals = c('09','10','11','12'), geom = "line", interval = FALSE))
garbage <- dev.off()

'month day-of-the-week interaction'
png('./plots/interaction_month_1to4_weekday.png', width = 1000, height = 600)
suppressWarnings(cat_plot(model, pred = weekday, modx = month, modxvals = c('01','02','03','04'), geom = "line", interval = FALSE))
garbage <- dev.off()
png('./plots/interaction_month_5to8_weekday.png', width = 1000, height = 600)
suppressWarnings(cat_plot(model, pred = weekday, modx = month, modxvals = c('05','06','07','08'), geom = "line", interval = FALSE))
garbage <- dev.off()
png('./plots/interaction_month_9to12_weekday.png', width = 1000, height = 600)
suppressWarnings(cat_plot(model, pred = weekday, modx = month, modxvals = c('09','10','11','12'), geom = "line", interval = FALSE))
garbage <- dev.off()

'weekday with week interaction'
png('./plots/interaction_week_weekday.png', width = 1000, height = 600)
suppressWarnings(cat_plot(model, pred = week, modx = weekday, modxvals = c('1Mon','2Tue','3Wed','4Thu','5Fri','6Sat','7Sun'), geom = "line", interval = FALSE))
garbage <- dev.off()

'interactions of weekday with anything else'
wdinter_coeff_df  <- filter(model_df,grepl(glob2rx('weekday*:*'),term))
wdinter_coeff_df  <- filter(wdinter_coeff_df,(as.numeric(statistic) > 2) | (as.numeric(statistic) < -2))
wdinter_coeff_df  <- wdinter_coeff_df[order(wdinter_coeff_df$term),]
png('./plots/interactions_weekday.png', width = 1000, height = 600)
dwplot(wdinter_coeff_df)
garbage <- dev.off()

'interactions of week with event'
weinter_coeff_df  <- filter(model_df,grepl(glob2rx('week*:event'),term))
weinter_coeff_df  <- weinter_coeff_df[order(weinter_coeff_df$term),]
png('./plots/interactions_week_event.png', width = 1000, height = 600)
dwplot(weinter_coeff_df)
garbage <- dev.off()


'MODEL ANALYSIS'
"residuals against fitted values"
png('./plots/analysis_of_model_1.png', width = 1000, height = 600)
plot(model,which=c(1),labels.id=so_data$salesdate,sub.caption="residuals against fitted values")
garbage <- dev.off()

"Scale-Location plot of sqrt(| residuals |) against fitted"
png('./plots/analysis_of_model_2.png', width = 1000, height = 600)
plot(model,which=c(2),labels.id=so_data$salesdate,sub.caption="Scale-Location plot of sqrt(| residuals |) against fitted")
garbage <- dev.off()

"Normal Q-Q"
png('./plots/analysis_of_model_3.png', width = 1000, height = 600)
plot(model,which=c(3),labels.id=so_data$salesdate,sub.caption="Normal Q-Q")
garbage <- dev.off()

"Cook's distances versus row labels"
png('./plots/analysis_of_model_4.png', width = 1000, height = 600)
plot(model,which=c(4),labels.id=so_data$salesdate,sub.caption="Cook's distances versus row labels")
garbage <- dev.off()

"residuals against leverages"
png('./plots/analysis_of_model_5.png', width = 1000, height = 600)
plot(model,which=c(5),labels.id=so_data$salesdate,sub.caption="residuals against leverages")
garbage <- dev.off()

"Cook's distances against leverage/(1-leverage)"
png('./plots/analysis_of_model_6.png', width = 1000, height = 600)
plot(model,which=c(6),labels.id=so_data$salesdate,sub.caption="Cook's distances against leverage/(1-leverage)")
garbage <- dev.off()

'number total sales'
NROW(so_data$total_sales)

'adding residuals'
so_data$pred_val <- fitted(model)
so_data$resid    <- NA
so_data$resid    <- so_data$pred_val - so_data$total_sales
'number we could not predict for some reason'
sum(is.na(so_data$resid)) # should be zero

'residuals by quarter'
so_data$quarterYear <- as.Date(as.yearqtr(so_data$salesdate))
sum_stat <- so_data %>%
  group_by(quarterYear) %>%
  summarise_each(funs(sum), resid)
write.csv(sum_stat,'./csvs/qtrly_sum_resid.csv', row.names=FALSE)

head(so_data[order(so_data$resid),][c("total_sales","salesdate","pred_val","resid")],10)
tail(so_data[order(so_data$resid),][c("total_sales","salesdate","pred_val","resid")],10)

#any patterns in where model is getting high error?
'residual by month dotw loc'
png('./plots/residual_by_month_dotw_loc.png', width = 1000, height = 600)
ggplot(so_data, aes(month, resid, color=weekday, shape=loc)) + 
    geom_point(position = "jitter")
garbage <- dev.off()

'comparisons of raw and modeled data'
png('./plots/predicted_and_actual_over_time.png', width = 1000, height = 600)
ggplot(data = so_data) +
  labs(x = 'days since store opened',y = 'predicted in red, actual sales in blue') +
  geom_smooth(mapping = aes(x=day_tot,y = pred_val),colour='red') +
  geom_smooth(mapping = aes(x=day_tot,y = total_sales),colour='blue') +
  xlim(0, NA)
garbage <- dev.off()

write.csv(so_data[,c("salesdate","resid")], row.names=FALSE, file = "./csvs/residuals_so.csv")

saveRDS(model, file = "./models/lm_w_resid.rds")
saveRDS(so_data, file="./dataframes/so_data_w_resid.rds")
write.csv(so_data,'./csvs/so_data_w_resid.csv', row.names=FALSE)
