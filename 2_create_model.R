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

'load dataframe'
so_data = readRDS(file = "./dataframes/post_clean.rds")

#split into pre- and post- cutoff date datasets
cutoff_date   <- '2018-06-01'
f_so_dt      <- so_data[which(so_data$salesdate >= as.Date(cutoff_date, format = "%Y-%m-%d")),]
so_data       <- so_data[which(so_data$salesdate < as.Date(cutoff_date, format = "%Y-%m-%d")),]

variables     <- 'weekday+week+month+loc+event+endofmonth+day_tot+corset+anniversary+holiday+tickets+ticket_accounting+goth_ball+tfw+sale+weekday*week*month+week*month+weekday*loc+event*week+goth_ball*week+anniversary*week+week*sale'

'model of untransformed response'
response      <- 'total_sales'
model         <- lm(paste(response,' ~ ',variables),data=so_data)
saveRDS(model, file = "./models/lm.rds")
paste('R-squared',summary(model)$r.squared,'Adjusted R-squared',summary(model)$adj.r.squared)
png(paste('./plots/density_',response,'.png', sep = ""), width = 1000, height = 600)
plot(density(so_data$total_sales),xlim=c(0,0.2))
garbage <- dev.off()

'for log of total_sales'
so_data$logts <- log(so_data$total_sales)
f_so_dt$logts <- log(f_so_dt$total_sales)
response      <- 'logts'
modellog      <- lm(paste(response,' ~ ',variables),data=so_data)
saveRDS(modellog, file = "./models/lm_log.rds")
paste('R-squared',summary(modellog)$r.squared,'Adjusted R-squared',summary(modellog)$adj.r.squared)
png(paste('./plots/density_',response,'.png', sep = ""), width = 1000, height = 600)
plot(density(so_data$logts),xlim=c(-4,-1))
garbage <- dev.off()


'for exp of total_sales'
so_data$expts <- exp(so_data$total_sales)
f_so_dt$expts <- exp(f_so_dt$total_sales)
response      <- 'expts'
modelexp      <- lm(paste(response,' ~ ',variables),data=so_data) 
saveRDS(modelexp, file = "./models/lm_exp.rds")
paste('R-squared',summary(modelexp)$r.squared,'Adjusted R-squared',summary(modelexp)$adj.r.squared)
png(paste('./plots/density_',response,'.png', sep = ""), width = 1000, height = 600)
plot(density(so_data$expts),xlim=c(.9,1.3))
garbage <- dev.off()

'for square of total_sales'
so_data$pw2ts <- so_data$total_sales^2
f_so_dt$pw2ts <- f_so_dt$total_sales^2
response      <- 'pw2ts'
modelpw2      <- lm(paste(response,' ~ ',variables),data=so_data)
saveRDS(modelpw2, file = "./models/lm_pw2.rds")
paste('R-squared',summary(modelpw2)$r.squared,'Adjusted R-squared',summary(modelpw2)$adj.r.squared)
png(paste('./plots/density_',response,'.png', sep = ""), width = 1000, height = 600)
plot(density(so_data$pw2ts),xlim=c(0,0.05))
garbage <- dev.off()

'for square root of total_sales'
so_data$sqrts <- so_data$total_sales^0.5
f_so_dt$sqrts <- f_so_dt$total_sales^0.5
response      <- 'sqrts'
modelsqr      <- lm(paste(response,' ~ ',variables),data=so_data)
saveRDS(modelsqr, file = "./models/lm_sqr.rds")
paste('R-squared',summary(modelsqr)$r.squared,'Adjusted R-squared',summary(modelsqr)$adj.r.squared)
png(paste('./plots/density_',response,'.png', sep = ""), width = 1000, height = 600)
plot(density(so_data$sqrts),xlim=c(0,0.6))
garbage <- dev.off()

'for reciprocal square root of total_sales'
so_data$rsrts <- so_data$total_sales^(-0.5)
f_so_dt$rsrts <- f_so_dt$total_sales^(-0.5)
response      <- 'rsrts'
modelrsr      <- lm(paste(response,' ~ ',variables),data=so_data)
saveRDS(modelrsr, file = "./models/lm_rsr.rds")
paste('R-squared',summary(modelrsr)$r.squared,'Adjusted R-squared',summary(modelrsr)$adj.r.squared)
png(paste('./plots/density_',response,'.png', sep = ""), width = 1000, height = 600)
plot(density(so_data$rsrts),xlim=c(0,10))
garbage <- dev.off()

'for reciprocal of total_sales'
so_data$rcpts <- so_data$total_sales^(-1)
f_so_dt$rcpts <- f_so_dt$total_sales^(-1)
response      <- 'rcpts'
modelrcp      <- lm(paste(response,' ~ ',variables),data=so_data)
saveRDS(modelrcp, file = "./models/lm_rcp.rds")
paste('R-squared',summary(modelrcp)$r.squared,'Adjusted R-squared',summary(modelrcp)$adj.r.squared)
png(paste('./plots/density_',response,'.png', sep = ""), width = 1000, height = 600)
plot(density(so_data$rcpts),xlim=c(0,100))
garbage <- dev.off()

'for 1/square of total_sales'
so_data$ng2ts <- so_data$total_sales^(-2)
f_so_dt$ng2ts <- f_so_dt$total_sales^(-2)
response      <- 'ng2ts'
modelng2      <- lm(paste(response,' ~ ',variables),data=so_data)
saveRDS(modelng2, file = "./models/lm_ng2.rds")
paste('R-squared',summary(modelng2)$r.squared,'Adjusted R-squared',summary(modelng2)$adj.r.squared)
png(paste('./plots/density_',response,'.png', sep = ""), width = 1000, height = 600)
plot(density(so_data$ng2ts),xlim=c(0,100000))
garbage <- dev.off()

png('./plots/boxcox.png', width = 1000, height = 600)
boxcox(model)
garbage <- dev.off()

write.csv(f_so_dt,'./csvs/post_cutoff.csv', row.names=FALSE)
saveRDS(f_so_dt, file = "./dataframes/post_cutoff.rds")
write.csv(so_data,'./csvs/pre_cutoff.csv', row.names=FALSE)
saveRDS(so_data, file = "./dataframes/pre_cutoff.rds")

