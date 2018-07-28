

#load data with weather and residuals added
so_data = readRDS(file = "./dataframes/weather_and_residuals_added.rds")
#create model for total_sales, with prediction and weather as inputs (but not residuals)
#variables                    <- 'PRCP+SNOW+TMAX+TMIN+weekday+endofmonth+day+week+month+day_tot+loc+holiday+tickets+ticket_accounting+anniversary+corset+goth_ball+sale+tfw+event+pred_val+TMAX*month+TMAX*anniversary+TMAX*corset+TMAX*goth_ball+TMAX*sale+TMAX*tfw+TMAX*event+PRCP*month+PRCP*anniversary+PRCP*corset+PRCP*goth_ball+PRCP*sale+PRCP*tfw+PRCP*event+TMIN*month+TMIN*anniversary+TMIN*corset+TMIN*goth_ball+TMIN*sale+TMIN*tfw+TMIN*event'
variables                    <- 'PRCP+SNOW+TMAX+weekday+endofmonth+day+week+month+day_tot+loc+holiday+tickets+ticket_accounting+anniversary+corset+goth_ball+sale+tfw+event+pred_val+TMAX*month+TMAX*anniversary+TMAX*corset+TMAX*goth_ball+TMAX*sale+TMAX*tfw+TMAX*event+PRCP*month+PRCP*anniversary+PRCP*corset+PRCP*goth_ball+PRCP*sale+PRCP*tfw+PRCP*event'
'model of untransformed response'
response      <- 'total_sales'
model         <- lm(paste(response,' ~ ',variables),data=so_data)
summary(model)
