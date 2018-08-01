

#load data with weather and residuals added
so_data = readRDS(file = "./dataframes/weather_and_residuals_added.rds")
so_data$TMAX2                <- so_data$TMAX*so_data$TMAX
so_data$trange               <- so_data$TMAX-so_data$TMIN

#create model for total_sales, with prediction and weather as inputs (but not residuals)

vars_all_four                <- 'PRCP+SNOW+TMAX+TMIN+weekday+endofmonth+day+week+month+day_tot+loc+holiday+tickets+ticket_accounting+anniversary+corset+goth_ball+sale+tfw+event+pred_val+TMAX*month+TMAX*anniversary+TMAX*corset+TMAX*goth_ball+TMAX*sale+TMAX*tfw+TMAX*event+PRCP*month+PRCP*anniversary+PRCP*corset+PRCP*goth_ball+PRCP*sale+PRCP*tfw+PRCP*event+TMIN*month+TMIN*anniversary+TMIN*corset+TMIN*goth_ball+TMIN*sale+TMIN*tfw+TMIN*event'

vars_trange_tmax2            <- 'PRCP+SNOW+TMAX+trange+weekday+endofmonth+day+week+month+day_tot+loc+holiday+tickets+ticket_accounting+anniversary+corset+goth_ball+sale+tfw+event+pred_val+TMAX*month+TMAX*anniversary+TMAX*corset+TMAX*goth_ball+TMAX*sale+TMAX*tfw+TMAX*event+PRCP*month+PRCP*anniversary+PRCP*corset+PRCP*goth_ball+PRCP*sale+PRCP*tfw+PRCP*event+trange*month+trange*anniversary+trange*corset+trange*goth_ball+trange*sale+trange*tfw+trange*event+trange+TMAX2+trange*month+TMAX2*month'

vars_no_tmin                 <- 'PRCP+SNOW+TMAX+weekday+endofmonth+day+week+month+day_tot+loc+holiday+tickets+ticket_accounting+anniversary+corset+goth_ball+sale+tfw+event+pred_val+TMAX*month+TMAX*anniversary+TMAX*corset+TMAX*goth_ball+TMAX*sale+TMAX*tfw+TMAX*event+PRCP*month+PRCP*anniversary+PRCP*corset+PRCP*goth_ball+PRCP*sale+PRCP*tfw+PRCP*event'

'model of untransformed response'
response                     <- 'total_sales'
'all four weather vars_all_four, as is'
model                        <- lm(paste(response,' ~ ',vars_all_four),data=so_data)
saveRDS(model, file = "./models/weather_all_four.rds")
summary(model)$r.squared
summary(model)$adj.r.squared

'trange instead of TMIN, but include TMAX squared'
model2                       <- lm(paste(response,' ~ ',vars_trange_tmax2),data=so_data)
saveRDS(model2, file = "./models/weather_trange_tmax2.rds")
summary(model2)$r.squared
summary(model2)$adj.r.squared

'no TMIN'
model3                       <- lm(paste(response,' ~ ',vars_no_tmin),data=so_data)
saveRDS(model3, file = "./models/weather_no_tmin.rds")
summary(model3)$r.squared
summary(model3)$adj.r.squared
