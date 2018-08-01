

new_data = readRDS(file = "./dataframes/post_cutoff.rds")

'untransformed'
model = readRDS(file = "./models/lm.rds")
suppressWarnings(new_data$pred_val             <- predict(model, new_data))
cor(new_data$total_sales,new_data$pred_val   )^2
write.csv(new_data,'./csvs/predictions_future.csv', row.names=FALSE)
'adding residuals'
new_data$resid                                 <- NA
new_data$resid                                 <- new_data$pred_val    - new_data$total_sales
saveRDS(new_data, file="./dataframes/new_data_w_resid.rds")
write.csv(new_data,'./csvs/new_data_w_resid.csv', row.names=FALSE)


'log transform'
model = readRDS(file = "./models/lm_log.rds")
suppressWarnings(new_data$pred_val             <- exp(predict(model, new_data)))
cor(new_data$total_sales,new_data$pred_val   )^2

'exp transform'
model = readRDS(file = "./models/lm_exp.rds")
suppressWarnings(new_data$pred_val             <- log(predict(model, new_data)))
cor(new_data$total_sales,new_data$pred_val   )^2

'square transform'
model = readRDS(file = "./models/lm_pw2.rds")
suppressWarnings(new_data$pred_val             <- (predict(model, new_data)))
new_data$pred_val   [new_data$pred_val    < 0] <- 0
new_data$pred_val                              <- (new_data$pred_val   )^0.5
cor(new_data$total_sales,new_data$pred_val   )^2

'square root transform'
model = readRDS(file = "./models/lm_sqr.rds")
suppressWarnings(new_data$pred_val             <- (predict(model, new_data))^2)
cor(new_data$total_sales,new_data$pred_val   )^2

'reciprocal square root transform'
model = readRDS(file = "./models/lm_rsr.rds")
suppressWarnings(new_data$pred_val             <- (predict(model, new_data))^-2)
cor(new_data$total_sales,new_data$pred_val   )^2

'reciprocal transform'
model = readRDS(file = "./models/lm_rcp.rds")
suppressWarnings(new_data$pred_val             <- (predict(model, new_data))^-1)
cor(new_data$total_sales,new_data$pred_val   )^2

'reciprocal squared transform'
model = readRDS(file = "./models/lm_ng2.rds")
#This does not work because of negative values; could fix it, but it does not
#seem to be a very promising transform anyway
suppressWarnings(new_data$pred_val             <- (predict(model, new_data))^-0.5)
new_data$pred_val                              <- (new_data$pred_val   )^-0.5
cor(new_data$total_sales,new_data$pred_val   )^2


