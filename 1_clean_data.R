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

#after pulling new file, will need to use vim or something to replace /N with ""
file_location                <- "./csvs/scaled_sales.csv"
so_data                      <- read_csv(file_location, na = "")
#now we clean up each column a bit
'total_sales'
so_data$total_sales          <- as.numeric(so_data$total_sales)
paste('nbr of na values',sum(is.na(so_data$total_sales)))
paste('min',min(so_data$total_sales),'max',max(so_data$total_sales))

'salesdate'
so_data$salesdate            <- as.Date(so_data$salesdate)
paste('min',min(so_data$salesdate),'max',max(so_data$salesdate))

'weekday'
so_data$weekday              <- weekdays(as.Date(so_data$salesdate))
so_data$weekday              <- factor(so_data$weekday,
                                   levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                                   labels=c("1Mon","2Tue","3Wed","4Thu","5Fri","6Sat","7Sun"))
levels(so_data$weekday)

'endofmonth'
so_data$day                  <- as.Date(so_data$salesdate, format = "%Y-%m-%d")
so_data$lastdayofmonth       <- ceiling_date(so_data$day, "month") - 1
so_data$daysleftinmonth      <- as.numeric(so_data$lastdayofmonth - so_data$day)
so_data$endofmonth           <- as.numeric((so_data$daysleftinmonth < 1))
paste('nbr na values',sum(is.na(so_data$endofmonth)),'nbr nonzero values',sum(so_data$endofmonth))

'day'
so_data$day                  <- as.numeric(format(as.Date(so_data$salesdate, format = "%Y-%m-%d"), "%d"))
paste('nbr na values',sum(is.na(so_data$day)),'min',min(so_data$day),'max',max(so_data$day))

'week'
so_data$week                 <- '1first'
so_data$week[so_data$day>7]  <- '2second'
so_data$week[so_data$day>14] <- '3third'
so_data$week[so_data$day>21] <- '4fourth'
so_data$week[so_data$day>28] <- '5fifth'
so_data$week                 <- as.factor(so_data$week)
contrasts(so_data$week)      <- contr.treatment(5,base=2)
levels(so_data$week)

'month'
so_data$month                <- as.factor(format(as.Date(so_data$salesdate), "%m"))
contrasts(so_data$month)     <- contr.treatment(12)
levels(so_data$month)

'day_tot'
so_data$day_tot              <- as.numeric(difftime(as.Date(as.character(so_data$salesdate), format="%Y-%m-%d"),as.Date("2006-12-31", format="%Y-%m-%d"),units=c('days')))
paste('nbr na values',sum(is.na(so_data$day_tot)),'min',min(so_data$day_tot),'max',max(so_data$day_tot))

so_data$notes[is.na(so_data$notes)] <- ""

'loc'
so_data$loc                                                            <- 'third'
so_data$loc[so_data$salesdate<as.Date("2018-01-31",format="%Y-%m-%d")] <- "second"
so_data$loc[so_data$salesdate<as.Date("2012-01-31",format="%Y-%m-%d")] <- "first"
so_data$loc                                                            <- as.factor(so_data$loc)
levels(so_data$loc)
contrasts(so_data$loc)                                                 <- contr.treatment(3)


'holiday'
so_data$holiday                                                                                      <- 'none'
so_data$holiday[so_data$month=='01' & so_data$day==1]                                                <- 'NewYearsDay'
so_data$holiday[grepl('aster', so_data$notes)]                                                       <- 'Easter'
so_data$holiday[so_data$month=='05' & so_data$weekday=='1Mon' & so_data$day>24]                      <- 'MemorialDay'
so_data$holiday[so_data$month=='07' & so_data$day==4]                                                <- 'July4th'
so_data$holiday[so_data$month=='09' & so_data$weekday=='1Mon' & so_data$week=='1first']              <- 'LaborDay'
so_data$holiday[so_data$month=='10' & so_data$day==31]                                               <- 'Halloween'
so_data$holiday[so_data$month=='11' & so_data$day==1]                                                <- 'DayOfTheDead'
so_data$holiday[so_data$month=='11' & so_data$weekday=='4Thu' & so_data$week=='4fourth']             <- 'Thanksgiving'
so_data$holiday[so_data$month=='11' & so_data$weekday=='5Fri' & so_data$day>22 & so_data$day<30]     <- 'BlackFriday'
so_data$holiday[so_data$month=='12' & so_data$day==24]                                               <- 'ChristmasEve'
so_data$holiday[so_data$month=='12' & so_data$day==25]                                               <- 'Christmas'
so_data$holiday[so_data$month=='12' & so_data$day==26]                                               <- 'BoxingDay'
so_data$holiday[so_data$month=='12' & so_data$day==31]                                               <- 'NewYearsEve'
so_data$holiday                                                                                      <- as.factor(so_data$holiday)
'levels(so_data$holiday)'
levels(so_data$holiday)

'tickets'
so_data$tickets                                          <- 0
so_data$tickets[grepl('ticket', so_data$notes)]          <- 1
so_data$tickets[grepl('tix', so_data$notes)]             <- 1
paste('nbr True',sum(so_data$tickets))

'ticket_accounting'
so_data$ticket_accounting                                <- 0
so_data$ticket_accounting[so_data$salesdate>as.Date("2018-02-28",format="%Y-%m-%d")] <- 1
paste('nbr w new system',sum(so_data$ticket_accounting))

'anniversary'
so_data$anniversary                                      <- 0
so_data$anniversary[grepl('anniversary', so_data$notes)] <- 1
paste('nbr occurrences',sum(so_data$anniversary))

'corset'
so_data$corset                                           <- 0
so_data$corset[grepl('corset', so_data$notes)]           <- 1
so_data$corset[grepl('trunk',so_data$notes)]             <- 1
paste('nbr occurrences',sum(so_data$corset))

'goth_ball'
so_data$goth_ball                                        <- 0
so_data$goth_ball[grepl('goth ball',so_data$notes)]      <- 1
so_data$goth_ball[grepl('gothball',so_data$notes)]       <- 1
so_data$goth_ball[grepl('gothic ball',so_data$notes)]    <- 1
paste('nbr occurrences',sum(so_data$goth_ball))

'sale'
so_data$sale                                             <- 0
so_data$sale[grepl('sale event', so_data$notes)]         <- 1
paste('nbr occurrences',sum(so_data$sale))

'tax_free_weekend'
so_data$tfw                                              <- 0
so_data$tfw[grepl('tax free weekend', so_data$notes)]    <- 1
so_data$tfw[grepl('tax-free weekend', so_data$notes)]    <- 1
so_data$tfw[grepl('taxfree weekend', so_data$notes)]     <- 1
paste('nbr occurrences',sum(so_data$tfw))

'event'
so_data$event                                            <- 0
so_data$event[grepl('event', so_data$notes)]             <- 1
so_data$event[so_data$corset==1]                         <- 0
so_data$event[so_data$anniversary==1]                    <- 0
so_data$event[so_data$goth_ball==1]                      <- 0
so_data$event[so_data$sale==1]                           <- 0
so_data$event[so_data$tfw==1]                            <- 0
paste('nbr occurrences',sum(so_data$event))

nrow(so_data)
so_data <- subset(so_data, select = c('total_sales', 'salesdate', 'weekday', 'endofmonth', 'day', 'week', 'month', 'day_tot', 'loc', 'holiday', 'tickets', 'ticket_accounting', 'anniversary', 'corset', 'goth_ball', 'sale', 'tfw', 'event'))
so_data$holiday <- as.factor(so_data$holiday)
'after throwing out columns we do not use'
nrow(so_data)
'NROW total sales'
NROW(so_data$total_sales)

#after last subset we drop all unused factor levels
so_data <- drop.levels(so_data) 
'after dropping levels we do not use'
nrow(so_data)
holidays_found                                                                                       <- levels(so_data$holiday)
'holidays found'
holidays_found
int_for_none                                                                                         <- grep("none",holidays_found)
contrasts(so_data$holiday)                                                                           <- contr.treatment(holidays_found,base=int_for_none)

'PLOTS OF RAW DATA'
'day of the week'
png('./plots/raw_dotw.png', width = 1000, height = 600)
ggplot(data = so_data) +
  labs(x = 'day of the week',y = 'in-store sales') +
  stat_summary(
    mapping = aes(x = weekday, y = total_sales),
    fun.ymin = function(x) quantile(x, .10),
    fun.ymax = function(x) quantile(x, .90),
    fun.y = median
  )
garbage <- dev.off()

'week of the month'
png('./plots/raw_by_week.png', width = 1000, height = 600)
ggplot(data = so_data) +
  labs(x = 'week of the month',y = 'in-store sales') +
  geom_boxplot(mapping = aes(x=week,y=total_sales, color=weekday))
garbage <- dev.off()

'dotw and week'
png('./plots/raw_by_dotw_and_week.png', width = 1000, height = 600)
ggplot(data = so_data) +
  labs(x = 'day of the week',y = 'in-store sales',colour = "week of the month") +
  geom_boxplot(mapping = aes(x=weekday,y=total_sales,color=week))
garbage <- dev.off()

'week and location'
png('./plots/raw_by_week_and_loc.png', width = 1000, height = 600)
ggplot(data = so_data) +
  labs(x = 'week of the month',y = 'in-store sales',colour = "location") +
  geom_boxplot(mapping = aes(x=week, y=total_sales, color=loc)) +
  facet_wrap(~ month, nrow=2)
garbage <- dev.off()



write.csv(so_data,'./csvs/post_clean.csv', row.names=FALSE)
saveRDS(so_data, file = "./dataframes/post_clean.rds")


