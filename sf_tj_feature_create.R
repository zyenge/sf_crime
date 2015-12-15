setwd('C:\\Users\\tjspross\\Documents\\kaggle\\')
train = read.csv('train.csv')
library(plyr)
library(lubridate)
train$yearweek = (year(train$Dates)*100 + isoweek(train$Dates)) %>% as.factor()
train$tofday = hour(train$Dates)
train$month = month(train$Dates)
train$
View(train)
View(plyr::count(train,vars=c('Category','Descript')))
train$car_related = train$Descript %in% c('GRAND THEFT FROM LOCKED AUTO','PETTY THEFT FROM LOCKED AUTO','MALICIOUS MISCHIEF, VANDALISM OF VEHICLES',
                                          'STOLEN AUTOMOBILES','STOLEN TRUCK','VEHICLE, RECOVERED, AUTO')
