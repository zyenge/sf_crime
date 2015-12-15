#import libraries
library(dplyr) #dplyr for manipulation and summarising data
library(lubridate) #for date manipulation/extraction
library(nnet) #multinomial logistic regression

#Read in training data
path_to_train_data <- 'train.csv' #edit this accordingly
data <- read.csv(path_to_train_data)

#Preprocessing, add some fields for hour/month/year from Dates field.
data$hour <- hour(x = data$Dates)
data$month <- month(x = data$Dates)
data$year <- year(x= data$Dates)


#Exploration of description and resolution values
#------------------------------------
data_category_description <- summarise(group_by(data,Category,Descript),
                                      count = length(Descript))
t_c_d<-table(data$Category,data$Descript)
t_c_r<-table(data$Category,data$Resolution)
#----------------------------------


#Build model on sample of data
#-------------------------------------------
#Sample 
num_rows_to_sample = 1000 #obviously, this is low
sampled_data <- sample_n(data,replace = FALSE,size =num_rows_to_sample)
fit <- multinom(data = sampled_data, formula = Category ~ X+Y + PdDistrict + hour + month + year + DayOfWeek)
#-------------------------------------------

#Validate
# https://www.kaggle.com/wiki/LogarithmicLoss - go here for sample usage
# --------------------------------------------
MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll * -1/(nrow(act))      
  return(ll);
}
# --------------------------------------------



