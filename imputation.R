library(dplyr)
library(data.table)
setwd("/Users/jasonchiu0803/Desktop/data_bootcamp/sberbank_project")
train <- fread("./train.csv",stringsAsFactors = TRUE)

# cleaning data
# train year
hist(train$build_year)
boxplot(train$build_year)
train %>% filter(build_year == 20052009)
train$year_cleaned <- train$build_year

# convert large number to smaller (2005, 2009)
train[which(train$year_cleaned==20052009),]$year_cleaned <- 2009
hist(train$year_cleaned)
summary(train$year_cleaned)

# convert year 4965 to 1965
train[train$year_cleaned == 4965, "year_cleaned"] <- 1965
hist(train$year_cleaned)

# converting all year between 0 - 1600 to NAs
train <- train %>% mutate(year_cleaned = replace(year_cleaned, year_cleaned<1600, NA))
summary(train$year_cleaned)
hist(train$year_cleaned)

# replacing all missing values with the sub_are average
# calculating average year that the apartments are built
#year_index_sub_area <- train %>% 
#  select(year_cleaned, sub_area) %>%
#  group_by(sub_area) %>%
#  summarise(avg_year = round(mean(year_cleaned, na.rm=TRUE)))

#replacing NAs with average from sub_area
#train1 <- left_join(train, year_index_sub_area,by = "sub_area")
#summary(train1$avg_year)
#na.index <- which(is.na(train$year_cleaned))
#train[na.index,"year_cleaned"] <- train1[na.index,"avg_year"]
#hist(train$year_cleaned)
#summary(train$year_cleaned)

#State
summary(train$state)
hist(train$state)
summary(train$state)

# replacing 33 to 3
train$state_cleaned <- train$state
index_33 <- which(train$state_cleaned == 33)
index_33
train[index_33,"state_cleaned"] <- 3
train[which(train$state_cleaned>4),"state_cleaned"] <- NA
hist(train$state_cleaned)

# kitch_sq
train[which(train$kitch_sq==1970),]$year_cleaned = 1970
train[which(train$kitch_sq==1970),]$kitch_sq = NA
train[which(train$kitch_sq==1974),]$year_cleaned = 1974
train[which(train$kitch_sq==1974),]$kitch_sq = NA
train[which(train$kitch_sq==2013),]$year_cleaned = 2013
train[which(train$kitch_sq==2013),]$kitch_sq = NA
train[which(train$kitch_sq==2014),]$year_cleaned = 2014
train[which(train$kitch_sq==2014),]$kitch_sq = NA
train[which(train$kitch_sq==620),]$kitch_sq = NA
summary(train$year_cleaned)
#train[which(train$kitch_sq > train$full_sq),]  <-  train[which(train$kitch_sq > train$full_sq),]$kitch_sq/10

#full_sq cleaning
train <- train %>% mutate(full_sq = replace(full_sq, full_sq == 0, NA))
train <- train %>% mutate(full_sq = replace(full_sq, full_sq == 1, NA))
train <- train %>% mutate(full_sq = replace(full_sq, full_sq == 5326, NA))
#train$full_sq[is.na(train$full_sq)] = mean(train$full_sq, na.rm=TRUE)
summary(train$year_cleaned)

# life sq
sum(train$life_sq > train$full_sq, na.rm=TRUE)
train <- train %>% mutate(life_sq = replace(life_sq, life_sq == 7478, NA))
train[which(train$life_sq > train$full_sq),]$life_sq  <-  train[which(train$life_sq > train$full_sq),]$life_sq/10
train[which(train$life_sq == 0),]$life_sq <- NA
train[which(train$life_sq == 1),]$life_sq <- NA
sum(is.na(train$life_sq))

# num of rooms 
train[which(train$num_room > 10),]$num_room <- NA
train[which(train$num_room == 0),]$num_room <- NA

# floor
train[which(train$floor==77),]$floor = NA

# material
summary(train$material)
train[which(train$material > 6),]$material <- NA
summary(train$material)

#showing missing values
library(DT)
library(tidyverse)
miss_pct <- map_dbl(train, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })

miss_pct <- miss_pct[miss_pct > 0]

data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) +
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

first_set <- train %>% dplyr::select(full_sq,
                              life_sq,
                              floor,
                              max_floor,
                              material,
                              year_cleaned,
                              num_room,
                              kitch_sq,
                              state_cleaned,
                              sub_area,
                              price_doc) %>%
  dplyr::mutate(log_price = log(price_doc),
         state_cleaned = factor(state_cleaned, levels = c(1,2,3,4), ordered = TRUE),
         material = factor(material,levels = c(1,2,3,4,5,6))) %>%
  dplyr::select(-price_doc)

#linear regression
#install.packages("VIF")
#library(VIF)
first_set_model <- na.omit(first_set)
dim(first_set_model)
model.empty = lm(log_price ~ 1, data = first_set_model)
model.full = lm(log_price ~ ., data = first_set_model)
scope = list(lower = formula(model.empty), upper = formula(model.full))
library(MASS) #The Modern Applied Statistics library.

# variable 
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
summary(forwardAIC)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
summary(backwardAIC)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
summary(bothAIC.empty)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)
summary(bothAIC.full)

AIC(forwardAIC,
    backwardAIC,
    bothAIC.empty,
    bothAIC.full)

#Lasso
#Need matrices for glmnet() function. Automatically conducts conversions as well
#for factor variables into dummy variables.
x = model.matrix(log_price ~ ., first_set_model)[, -1] #Dropping the intercept column.
dim(x)
y = first_set_model$log_price

grid = 10^seq(3, -5, length = 100)

#80 and 20 % train and test
set.seed(0)
train = sample(1:nrow(x), 8*nrow(x)/10)
test = (-train)
y.test = y[test]

length(train)/nrow(x)
length(y.test)/nrow(x)

library(caret)
dim(x)
#dim(na.omit(x))
#plyr::count(is.na(x))
#sum(is.na(x))
set.seed(0)
train_control = trainControl(method = 'cv', number=10)
#expand.grid
tune.grid = expand.grid(lambda = grid, alpha=c(1))
lasso.caret = train(x[train, ], y[train],
                    method = 'glmnet',
                    trControl = train_control, tuneGrid = tune.grid)
plot(lasso.caret, xTrans=log)
log(lasso.caret$bestTune)
# -6.30


set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)
# -6.12

lasso.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = bestlambda.lasso)
lasso.models.train$beta
predict(lasso.models.train,)
predict(lasso.models.train,  

lasso.models.caret = glmnet(x[train, ], y[train], alpha = 1, lambda = lasso.caret$bestTune)
lasso.models.caret$beta

# random Forest
library(randomForest)
total <- data.frame(x,y)
summary(total)
log_random <- randomForest(y ~ . ,data = total, importance = TRUE)
summary(log_random)
importance(log_random)
varImpPlot(log_random)
sum((total$y - log_random$predicted)^2)


set.seed(0)
rf.boston = randomForest(medv ~ ., data = Boston, subset = train, importance = TRUE)
rf.boston


































complete_data <- train_mummy_miss$ximp %>% select(-id,
                                                  -ID_metro,
                                                  -ID_railroad_station_walk,
                                                  -ID_railroad_station_avto,
                                                  -ID_big_road1,
                                                  -ID_big_road2,
                                                  -ID_railroad_terminal,
                                                  -ID_bus_terminal)
x = model.matrix(price_doc~., complete_data,na.action=NULL)[, -1]
dim(x)

#creating dependent variable
y = complete_data$price_doc
length(y)

# lasso regression: Long story short, if you let glmnet standardize the 
# coefficients (by relying on the default standardize = TRUE), 
# glmnet performs standardization behind the scenes and reports everything,
# including the plots, the "de-standardized" way, in the coefficients' 
# natural metrics.
# https://stats.stackexchange.com/questions/126109/coefficient-value-from-glmnet
library(glmnet)

#cross validation
train = sample(1:nrow(x), 8*nrow(x)/10)
test = (-train)
y.test = y[test]
length(train)/nrow(x)
length(y.test)/nrow(x)

set.seed(0)
grid = 10^seq(10, -10, length = 100)
cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

# train a lasso based on the bestlambda.lasso
lasso.models = glmnet(x, y, alpha = 1, lambda = bestlambda.lasso)
a <- coef(lasso.models)
a



