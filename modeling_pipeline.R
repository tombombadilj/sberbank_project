# read in the data 
library(dplyr)
library(data.table)
library(DT)
library(tidyverse)
setwd("/Users/jasonchiu0803/Desktop/data_bootcamp/sberbank_project")
train <- fread("./train.csv",stringsAsFactors = TRUE)
test <- fread("./test.csv",stringsAsFactors = TRUE)
macro <- fread("./macro.csv",stringsAsFactors = TRUE)

summary(test$floor)

test$price_doc <- NA
test$source <- "test"
train$source <- "train"

total <- rbind(train,test)

miss_pct <- map_dbl(total, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })

miss_pct <- miss_pct[miss_pct > 0]

data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) +
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#clean up for build_year and state
summary(total$build_year)
total[which(total$build_year == 20052009),]$build_year <- 2009
total[which(total$build_year == 4965),]$build_year <- 1965
total <- total %>% mutate(build_year = replace(build_year, build_year<1600, NA))
summary(total$build_year)
# converting all year between 0 - 1600 to NAs
summary(train$year_cleaned)
hist(train$year_cleaned)
# replacing all missing values with the sub_are average
# calculating average year that the apartments are built
year_index_sub_area <- total %>% 
  dplyr::select(build_year, sub_area) %>%
  dplyr::group_by(sub_area) %>%
  dplyr::summarise(avg_year = round(mean(build_year, na.rm=TRUE)))

#replacing NAs with average from sub_area
total1 <- left_join(total, year_index_sub_area,by = "sub_area")
na.index <- which(is.na(total$build_year))
total[na.index,"build_year"] <- total1[na.index,"avg_year"]
hist(total$build_year)
summary(total$build_year)

#state
#install.packages("pracma")
library(pracma)
summary(total$state)
total[which(total$state == 33),]$state <- 3
state_index_sub_area <- 
  data.frame(aggregate(total$state, by=list(total$sub_area), FUN=Mode))
state_index_sub_area <- 
  state_index_sub_area %>% rename(sub_area = Group.1)
total2 <- left_join(total, state_index_sub_area, by = "sub_area")
na.index <- which(is.na(total$state))
total[na.index,"state"] <- total2[na.index,"x"]
summary(total$state)
hist(total$state)

miss_pct <- map_dbl(total, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })

miss_pct <- miss_pct[miss_pct > 0]

data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) +
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#converting variables to factor
total$state <- factor(total$state, levels = c(1,2,3,4))
total$material <- factor(total$material)
summary(total$state)

train_1 <- total%>% dplyr::filter(source=="train") %>%
  dplyr::select(price_doc,
                full_sq,
                build_year,
                state,
                kitch_sq,
                life_sq,
                material,
                floor,
                num_room,
                sub_area,
                product_type)

train_1$log_price <- log(train$price_doc)

test_1 <- total%>% dplyr::filter(source=="test") %>%
  dplyr::select(full_sq,
                build_year,
                state,
                kitch_sq,
                life_sq,
                material,
                floor,
                num_room,
                sub_area,
                product_type)

model.empty = lm(log_price ~ 1, data = train_1)
model.full = lm(log_price ~ ., data = train_1)
scope = list(lower = formula(model.empty), upper = formula(model.full))
library(MASS) #The Modern Applied Statistics library.



# forward AIC
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
summary(forwardAIC)
test_predict <- predict(forwardAIC, test_1)
test_origin <- exp(test_predict)
total %>% filter(source=="test") %>% dplyr::select(id)

test_2 <- total%>% dplyr::filter(source=="test") %>%
  dplyr::select(id)



# Lasso
x = model.matrix(log_price ~ ., train_1)[, -1] #Dropping the intercept column.
dim(x)
y = train_1$log_price

grid = 10^seq(3, -5, length = 100)

#80 and 20 % train and test
set.seed(0)
train_index = sample(1:nrow(x), 8*nrow(x)/10)
test_index = (-train_index)
y.test = y[test_index]

length(train_index)/nrow(x)
length(y.test)/nrow(x)

set.seed(0)
cv.lasso.out = cv.glmnet(x[train_index, ], y[train_index],
                         lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

lasso.models.train = glmnet(x[train_1, ], y[train_1], alpha = 1, lambda = bestlambda.lasso)
lasso.models.train$beta[lasso.models.train$beta>0]
laso_predict_cv <- predict(lasso.models.train, x)



# random Forest
library(randomForest)
log_random <- randomForest(log_price ~ . ,data = train_1, importance = TRUE)
log_random
summary(log_random)
importance(log_random)
varImpPlot(log_random)


# Creating a new submission file
submission <- data.frame(id=test_2$id, price_doc = test_origin)

View(submission)

write.csv(x = submission,"./submission.csv",row.names = FALSE)

dim(submission)
