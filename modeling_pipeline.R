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

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

total$build_year <- as.numeric(total$build_year)
total[which(is.na(total$build_year)),]$build_year <- round(mean(total$build_year,na.rm=TRUE))
total[which(is.na(total$state)),]$state <- median(total$state,na.rm=TRUE)
total[which(is.na(total$kitch_sq)),]$kitch_sq <- mean(total$kitch_sq,na.rm=TRUE)
total[which(is.na(total$floor)),]$floor <-median(total$floor,na.rm=TRUE)  
total$max_floor<-as.factor(total$max_floor)
total[which(is.na(total$max_floor)),]$max_floor <- getmode(total$max_floor)
total$material<-as.factor(total$material)
total[which(is.na(total$material)),]$material <-getmode(total$material)
total$num_room<-as.factor(total$num_room)
total[which(is.na(total$num_room)),]$num_room <-getmode(total$num_room)
total[which(is.na(total$full_sq)),]$full_sq <-mean(total$full_sq,na.rm=TRUE)
total[which(is.na(total$life_sq)),]$life_sq <- mean(total$life_sq, na.rm=TRUE)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

summary(total$price_doc)

total <- total %>% dplyr::mutate()

train_1 <- total%>% dplyr::filter(source=="train") %>%
  dplyr::select(full_sq,build_year,state,kitch_sq,life_sq,material,floor,num_room,sub_area)

test_1 <- total%>% dplyr::filter(source=="test") %>%
  dplyr::select(full_sq,build_year,state,kitch_sq,life_sq,material,floor,num_room,sub_area)
class(test_1$num_room)
train_1$log_price <- log(train$price_doc)

model.empty = lm(log_price ~ 1, data = train_1)
model.full = lm(log_price ~ ., data = train_1)
scope = list(lower = formula(model.empty), upper = formula(model.full))
library(MASS) #The Modern Applied Statistics library.

# variable 
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
summary(forwardAIC)
test_predict <- predict(forwardAIC, test_1)
test_origin <- exp(test_predict)
total %>% filter(source=="test") %>% dplyr::select(id)

test_2 <- total%>% dplyr::filter(source=="test") %>%
  dplyr::select(id)

submission <- data.frame(id=test_2$id, price_doc = test_origin)

View(submission)

write.csv(x = submission,"./submission.csv",row.names = FALSE)

dim(submission)
