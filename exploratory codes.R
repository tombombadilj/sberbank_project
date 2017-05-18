setwd("/Users/jasonchiu0803/Desktop/data_bootcamp/Porject 3")
library(data.table)
library(dplyr)
library(Hmisc)
#install.packages("Hmisc")
data <- fread("train.csv",stringsAsFactors = TRUE)
dim(data)
View(data)
str(data)
summary(data)

c_d <- function(x){
  if (class(x) == "integer" | class(x) == "numeric"){
    b = sum(is.na(x))
    a = summary(x)
    print (a)
    b.str = as.character(b)
    print (cat("NAs:",b))
    c = cor(x, data$price_doc,use = "pairwise.complete.obs")
    print(c)
    boxplot(x)
  }
}

# building time
c_d(data$raion_build_count_with_builddate_info)
# close to zero correlation(0.06), 4991 MS
c_d(data$`build_count_before_1920`)
# 0.1, 4991
c_d(data$`build_count_1921-1945`)
# 0.02, 4991
c_d(data$`build_count_1946-1970`)
# 0.06, 4991
c_d(data$`build_count_1971-1995`)
# - 0.01, 4991
c_d(data$`build_count_after_1995`)
# 0.02, 4991
data$total <- data$`build_count_before_1920` + data$`build_count_1921-1945`+
  data$`build_count_1946-1970` + data$`build_count_1971-1995` + data$`build_count_after_1995`
data %>% mutate(diff = total -raion_build_count_with_builddate_info) %>% 
  select(diff) %>% summarise(mean(diff, na.rm=TRUE))
building_count <- data %>% select(`build_count_before_1920`, 
                `build_count_1921-1945`,
                `build_count_1946-1970`,
                `build_count_1971-1995`,
                `build_count_after_1995`)
c = cor(building_count,use = "pairwise.complete.obs")
#install.packages("corrplot")
#library(corrplot)
corrplot(c, method="number")
corrplot(c, method="circle")
#largest correlation between (1921 - 1945 and 1920) 0.57, 0.69 (1995 and 1921-1945),
# 0.45 ( 1946 - 1970, 1921-1945)
# raion building info count is just sum of the other variables. There are 
# about max 845 between 1971-1995 and 799 after 1995 in certain neighborhood. 
# Not sure if this is possible

c_d(data$cafe_count_500)
# maximum is 120 cafe arourd a place, 0.12 correlation min 0
data %>% filter(cafe_count_500 ==120)
# both of them are in Tverskoe, and are for investment

c_d(data$cafe_count_1000)
# maximum is 449, and corelation (0.11) mean = 15 min = 0
data %>% filter(cafe_count_1000 ==449) %>% select(sub_area,product_type)
# the highest number is in Meshhanskoe and Owner Occupier

c_d(data$cafe_count_1500)
# maximum is 784, 0.11, mean = 32.5 min = 0
data %>% filter(cafe_count_1500 ==784) %>% select(sub_area,product_type)
# Tverskoe Investment

c_d(data$cafe_count_2000)
# maximum is 1115, 0.12, mean = 55 min =0 
data %>% filter(cafe_count_2000 ==1115) %>% select(sub_area,product_type)
# Tverskoe Investment

c_d(data$cafe_count_3000)
# maximum is 1815, 0.12, mean = 111 min = 0 
data %>% filter(cafe_count_3000 ==1815) %>% select(sub_area,product_type) %>%
  group_by(sub_area, product_type) %>% summarise(n())
# Tverskoe Investment 603 sellings, mostly owneroccupier

c_d(data$cafe_count_5000)
# maximum is 2645, mean = 266 min = 0
data %>% filter(cafe_count_5000 ==2645) %>% select(sub_area,product_type) %>%
  group_by(sub_area,product_type) %>% summarise(n())
# Tverskoe investiment (1)

data %>% mutate(gr1 = ifelse(cafe_count_1000>=cafe_count_500,1,0),
                gr2 = ifelse(cafe_count_1500>=cafe_count_1000,1,0),
                gr3 = ifelse(cafe_count_2000>=cafe_count_1500,1,0),
                gr4 = ifelse(cafe_count_3000>=cafe_count_2000,1,0),
                gr5 = ifelse(cafe_count_5000>=cafe_count_3000,1,0)) %>%
  summarise(avg_1 = mean(gr1, na.rm=TRUE),
            avg_2 = mean(gr2, na.rm=TRUE),
            avg_3 = mean(gr3, na.rm=TRUE),
            avg_4 = mean(gr4, na.rm=TRUE),
            avg_5 = mean(gr5, na.rm=TRUE))

data %>% select(sub_area, cafe_count_500) %>%head(50)
# all of them becomes greater as it increases, so no mistakes

cafe_count <- data %>% select(cafe_count_500,
                cafe_count_1000,
                cafe_count_1500,
                cafe_count_2000,
                cafe_count_3000,
                cafe_count_5000)
c = cor(cafe_count,use = "pairwise.complete.obs")
#install.packages("corrplot")
#library(corrplot)
corrplot(c, method="number")
# All of them are highly correlated. (above 0.8)

c_d(data$cafe_avg_price_500)
# missing data 13281, mean = 994, 0.04
c_d(data$cafe_sum_500_min_price_avg)
# missing data 13281

cafe_500 <- data %>% filter(is.na(cafe_avg_price_500)) %>% 
  select(cafe_sum_500_min_price_avg,
         cafe_sum_500_max_price_avg,
         cafe_count_500_na_price,
         cafe_count_500_price_500,
         cafe_count_500_price_1000,
         cafe_count_500_price_1500,
         cafe_count_500_price_2500,
         cafe_count_500_price_4000,
         cafe_count_500_price_high)

# when avg price is missing then cafe_sum_500_min_price_avg,
# cafe_sum_500_max_price_avg will both be missing
# if mean price is missing, all of the below will be 0 or 1
# cafe_count_500_na_price, cafe_count_500_price_500, cafe_count_500_price_1000,
# cafe_count_500_price_1500, cafe_count_500_price_2500, cafe_count_500_price_4000,
# cafe_count_500_price_high

# 
c_d(data$cafe_count_500_price_500)


c_d(data$`build_count_1921-1945`)

checking_data(x)
names(x)



names(which(colSums(is.na(data))>0))
#1-8 Chris
#9-21 - Fouad
#22-32 - Jason
#33-51 - John

#finding NAs
findingms <- function(x){
  sum(is.na(x))
}

dim(data)
a <- apply(data, MARGIN = 2, findingms)
length(a[a>0])
#how many NAs are in each column with NAs
ms_names <- names(which(colSums(is.na(data))>0))
ms
dim(data)

data %>% group_by(ecology) %>% summarise(count=n())
head(data$timestamp)
data$timestamp <- as.Date(data$timestamp,format = "%Y-%m-%d")
summary(data)
dim(data)

table(data$sub_area)
sum(is.na(data$sub_area))
sum(is.na(data$price_doc))

#[22-32]
#missing data exploration
ms_names[22:32]
# raion_build_count_with_builddate_info
summary(data$raion_build_count_with_builddate_info)
# a total of 4991 missing data
# This variable represents the
# Number of building with build year info in district
# there is no point of imputing this variable. 

plot(data$raion_build_count_with_builddate_info, data$price_doc)

summary(data$build_count_before_1920)
summary(data$`build_count_1921-1945`)
summary(data$`build_count_1946-1970`)
summary(data$`build_count_1971-1995`)
summary(data$build_count_after_1995)

# no missing data in sub_area
city_list <- data %>% group_by(sub_area) %>% summarise(total_count =n())
city_list

ms = data[is.na(data$build_count_before_1920),]
#4991 missing
summary(ms$raion_build_count_with_builddate_info)
#4991 missing
summary(ms$`build_count_1921-1945`)
# 4991 missing
summary(ms$`build_count_1946-1970`)
# 4991 missing
summary(ms$`build_count_1971-1995`)
#4991 missing
summary(ms$build_count_after_1995)
#4991 missing
sub_area_list <- ms %>% group_by(sub_area) %>% summarise(count_1920=n())
sub_area_list
total <- left_join(sub_area_list, city_list, by = c("sub_area"))
total %>% mutate(diff = total_count - count_1920)
# 15 sub_area/raions are completely missing information regarding when
# the building is built
# there is nothing to input these values on...

# Metro
summary(data$metro_min_walk)
# 25 missing values
summary(data$metro_km_walk)
# 25 missing values
msmin <- data[is.na(data$metro_min_walk),]
summary(msmin$metro_km_walk)
# same 25 for km and mins
summary(msmin$metro_km_avto)
summary(msmin$metro_min_avto)
summary(msmin$railroad_station_walk_km)
# same 25 for railroad station km
summary(msmin$railroad_station_walk_min)
# same 25 for railroad station min
summary(msmin$railroad_station_avto_km)
summary(msmin$railroad_station_avto_min)
# no missing data for these
summary(msmin$ID_railroad_station_avto)
# no missing data for these
# we do have the distance between metro station with car ride in km 
# and min
# taking a look at the relationship between cars and walk
data %>% 
  select(metro_min_walk,
         metro_km_walk,
         metro_min_avto,
         metro_km_avto,
         sub_area) %>%
  mutate(diff_km = metro_km_walk - metro_km_avto,
         diff_min = metro_min_walk - metro_min_avto) %>%
  group_by(sub_area) %>%
  summarise(avg_km = mean(diff_km, na.rm = TRUE),
            avg_min = mean(diff_min, na.rm = TRUE))
# average distance diff is only -0.14, we might be able to imput 
# based on the car distance, the adjustment varies quite a bit by 
# sub_area

data %>% 
  select(metro_min_walk,
         metro_km_walk,
         metro_min_avto,
         metro_km_avto,
         sub_area) %>%
  mutate(avg_speed = metro_km_walk/metro_min_walk) %>%
  summarise(avg_avg_speed = mean(avg_speed,na.rm=TRUE))

# they use a uniform avg_speed of 0.08333 km/min for walking
# if we use car distance to impute missing walking distance
# we can use the uniform speed to impute missing mins

# railroad stations
data %>% 
  select(railroad_station_avto_km,
         railroad_station_avto_min,
         ID_railroad_station_avto,
         railroad_station_walk_km,
         railroad_station_walk_min,
         ID_railroad_station_walk) %>%
  mutate(diff_km = railroad_station_avto_km - railroad_station_walk_km,
         diff_min = railroad_station_avto_min - railroad_station_walk_min,
         speed = railroad_station_walk_km/railroad_station_walk_min)%>%
  summarise(avg_diff_km = mean(diff_km, na.rm=TRUE),
            avg_diff_min = mean(diff_min, na.rm=TRUE),
            avg_walk_speed = mean(speed, na.rm=TRUE))

# the speed is still 0.0833, the average difference between walking and car
# is around 0.2 (not a large number), we might be able to impute based
# on these values.

data %>% 
  select(railroad_station_avto_km,
         railroad_station_avto_min,
         ID_railroad_station_avto,
         railroad_station_walk_km,
         railroad_station_walk_min,
         ID_railroad_station_walk,
         sub_area) %>%
  mutate(samestation = ID_railroad_station_avto-ID_railroad_station_walk) %>%
  mutate(same = ifelse(samestation == 0, 1, 0)) %>%
  group_by(sub_area) %>%
  summarise(num = sum(same, na.rm=TRUE)/n()*100) %>%
  summarise(meanp = mean(num))
# about 78% of subarea listings have the same railroad station, but giving 
# that we only have 25 missing data, we can use the car information for station,
# and distance to imput the rest and divide that by the speed.

# Lasso_initial
data_lasso <- data %>% select(-timestamp, -id)
character_only <- sapply(data_lasso, is.factor)
data_lasso <- data.frame(data_lasso)
sum(character_only)
dim(data_lasso)
# 15 total character variables + id+ timestamp

#convert to matrix for lasso regression
library(dplyr)
names(data)
complete_data <- data[complete.cases(data)]
names(select(complete_data, starts_with("ID")))
complete_data <- complete_data %>% select(-id, 
                                          -timestamp,
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
grid = 10^seq(5, -2, length = 100)
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

