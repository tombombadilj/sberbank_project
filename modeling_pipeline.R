# read in the data 
library(dplyr)
library(data.table)
library(DT)
library(tidyverse)
setwd("/Users/jasonchiu0803/Desktop/data_bootcamp/sberbank_project")
train <- fread("./train.csv",stringsAsFactors = TRUE)
test <- fread("./test.csv",stringsAsFactors = TRUE)
macro <- fread("./macro.csv",stringsAsFactors = TRUE)
convert <- fread("./name_list.csv", stringsAsFactors = TRUE)
dim(test)

summary(test$floor)

test$price_doc <- NA
test$source <- "test"
train$source <- "train"

total <- rbind(train,test)

#miss_pct <- map_dbl(total, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })

#miss_pct <- miss_pct[miss_pct > 0]

#data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) +
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#unique(total$sub_area)

# verifying that total number of buildings adds up all of them
total %>% 
  dplyr::select(build_count_before_1920,
                sub_area,
                `build_count_1921-1945`,
                `build_count_1946-1970`,
                `build_count_1971-1995`,
                `build_count_after_1995`,
                raion_build_count_with_builddate_info) %>% 
  dplyr::mutate(totalbuilding = build_count_before_1920+
                  `build_count_1921-1945`+`build_count_1946-1970`+
                  `build_count_1971-1995`+`build_count_after_1995`) %>%
  dplyr::filter(sub_area =="Nagatinskij Zaton") %>%
    dplyr::select(raion_build_count_with_builddate_info, totalbuilding)

names(convert)
# build_year information missing all listings in a single raion
total_region <- left_join(total,convert, by="sub_area")

total_region[is.na(total_region$raion_build_count_with_builddate_info),] %>%
  group_by(sub_area) %>% summarise(count=n())

convert %>% dplyr::filter(sub_area == "Poselenie Desjonovskoe" |
                   sub_area == "Poselenie Filimonkovskoe" |
                   sub_area == "Poselenie Kievskij" |
                   sub_area == "Poselenie Klenovskoe" |
                   sub_area == "Poselenie Krasnopahorskoe" |
                   sub_area == "Poselenie Mihajlovo-Jarcevskoe" |
                   sub_area == "Poselenie Mosrentgen" |
                   sub_area == "Poselenie Novofedorovskoe" |
                   sub_area == "Poselenie Rjazanovskoe" |
                   sub_area == "Poselenie Rogovskoe" |
                   sub_area == "Poselenie Shhapovskoe" |
                   sub_area == "Poselenie Sosenskoe" |
                   sub_area == "Poselenie Vnukovskoe" |
                   sub_area == "Poselenie Voronovskoe" |
                   sub_area == "Poselenie Voskresenskoe")
convert[convert$sub_area]

total_sub_area_bild_count <- total_region %>% 
  dplyr::select(sub_area, OKRUG,
                raion_build_count_with_builddate_info, 
                build_count_before_1920,
                `build_count_1921-1945`,
                `build_count_1946-1970`,
                `build_count_1971-1995`,
                build_count_after_1995) %>%
  dplyr::group_by(sub_area) %>% 
  dplyr::filter(row_number()==1) %>%
  dplyr::group_by(OKRUG) %>%
  dplyr::summarise(total_build = sum(raion_build_count_with_builddate_info),
                   total_before_1920 = sum(build_count_before_1920),
                   total_1921_1945 = sum(`build_count_1921-1945`),
                   total_1946_1970 = sum(`build_count_1946-1970`),
                   total_1971_1995 = sum(`build_count_1971-1995`),
                   total_after_1995 = sum(build_count_after_1995)) %>%
  dplyr::mutate(ratio_before_1920 = total_before_1920/total_build,
                ratio_1921_1945 = total_1921_1945/total_build,
                ratio_1946_1970 = total_1946_1970/total_build,
                ratio_1971_1995 = total_1971_1995/total_build,
                ratio_after_1995 = total_after_1995/total_build)
View(total_sub_area_bild_count)
names(total_region)

# all of them are missing in the entire region...so cannot input based on that
# impute based on a combination of western and south-western regions closes 
# to the missing areas
#before 1920
total_sub_area_bild_count[6,]$ratio_before_1920 = (6052*0.014 + 4408*0.0082)/(6052+4408)
total_sub_area_bild_count[10,]$ratio_before_1920 = (6052*0.014 + 4408*0.0082)/(6052+4408)

#1921 - 1945
total_sub_area_bild_count[6,]$ratio_1921_1945 = (6052*0.122 + 4408*0.069)/(6052+4408)
total_sub_area_bild_count[10,]$ratio_1921_1945 = (6052*0.122 + 4408*0.069)/(6052+4408)

#1946 - 1970
total_sub_area_bild_count[6,]$ratio_1946_1970 = (6052*0.498 + 4408*0.45)/(6052+4408)
total_sub_area_bild_count[10,]$ratio_1946_1970 = (6052*0.498 + 4408*0.45)/(6052+4408)

#1971 - 1995
total_sub_area_bild_count[6,]$ratio_1971_1995 = (6052*0.191 + 4408*0.21)/(6052+4408)
total_sub_area_bild_count[10,]$ratio_1971_1995 = (6052*0.191 + 4408*0.21)/(6052+4408)

#after 1995
total_sub_area_bild_count[6,]$ratio_after_1995 = (6052*0.176 + 4408*0.26)/(6052+4408)
total_sub_area_bild_count[10,]$ratio_after_1995 = (6052*0.176 + 4408*0.26)/(6052+4408)

final_build_count <- total_sub_area_bild_count %>% dplyr::select(OKRUG,
                                     ratio_before_1920,
                                     ratio_1921_1945,
                                     ratio_1946_1970,
                                     ratio_1971_1995,
                                     ratio_after_1995)

# adding in ratio of building age
total <- left_join(total, convert, by = "sub_area")
names(total)
names(final_build_count)
total <- left_join(total, final_build_count, by = "OKRUG")

#buliding 
material_index <- total %>% dplyr::select(sub_area,OKRUG,
                 raion_build_count_with_material_info,
                 build_count_block,
                 build_count_wood,
                 build_count_frame,
                 build_count_brick,
                 build_count_monolith,
                 build_count_panel,
                 build_count_foam,
                 build_count_slag,
                 build_count_mix) %>%
  dplyr::group_by(OKRUG) %>%
  dplyr::summarise(total_build = sum(raion_build_count_with_material_info),
            total_block = sum(build_count_block),
            total_wood = sum(build_count_wood),
            total_frame = sum(build_count_frame),
            total_brick = sum(build_count_brick),
            total_monolith = sum(build_count_monolith),
            total_panel = sum(build_count_panel),
            total_foam = sum(build_count_foam),
            total_slag = sum(build_count_slag),
            total_mix = sum(build_count_mix)) %>%
  dplyr::mutate(ratio_block = total_block/total_build,
         ratio_wood = total_wood/total_build,
         ratio_frame = total_frame/total_build,
         ratio_brick = total_brick/total_build,
         ratio_monolith = total_monolith/total_build,
         ratio_panel = total_panel/total_build,
         ratio_foam = total_foam/total_build,
         ratio_slag = total_slag/total_build,
         ratio_mix = total_mix/total_build) %>%
  dplyr::select(OKRUG,
                total_build,
                ratio_wood,
                ratio_frame,
                ratio_brick,
                ratio_monolith,
                ratio_panel,
                ratio_foam,
                ratio_slag,
                ratio_mix)
View(material_index)
#ratio_wood
material_index[6,]$ratio_wood = (1489829*0.283+1591137*0.218)/(1489829+1591137)
material_index[10,]$ratio_wood = (1489829*0.283+1591137*0.218)/(1489829+1591137)

#ratio_frame
material_index[6,]$ratio_frame = (1489829*0.0567+1591137*0.00679)/(1489829+1591137)
material_index[10,]$ratio_frame = (1489829*0.0567+1591137*0.00679)/(1489829+1591137)

#ratio_brick
material_index[6,]$ratio_brick = (1489829*0.259+1591137*0.171)/(1489829+1591137)
material_index[10,]$ratio_brick = (1489829*0.259+1591137*0.171)/(1489829+1591137)

#ratio_monolith
material_index[6,]$ratio_monolith = (1489829*0.038+1591137*0.0516)/(1489829+1591137)
material_index[10,]$ratio_monolith = (1489829*0.038+1591137*0.0516)/(1489829+1591137)

#ratio_panel
material_index[6,]$ratio_panel = (1489829*0.246+1591137*0.344)/(1489829+1591137)
material_index[10,]$ratio_panel = (1489829*0.246+1591137*0.344)/(1489829+1591137)

#ratio_foam
material_index[6,]$ratio_foam = (1489829*0.00033+1591137*0.00036)/(1489829+1591137)
material_index[10,]$ratio_foam = (1489829*0.00033+1591137*0.00036)/(1489829+1591137)

#ratio_slag
material_index[6,]$ratio_slag = (1489829*0.0179+1591137*0.0306)/(1489829+1591137)
material_index[10,]$ratio_slag = (1489829*0.0179+1591137*0.0306)/(1489829+1591137)

#ratio_mix
material_index[6,]$ratio_slag = (1489829*0.00395+1591137*0.00179)/(1489829+1591137)
material_index[10,]$ratio_slag = (1489829*0.00395+1591137*0.00179)/(1489829+1591137)

#
material_index <- material_index %>% dplyr::select(-total_build)
total <- left_join(total,material_index, by = "OKRUG")
names(total)

#clean up for build_year, full_sq, and state
summary(total$full_sq)
summary(total$price_doc)
hist(total$full_sq)

total[which(total$full_sq==5326),]$full_sq <- 53.26
total[which(total$full_sq==729),]$full_sq <- 72.9
total[which(total$full_sq==637),]$full_sq <- 63.7
total[which(total$full_sq==634),]$full_sq <- 63.4
total[which(total$full_sq==603),]$full_sq <- 60.3
total[which(total$full_sq==461),]$full_sq <- 46.1
total[which(total$full_sq==412),]$full_sq <- 41.2
total[which(total$full_sq==407),]$full_sq <- 40.7
total[which(total$full_sq>300),]$full_sq <- total[which(total$full_sq>300),]$full_sq/10
total[which(total$full_sq==291),]$full_sq <- 29.1
total[which(total$full_sq==204),]$full_sq <- 20.4
total[which(total$full_sq==0),]$full_sq <- NA
total[which(total$full_sq==1),]$full_sq <- NA
total[which(total$full_sq<10),]$full_sq <- NA

summary(total$full_sq)

sub_area_full_index <- total %>% 
  dplyr::select(sub_area, full_sq) %>%
  dplyr::group_by(sub_area) %>%
  dplyr::summarise(mean_sub = median(full_sq,na.rm=TRUE))

total3 <- left_join(total, sub_area_full_index, by = "sub_area")
na.index <- which(is.na(total$full_sq))
total[na.index,"full_sq"] <- total3[na.index,"mean_sub"]
hist(total$full_sq)
summary(total$full_sq)



#clean up for build_year
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
summary(total$material)

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
                product_type) %>% 

train_1
train_1$log_price <- log(train$price_doc)
train_1 <- train_1 %>% dplyr::select(-price_doc)

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
