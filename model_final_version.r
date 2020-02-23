rm(list=ls())
#Get location of current script
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set working directory to script location
setwd(fileloc)

# Remove fileloc object
rm(fileloc)

# Set locale to English
Sys.setlocale("LC_ALL","English")

airbnb <-  read.csv2("D:/AEEM/Cross-section and Panel data analysis/Project/berlin-airbnb-data/listings_clean_14_may - Copy.csv", sep= ",")


### Histogram of price
hist(log(airbnb$price), main= "Histogram of log of price")


ggplot(data = airbnb, 
       aes(airbnb$log_price)) + 
  geom_histogram( col = "#000000", 
                 fill = "#99FFFF", 
                 alpha = .5 ) + 
  labs(x = "Log ofPrice", y = "Frequency") + 
  scale_x_continuous(breaks = c(seq(0, 800, 200),1000, 1500, 2000)) +
  scale_y_continuous(breaks = c(seq(0, 4000, 1000), 4600)) +
  ggtitle("Histogram")+
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11))

hist(airbnb$price)
# Boxplots of logarithm of price, for each room type.
ggplot(airbnb, 
       aes(x = as.numeric(airbnb$room_type), 
           y =  log(airbnb$price), 
           fill = airbnb$room_type)) + 
  geom_boxplot(alpha = .6, outlier.alpha = 0.4) + 
  scale_y_continuous(name = "Logarithm of Price") + 
  scale_x_discrete(name = "Room Type") + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.position = "bottom",
        legend.text = element_text(size = 8)) + 
  scale_fill_brewer(palette = "Paired") + 
  labs(fill = "")


### Boxplot _ Price_ by _ property_ type 
ggplot(airbnb, 
       aes(x = as.numeric(airbnb$property_type), 
           y =  log(airbnb$price), 
           fill = airbnb$property_type)) + 
  geom_boxplot(alpha = .6, outlier.alpha = 0.4) + 
  scale_y_continuous(name = "Logarithm of Price") + 
  scale_x_discrete(name = "Room Type") + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.position = "bottom",
        legend.text = element_text(size = 8)) + 
  scale_fill_brewer(palette = "Paired") + 
  labs(fill = "")


### Grouping them into categories
summary(airbnb$price)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00   32.00   49.00   69.61   75.00 9000.00 

#Divide prices into categories based on above frequencies
airbnb$price2 <- cut(airbnb$price, c(0,32,70,150,8000), c("Cheap", "Average", "Pricey", "Very Expensive"), include.lowest = TRUE, right = TRUE)

grouped <- airbnb %>%
  group_by(price2) %>%
  count(price2)


# Neighbourhood # number of listings???
qplot(airbnb$neighbourhood_group, main="", data=airbnb ) +
  theme_minimal(base_size=18) +
  xlab("Neighbourhood Group") +
  ylab("") +
  geom_bar(aes(fill = airbnb$neighbourhood_group)) +
  coord_flip()+
  scale_fill_discrete(guide=FALSE) 

dev.copy(png,'neigh.png', width=700)
dev.off() 



#6.2 Type of airbnb and neighbourhood group grouped by price group
qplot(x=airbnb$room_type, main="", data=airbnb) +
  facet_wrap(~ price2) +
  theme_minimal(base_size=18) +
  xlab("Type of airbnb") +
  ylab("Freq") +
  geom_bar(aes(fill = airbnb$neighbourhood_group)) +
  scale_fill_discrete(name="Neighbourhood groups") + coord_flip()


# Side-by-side boxplots of logarithm of price, for each neighbourhood.
ggplot(airbnb, 
       aes(x = airbnb$neighbourhood_group_cleansed, 
           y = log(airbnb$price), 
           fill = airbnb$neighbourhood_group_cleansed)) + 
  geom_boxplot(alpha = .6, outlier.alpha = .4) +
  scale_y_continuous(name = "Logarithm of Price") + 
  scale_x_discrete(name = "Neighbourhoods") + 
  theme_minimal() +
  theme( axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle=45),
        legend.position = "bottom") +
 theme(legend.text = element_text(size = 10)) + 
  labs(fill = "")

# Correlograms represent pairwise correlations between each variable. DOES NOT WORK!!!!!
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(as.matrix(airbnb[, 2:ncol(airbnb)])), method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, 
         diag = FALSE)
library(corrplot)


### MODEL BUILDING 

### Tranform_variables 
# Cleaning the data 
airbnb$log_price <- log(airbnb$price)  #adding a column in the data 

airbnb$name <- as.character(airbnb$name)
airbnb$host_location <- as.character(airbnb$host_location)
airbnb$host_name <- as.character(airbnb$host_name)
airbnb$last_review <- as.Date(airbnb$last_review)
airbnb$reviews_per_month <- as.numeric(airbnb$reviews_per_month)
airbnb$neighbourhood_group_cleansed <- as.character(airbnb$neighbourhood_group)
airbnb$cleaning_fee = as.character(airbnb$cleaning_fee)
airbnb$cleaning_fee = as.numeric(airbnb$cleaning_fee)
airbnb$bathrooms = as.numeric(airbnb$bathrooms)
airbnb$property_type<- as.character(airbnb$property_type)

airbnb$log_price<- na.omit(airbnb$log_price)  ### omit the NAs 

sum(is.na(airbnb$log_price))

airbnb$neighbourhood_group_cleansed[which(airbnb$neighbourhood_group_cleansed == "Tempelhof - Schoenenberg")] <- "Other"
airbnb$neighbourhood_group_cleansed[which(airbnb$neighbourhood_group_cleansed == "Pankow")] <- "Other"
airbnb$neighbourhood_group_cleansed[which(airbnb$neighbourhood_group_cleansed == "Friedrichshain-Kreuzberg")] <- "Other"

###Trrain test sets
index_train <- sample(1:nrow(airbnb),0.8*nrow(airbnb))

# Create training set: training_set
training_set <- airbnb[index_train, ]

# Create test set: test_set
test_set <- airbnb[-index_train,]

# We remove levels of property_type with less than or equal to five observations. There are 10 such levels.
table(airbnb$property_type)
airbnb = airbnb[!as.factor(airbnb$property_type) %in% 
                  which(table(airbnb$property_type) <= 25), ] 

library(dplyr)

#### Leave only House and Apartment/ Deleting variables 
airbnb_app_house <- airbnb [(airbnb$property_type == "Apartment" & airbnb$property_type == "House"),]
airbnb %>% 
  select(colnames(airbnb)) %>%
  filter( property_type %in% c("Apartament", "House"))

airbnb_app_house$host_location <- NULL
airbnb_app_house$zipcode <- NULL
airbnb_app_house$latitude <- NULL
airbnb_app_house$extra_people <- NULL
airbnb_app_house$longitude <- NULL

### Remove outliers 
#table(airbnb$price)
airbnb<-  airbnb[(airbnb$price)< 2500, ]
airbnb<-  airbnb[(airbnb$price)> 0, ]
# remocve all listings above the price 2500 per night 
hist(airbnb$price)
hist(airbnb_app_house$price)


# Function vif() examines for multicolinearity between regressors.
library(car)
vif(fullModel)

fullModel <- lm(log_price ~  room_type + bedrooms +
              neighbourhood_group_cleansed + 
               bathrooms + cleaning_fee +review_scores_rating + minimum_nights+ guests_included, data = airbnb)  

summary(fullModel)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(fullModel)

summary(airbnb$review_scores_rating)
summary(airbnb$bathrooms)

summary(airbnb$cleaning_fee)

glm_model <- glm(fullModel, family= "weights", data= training_set)

hist(fullModel$residuals)
jarque.test(fullModel$residuals)

training_set$cleaning_fee <- as.numeric(training_set$cleaning_fee)

### Tests for heteroscedasticity 
lmtest::bptest(fullModel)   #can reject the null hypothesis that the variance 
#of the residuals is constant and infer that heteroscedasticity is indeed present

## Fixing the heteroscedasticity: 
library(sandwich)
vcv <- vcovHC(fullModel, type = "HC1")
#sandwich:: coeftest(fullModel, vcv)
#install.packages("robustbase")
library(robustbase)
library(lmtest)
library(sandwich)
coeftest(fullModel, vcov = vcovHC(fullModel, "HC1"))
### Forecasts ??? predict and compare to the test set 


predicted_values <- predict(fullModel, newdata= test_set)
actual_values <- test_set$log_price

plot(predicted_values, type = "l")

### Residuals diagnostics + Actual vs fitted 
hist(fullModel$residuals)

qq_plot_residuals <- qqPlot(fullModel$residuals)

hist(fullModel$residuals)

plot(predicted_values, actual_values)

library(tseries)
jarque.bera.test(fullModel$residuals)  # reject the normality 
jarque.bera.test(airbnb$log_price)   # not normal 

coef(fullModel)




