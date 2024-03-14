library(ggmap)
Myfile2="/Users/anilareddy/Downloads/project/portland/datasets/listings.csv"
listings <- read.csv(Myfile2)
str(listings)
summary(listings)
colSums(is.na(listings))

#cleaning and preprocessing
# This dataset has a lot of missing values and there are a lot of columns that would not be needed for analysis.
listings <- listings %>% select(-listing_url,-source, -scrape_id,-last_scraped,-name,-description, -neighborhood_overview,-picture_url,-host_url, -host_about, -host_thumbnail_url,-host_picture_url, -license, -host_name, -host_location,
                                -host_neighbourhood, -neighbourhood_group_cleansed,-host_acceptance_rate,
                                -has_availability, -calendar_last_scraped,-host_listings_count,-host_verifications, -host_has_profile_pic, -host_identity_verified,neighbourhood, -bathrooms, -minimum_minimum_nights, -maximum_minimum_nights, -minimum_maximum_nights, -maximum_maximum_nights, -minimum_nights_avg_ntm, -maximum_nights_avg_ntm,-calendar_updated, -number_of_reviews_l30d, -calculated_host_listings_count_private_rooms, -calculated_host_listings_count_shared_rooms, -calculated_host_listings_count_entire_homes)
listings <- listings %>% select(-neighbourhood,-host_response_time, -host_response_rate, -host_since)
str(listings)
summary(listings)
cleaned_listings<-listings
# the columns reviews_per_months, beds and bathrooms have lot of null values. we could replace them by mean.
cleaned_listings$beds[is.na(cleaned_listings$beds)] <- mean(cleaned_listings$beds, na.rm = TRUE)
cleaned_listings$bedrooms[is.na(cleaned_listings$bedrooms)] <- mean(cleaned_listings$bedrooms, na.rm = TRUE)
colSums(is.na(cleaned_listings))
summary(cleaned_listings)
str(cleaned_listings)
cleaned_listings$price <- as.numeric(gsub("[$,]", "", cleaned_listings$price))
cleaned_listings$first_review <- as.Date(cleaned_listings$first_review, format =  "%m/%d/%Y")
cleaned_listings$last_review <- as.Date(cleaned_listings$last_review, format =  "%m/%d/%Y")
cleaned_listings$review_scores_value[is.na(cleaned_listings$review_scores_value)] <- mean(cleaned_listings$review_scores_value, na.rm = TRUE)
cleaned_listings$reviews_per_month[is.na(cleaned_listings$reviews_per_month)] <- mean(cleaned_listings$reviews_per_months, na.rm = TRUE)
cleaned_listings$review_scores_checkin[is.na(cleaned_listings$review_scores_checkin)] <- mean(cleaned_listings$review_scores_checkin, na.rm = TRUE)
cleaned_listings$review_scores_communication[is.na(cleaned_listings$review_scores_communication)] <- mean(cleaned_listings$review_scores_communication, na.rm = TRUE)
cleaned_listings$review_scores_location[is.na(cleaned_listings$review_scores_location)] <- mean(cleaned_listings$review_scores_location, na.rm = TRUE)
cleaned_listings$review_scores_rating[is.na(cleaned_listings$review_scores_rating)] <- mean(cleaned_listings$review_scores_rating, na.rm = TRUE)
cleaned_listings$review_scores_accuracy[is.na(cleaned_listings$review_scores_accuracy)] <- mean(cleaned_listings$review_scores_accuracy, na.rm = TRUE)
cleaned_listings$review_scores_cleanliness[is.na(cleaned_listings$review_scores_cleanliness)] <- mean(cleaned_listings$review_scores_cleanliness, na.rm = TRUE)
cleaned_listings$reviews_per_month[is.na(cleaned_listings$reviews_per_month)] <- mean(cleaned_listings$reviews_per_month, na.rm = TRUE)
summary(cleaned_listings)
cleaned_listings <- cleaned_listings %>% select(-first_review, -last_review)
cleaned_listings$price <- as.numeric(gsub("[$,]", "", cleaned_listings$price))
write.csv(cleaned_listings, "hh.csv", row.names = FALSE)


listingdata="/Users/anilareddy/hh.csv"
listing<-read.csv(listingdata, stringsAsFactors=TRUE)
listing <- listing %>% dplyr::select(-availability_30,-availability_60, -availability_90, -number_of_reviews_ltm)
blank_values <- colSums(listing == "")
blank_values
#we could see that there no blank values.
listing[listing==""]<-NA
listing<-listing[complete.cases(listing),]
summary(listing)
#Now there are no blank spaces or na values in any column.
str(listing)

#EDA and visualisations
ggplot(data = listing, aes(x = price, y = number_of_reviews))+ 
  geom_jitter(color="steelblue4") + 
  ggtitle("Distribution of Price According to Review")

ggplot(data = listing, aes(x = price, y = reviews_per_month))+ 
  geom_jitter(color="steelblue4") + 
  ggtitle("Distribution of Price According to Reviews per month")

ggplot(listing, aes(x = listing$price)) +
  geom_histogram(bins=30, fill = "lightblue", color = "black") +
  labs(x = "price", y = "Frequency", title = "Histogram of price")

MyPlot7 <- ggplot(listing, aes(price))
MyPlot7 + geom_density(aes(fill=factor(accommodates)), alpha=0.2) + 
  labs(title="Density plot", 
       subtitle="Prices Based on number of people a listing could accommodate")


#Only considering price from 0 to 300
boxplot.stats(listing$price)$out
out <- boxplot.stats(listing$price)$out
out_ind <- which(listing$price %in% c(out))
out_ind
listing[out_ind, ]
boxplot(listing$price,
        ylab = "price",
        main = "Boxplot of prices", ylim = c(0, 9999)
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
listing <- subset(listing,price < 300)
listing <- subset(listing,price > 0)
boxplot(listing$price,
        ylab = "price",
        main = "Boxplot of prices", ylim = c(0, 2000)
)

mtext(paste("Outliers: ", paste(out, collapse = ", ")))
plot(density(listing$price),
     main = "Density plot", #this is the title of the plot
     xlab = "distribution of price of listings") 

str(listing)
listing$price <- as.integer(listing$price)

#hist of price
ggplot(listing, aes(x = listing$price)) +
  geom_histogram(bins=30, fill = "lightblue", color = "black") +
  labs(x = "price", y = "Frequency", title = "Histogram of price")

#regression
reg1 <- lm(data=listing, price~ longitude + latitude + accommodates + bedrooms + host_total_listings_count + minimum_nights + reviews_per_month + calculated_host_listings_count + number_of_reviews + availability_365 + maximum_nights)
summary(reg1)
par(mfrow=c(2,2)) 
plot(reg1)

reg2<- lm(data=listing, price~ longitude + latitude + accommodates + bedrooms + host_total_listings_count + minimum_nights + reviews_per_month + calculated_host_listings_count +number_of_reviews)
summary(reg2)
par(mfrow=c(2,2)) 
plot(r_reg)



#ANOVA testing

check.aov<- aov(price~reviews_per_month,listing )
summary(check.aov)
AIC(check.aov)

check.aov<- aov(price~number_of_reviews,listing)
summary(check.aov)
AIC(check.aov)

check.aov<- aov(price~accommodates,listing)
summary(check.aov)
AIC(check.aov)


check.aov<- aov(price~minimum_nights,listing)
summary(check.aov)
AIC(check.aov)


check.aov<- aov(price~bedrooms,listing)
summary(check.aov)
AIC(check.aov)


check.aov<- aov(price~beds,listing)
summary(check.aov)
AIC(check.aov)


check.aov<- aov(price~neighbourhood_cleansed,listing)
summary(check.aov)
AIC(check.aov)


check.aov<- aov(price~room_type,listing)
summary(check.aov)
AIC(check.aov)

check.aov<- aov(price~property_type,listing)
summary(check.aov)
AIC(check.aov)




#Split the final data into train and test
set.seed(123)
smp_siz = floor(0.75*nrow(listing)) 
train_ind = sample(seq_len(nrow(listing)),size = smp_siz)
train =listing[train_ind,]
test=listing[-train_ind,]
cat("Number of train data: ", nrow(train), "\n")
cat("Number of test data: ", nrow(test), "\n")


#correlation plot
numerical_columns <- listing %>% select_if(is.numeric)
# Access the list of column names
numerical_columns_names <- colnames(numerical_columns)
install.packages("corrplot")
#library(corrplot)
cleaned_listings.cor = cor(numerical_columns , method = c("spearman"))
corrplot(cleaned_listings.cor)



#Multiple Linear regression
lm_1 <- lm(price ~ accommodates + maximum_nights+ bedrooms +room_type+ minimum_nights + number_of_reviews + availability_365 + beds, data = train)
summary(lm_1)
AIC(lm_1)
BIC(lm_1)
#0.393


#maximim nights, availability_365
lm_2 <- lm(price ~ latitude  + longitude +accommodates + bedrooms + room_type+ minimum_nights +  availability_365 + beds + number_of_reviews +calculated_host_listings_count+reviews_per_month , data = train)
summary(lm_2)
AIC(lm_2)
BIC(lm_2)
#m1 0.4044

#host_total_listings_count
lm_3 <- lm(price ~ latitude  + host_total_listings_count+ longitude + accommodates +room_type + bedrooms + minimum_nights +  beds +  number_of_reviews + calculated_host_listings_count + reviews_per_month, data = train)
summary(lm_3)
AIC(lm_3)
BIC(lm_3)
#m3 0.4071



plot(density(lm_3$residuals))

#prediction on test set
prediction<-predict(lm_3, newdata = test)
prediction<- exp(prediction)
mse = mean(lm_3$residuals^2)
mspe <- mean((test$price - prediction)^2)
mspe
AIC(lm_3)

pred <- predict(lm_3, newdata = test)
pred <- pmax(pred, 0)
RMSE <- sqrt(mean( (test$price - pred)**2 ))
SSE <- sum((test$price - pred)**2)
SSR <- sum((pred - mean(test$price)) ** 2)
SST <- SSR +SSE
R2 <- (SST - SSE) / SST

cat("SST: ", SST, "   SSE: ", SSE, "   SSR: ", SSR, "\nR2: ", R2, "   RMSE: ", RMSE)

actual <- test$price

lr_result <- data.frame(
  "Actual" = actual,
  "Predicted" = pred
)

head(lr_result, 20)
lm_line = lm(Predicted ~ Actual, data = lr_result)
plot(x = lr_result$Actual, y = lr_result$Predicted,
     main = "Actual and Predicted Price",
     xlab = "Actual Price ($)",
     ylab = "Predicted Price ($)")
abline(lm_line, col="red", lwd=3)
mspe <- mean((test_data$y - y_pred)^2)


#SQUARE ROOT TRANSFORMATION
sqrtPrice= sqrt(listing$price)
reg2<-lm(data=listing, sqrtPrice~latitude  + host_total_listings_count+ longitude + accommodates +room_type + bedrooms + minimum_nights +  beds +  number_of_reviews + calculated_host_listings_count + reviews_per_month)
par(mfrow=c(2,2)) 
plot(reg2)
plot(density(reg2$residuals))
#Prediction after square root transformation
test$price_sqrt <- sqrt(test$price)
train$sqrtPrice <- sqrt(train$price)
lm_3_sqrt <- lm(sqrtPrice ~ latitude + host_total_listings_count + longitude + accommodates + room_type + bedrooms + minimum_nights + beds + number_of_reviews + calculated_host_listings_count + reviews_per_month, data = train)
# print the summary of the model and AIC, BIC values
summary(lm_3_sqrt)
AIC(lm_3_sqrt)
BIC(lm_3_sqrt)
prediction <- predict(lm_3_sqrt, newdata = test)
prediction <- prediction^2
# predict on test data and back-transform the predictions
prediction <- predict(lm_3_sqrt, newdata = test)
prediction <- prediction^2
# compute model evaluation metrics
mse <- mean(lm_3_sqrt$residuals^2)
RMSE <- sqrt(mean((test$price - prediction)^2))
SSE <- sum((test$price - prediction)^2)
SSR <- sum((prediction - mean(test$price))^2)
SST <- SSR + SSE
R2 <- (SST - SSE) / SST

cat("SST: ", SST, "   SSE: ", SSE, "   SSR: ", SSR, "\nR2: ", R2, "   RMSE: ", RMSE)

# plot actual vs predicted values
actual <- test$price
lr_result <- data.frame("Actual" = actual, "Predicted" = prediction)
lm_line = lm(Predicted ~ Actual, data = lr_result)

plot(x = lr_result$Actual, y = lr_result$Predicted,
     main = "Actual and Predicted Price",
     xlab = "Actual Price ($)",
     ylab = "Predicted Price ($)")

abline(lm_line, col = "red", lwd = 3)
par(mfrow=c(2,2)) 
plot(lm_3)


#Generalized linear models

#lm_1
glm_train1 <- glm(price ~ accommodates + bedrooms + minimum_nights + number_of_reviews + availability_365 + beds + room_type,data = train, family = "gaussian")
summary(glm_train1)
pred = predict(glm_train1, newdata = test, type = "response")
mspe = mean((test$price - pred)^2)
cat("MSPE is", round(mspe, 2), "\n")
RMSE <- sqrt(mean( (test$price - pred)**2 ))
SSE <- sum((test$price - pred)**2)
SSR <- sum((pred - mean(test$price)) ** 2)
SST <- SSR +SSE
R2 <- (SST - SSE) / SST
RMSE
R2
AIC(glm_train1)
BIC(glm_train1)


#lm2
glm_train2 <- glm(price ~ accommodates + bedrooms + minimum_nights + number_of_reviews + availability_365 + beds + room_type +calculated_host_listings_count+ maximum_nights, data = train, family = "gaussian")
summary(glm_train2)
pred = predict(glm_train2, newdata = test, type = "response")
mspe = mean((test$price - pred)^2)
cat("MSPE is", round(mspe, 2), "\n")
RMSE <- sqrt(mean( (test$price - pred)**2 ))
SSE <- sum((test$price - pred)**2)
SSR <- sum((pred - mean(test$price)) ** 2)
SST <- SSR +SSE
R2 <- (SST - SSE) / SST
RMSE
R2
AIC(glm_train2)
BIC(glm_train2)


#lm_3
glm_train3<- glm(price ~ latitude  + longitude + accommodates + room_type+ bedrooms + minimum_nights +  beds +  number_of_reviews + calculated_host_listings_count, data = train, family = "gaussian")
summary(glm_train3)
pred = predict(glm_train3, newdata = test, type = "response")
mspe = mean((test$price - pred)^2)
cat("MSPE is", round(mspe, 2), "\n")
RMSE <- sqrt(mean( (test$price - pred)**2 ))
SSE <- sum((test$price - pred)**2)
SSR <- sum((pred - mean(test$price)) ** 2)
SST <- SSR +SSE
R2 <- (SST - SSE) / SST
RMSE
R2
AIC(glm_train3)
BIC(glm_train3)


#Multicollinearity
library(car)
#vif_aic = vif(lm(price ~ latitude  + longitude + accommodates + bedrooms + number_of_reviews + availability_365 , calculated_host_listings_count, data = train))
#vif_bic = vif(lm(price ~ latitude  + longitude + accommodates + bedrooms + number_of_reviews + availability_365 , calculated_host_listings_count, data = train))
vif_mspe = vif(lm(price ~ latitude  + host_total_listings_count+ longitude + accommodates +room_type + bedrooms + minimum_nights +  beds +  number_of_reviews + calculated_host_listings_count + reviews_per_month, data = train))
vif_adjr2 = vif(lm(price ~ latitude  + host_total_listings_count+ longitude + accommodates +room_type + bedrooms + minimum_nights +  beds +  number_of_reviews + calculated_host_listings_count + reviews_per_month, data = train))
#vif_aic 
#vif_bic
vif_mspe
vif_adjr2


#Ridge regression
library(glmnet)
x <- model.matrix(price ~ latitude + host_total_listings_count + longitude + accommodates + room_type + bedrooms + minimum_nights + beds + number_of_reviews + calculated_host_listings_count + reviews_per_month, data = train)
y <- train$price
# Fit the ridge regression model using cross-validation to select the optimal lambda value
set.seed(123)
cv.fit <- cv.glmnet(x, y, alpha = 0, nfolds = 5, standardize = TRUE)
plot(cv.fit)
lambda.min <- cv.fit$lambda.min
# Fit the ridge regression model using the optimal lambda value
ridge.fit <- glmnet(x, y, alpha = 0, lambda = lambda.min, standardize = TRUE)
# Extract the coefficients from the ridge regression model
coef(ridge.fit)



#lr_m = loess(price ~ accommodates, train, span= 0.25)
#l1 = with(train, smooth.spline (y = price, x = accommodates, spar = 0.7))
#kr <- ksmooth(train$accommodates,train$price, kernel = "normal", bandwidth = 40, x.points = test$accommodates)
#cat("The MSPE for kernel regression is", mean((test$price - kr$y)^2), ".")
#ss <- predict(l1, x = test$accommodates)
#cat("The MSPE for smoothing spline regression is", mean((test$price - ss$y)^2), ".")
#loess <- predict(lr_m, newdata = test$accommodates)
#cat("The MSPE for loess regression is", mean((test$price - loess)^2), ".")

#Generealized Additive models
library(ggplot2)
library(mgcv)
gam_1 <- gam(price ~ s(latitude)  + s(host_total_listings_count) + s(longitude) + s(accommodates) + room_type + bedrooms + s(minimum_nights) +  s(beds) +  s(number_of_reviews) + s(calculated_host_listings_count) + s(reviews_per_month), data = train)
summary(gam_1)
plot(gam_1, pages = 1)


#semiparametric model
library(ggplot2)
library(mgcv)
gam_2 <- gam(price ~ s(latitude)  + host_total_listings_count+ longitude + accommodates +room_type + bedrooms + minimum_nights +  beds +  number_of_reviews + calculated_host_listings_count + reviews_per_month, data = train)
summary(gam_2)
plot(gam_2, pages = 1)


gam_pred = predict(gam_1, test)
gam_mspe = mean((test$price - gam_pred)^2);
cat ("The MSPE for the additive(non-parametric) model is", gam_mspe, ".")
AIC(gam_1)
BIC(gam_1)

semiparametric_pred = predict(gam_2, test)
semiparametric_mspe = mean((test$price - semiparametric_pred)^2);
cat ("The MSPE for the semiparametric model is", semiparametric_mspe, ".")
AIC(gam_2)
BIC(gam_2)

lm_pred = predict(lm_3, test)
lm_mspe =  mean((test$price - lm_pred)^2);
cat ("The MSPE for the linear regression model is", lm_mspe, ".")

pred = predict(glm_train3, newdata = test, type = "response")
mspe = mean((test$price - pred)^2)
cat("The MSPE for Generalized Linear Model is", round(mspe, 2), "\n")
AIC(glm_train3)


#correlation plot
require(MASS)
head(listing)
df<-listing
df<- df %>% dplyr::select(price, accommodates ,bedrooms , minimum_nights , number_of_reviews , availability_365 , beds ,maximum_nights,latitude, longitude)
num_cols <- sapply(df, is.numeric)

df_num <- df[, num_cols]
longley_scaled = data.frame(scale(df_num)); 
lmod_full = lm(price ~ accommodates + bedrooms + minimum_nights + number_of_reviews + availability_365 + beds + maximum_nights, data = longley_scaled)
summary(lmod_full)
str(longley_scaled)
par(mfrow = c(2,2))
plot(lmod_full)
plot(1:length(resid(lmod_full)), resid(lmod_full))
library(corrplot)
library(car)
vif(lmod_full)
col4 = colorRampPalette(c("black", "darkgrey", "grey","#CFB87C"))
corrplot(cor(longley_scaled), method = "ellipse", col = col4(100),  addCoef.col = "black", tl.col = "black")


