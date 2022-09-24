# Import New York Airbnb listings clean data
library(readxl)
ny_listings = read_excel("C:/Users/sonis/Desktop/Courses/Fall/MGSC661 - Multivariate Statistics for Machine Learning/Final Project/NY_Listings_clean.xlsx")
#ny_listings = read_excel("C:/Users/aruni/Desktop/McGill/MGSC 661/Final/NY_Listings_clean.xlsx")

attach(ny_listings)
#View(ny_listings)
nrow(ny_listings)

### Normalizing the Y variable to change range from 0-100 to 0-10
# zi = (xi - min(x)) / (max(x) - min(x)) * Q
ny_listings$Ratings <- 0
ny_listings$Review_Scores_Rating <- as.integer(ny_listings$Review_Scores_Rating)
attach(ny_listings)
max_y = max(ny_listings$Review_Scores_Rating)
min_y = min(ny_listings$Review_Scores_Rating)

for (i in 1:nrow(ny_listings)) {
  ny_listings[i, "Ratings"] <- (((ny_listings[i, "Review_Scores_Rating"] - min_y)/(max_y - min_y))*10)
}


################################################################
########### EXPLORATORY ANALYSIS & OUTLIER DETECTION ###########
################################################################

### Analyzing the outliers and distributions of numerical columns
par(mfrow = c(3,3))
boxplot(Host_Response_Rate, xlab = 'Host Response Rate', col = "green")
boxplot(Host_total_listings_count, xlab = 'Host Total Listings Count', col = "green")   # remove outliers above 100 (32 obs)
boxplot(Accommodates, xlab = 'Accommodates', col = "green")   # remove outliers above 10 (137 obs)
boxplot(Bathrooms, xlab = 'Bathrooms', col = "green")   # remove outliers above 4 (20 obs)
boxplot(Bedrooms, xlab = 'Bedrooms', col = "green")   # remove outliers above 5 (24 obs)
boxplot(Price, xlab = 'Price', col = "green")   # remove outliers above 900 (12 obs)
boxplot(Minimum_nights, xlab = 'Minimum Nights', col = "green")   # remove ouliers above 30 (49 obs)
boxplot(Number_of_reviews, xlab = 'Number of Reviews', col = "green")   # remove ouliers above 270 (41 obs)
boxplot(Reviews_per_month, xlab = 'Reviews Per Month', col = "green")   # remove ouliers above 15 (10 obs)


# Removing outliers from the dataframe

ny_listings = ny_listings[ny_listings$Host_total_listings_count<=100 & ny_listings$Accommodates<=10 & ny_listings$Bathrooms<=4 & 
                            ny_listings$Bedrooms<=5 & ny_listings$Price<=900 & ny_listings$Minimum_nights<=30 &
                            ny_listings$Number_of_reviews<=270 & ny_listings$Reviews_per_month<=15 , ]

nrow(ny_listings)
names(ny_listings)

### Analyzing the distribution of categorical columns

library(ggplot2)
ggplot(ny_listings, aes(Host_Is_Superhost, Ratings)) + geom_boxplot(fill = "light blue")+xlab("Host is Superhost")
ggplot(ny_listings, aes(City, Ratings)) + geom_boxplot(fill = "pink")
ggplot(ny_listings, aes(Property_type, Ratings)) + geom_boxplot(fill = "light yellow")+xlab("Property Type")
ggplot(ny_listings, aes(Room_type, Ratings)) + geom_boxplot(fill = "violet")+xlab("Room Type")


### Pearson's correlation
library(metan)  #install.packages("metan")

ny_listings_numeric = ny_listings[c("Host_Response_Rate", "Host_total_listings_count",
                                    "Accommodates", "Bathrooms", "Bedrooms", "Price",
                                    "Minimum_nights", "Number_of_reviews", "Reviews_per_month")]

corr <- corr_coef(ny_listings_numeric)
plot(corr)

# Removing 'bedrooms' since it is high collinear with Accommodates
ny_listings = subset(ny_listings, select = -c(Bedrooms) )
names(ny_listings)

### Checking distributions
par(mfrow = c(1,1))
ny_listings$Ratings <- as.numeric(ny_listings$Ratings)
hist(ny_listings$Ratings, col = "light blue", xlab = "Ratings", ylab = "Frequency", main='Distribution of Ratings')
hist(ny_listings$Review_Scores_Rating, col = "light blue", xlab = "Ratings", ylab = "Frequency", main='Distribution of Ratings')

###################################################
########### FINDING THE BEST PREDICTORS ###########
###################################################

ny_listings$Host_Is_Superhost <- as.factor(ny_listings$Host_Is_Superhost)
ny_listings$City <- as.factor(ny_listings$City)
ny_listings$Property_type <- as.factor(ny_listings$Property_type)
ny_listings$Room_type <- as.factor(ny_listings$Room_type)
attach(ny_listings)

library(randomForest)   #install.packages("randomForest")

myforest=randomForest(Ratings ~ Host_Response_Rate+Host_total_listings_count+
                        Accommodates+Bathrooms+Price+Minimum_nights+
                        Number_of_reviews+Reviews_per_month+
                        Host_Is_Superhost+City+Property_type+Room_type+
                        Wireless_Internet+Kitchen+Heating+Essentials+Air_conditioning+
                        Smoke_detector+TV+Hangers+Internet+Iron+Family_kid_friendly+Washer+
                        Dryer+Cable_TV+Pets_allowed+Pets_live_on_this_property+Breakfast+
                        Refrigerator+Hot_water+Microwave+Gym+Bathtub+Hot_tub+
                        Suitable_for_events+Smoking_allowed+Cats+Dogs+Indoor_fireplace+
                        Host_greets_you+Patio_or_balcony+Garden_or_backyard+Pool,
                      ntree=500, data=ny_listings, importance=TRUE,
                      na.action = na.omit)
myforest

importance(myforest)
varImpPlot(myforest)

# Storing predictors based on their importance
rf_imp = importance(myforest)
rf_imp <- cbind(predictor = rownames(rf_imp), rf_imp)
rownames(rf_imp) <- 1:nrow(rf_imp)
df_impt <- data.frame(rf_imp)
names(df_impt)[names(df_impt) == "X.IncMSE"] <- "PercentIncMSE"
df_impt$PercentIncMSE <- as.character(df_impt$PercentIncMSE)
df_impt$PercentIncMSE <- as.numeric(df_impt$PercentIncMSE)
df_impt = df_impt[order(-df_impt$PercentIncMSE),]

impt_preds <- as.vector(df_impt$predictor)
impt_preds <- append(impt_preds, 'Ratings', 0)

#impt_preds <- c("Ratings", "Host_Is_Superhost",	"Host_total_listings_count",	"Accommodates",	"Price",	"Washer",	"Reviews_per_month",	"Room_type",	"Hangers",	"Property_type",	"Bathrooms",	"Dryer",	"Microwave",	"City",	"Minimum_nights",	"Refrigerator",	"Host_Response_Rate",	"TV",	"Cable_TV",	"Family_kid_friendly",	"Hot_water",	"Kitchen",	"Garden_or_backyard",	"Smoke_detector",	"Wireless_Internet",	"Gym",	"Suitable_for_events",	"Cats",	"Bathtub",	"Iron",	"Internet",	"Patio_or_balcony",	"Indoor_fireplace",	"Breakfast",	"Pets_allowed",	"Number_of_reviews",	"Dogs",	"Essentials",	"Pets_live_on_this_property",	"Air_conditioning",	"Smoking_allowed",	"Hot_tub",	"Heating",	"Host_greets_you",	"Pool")
#length(impt_preds)

df <- ny_listings[,impt_preds]
names(df)

df$Host_Is_Superhost <- as.factor(df$Host_Is_Superhost)
df$City <- as.factor(df$City)
df$Property_type <- as.factor(df$Property_type)
df$Room_type <- as.factor(df$Room_type)
attach(df)


######### Train Test Split

smp_size <- floor(0.7 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]
nrow(df)
nrow(train)
nrow(test)


########################################
############ MODEL BUILDING ############
########################################
####### MODEL 1: REGRESSION TREE #######
########################################

library(tree)    #install.packages('tree')
library(rpart)   #install.packages('rpart.plot')
library(rpart.plot)

min_rpart_mse = vector()
cp_value_list = vector()
no_of_predictors_rpart_temp = vector()

# Loop for detecting the optimum number of predictors and respective CP value for the model
for (i in 2:length(impt_preds)) {
  x = impt_preds[1:i]
  y = impt_preds[2:i]
  mytree <-rpart(Ratings ~ ., data =train[, x], control=rpart.control(cp=0.00001))
  opt_cp=mytree$cptable[which.min(mytree$cptable[,'xerror']),'CP']
  mytree <-rpart(Ratings ~ ., data =train[, x], control=rpart.control(cp=opt_cp))
  pred = predict(mytree, test[, y])
  mse = mean((test$Ratings - pred)**2)
  print(paste("# of Preds: ",(i-1),"MSE: ",mse, "Optimal CP: ", opt_cp))
  min_rpart_mse <- append(min_rpart_mse, mse)
  cp_value_list <- append(cp_value_list, opt_cp)
  no_of_predictors_rpart_temp <- append(no_of_predictors_rpart_temp, i-1)
}

# Printing minimum MSE and optimum number of predictors
print(paste("Minimum MSE is", min(min_rpart_mse), "with", which.min(min_rpart_mse),"predictors"))
impt_preds_rpart_temp = impt_preds[2:((which.min(min_rpart_mse))+1)]
cp_value_temp <- cp_value_list[(which.min(min_rpart_mse))]

# Plotting the number of predictors and their corresponding MSE
line_rpart_df <- data.frame(no_of_predictors_rpart_temp, min_rpart_mse)
library(ggplot2)
ggplot(line_rpart_df, aes(x = no_of_predictors_rpart_temp, y = min_rpart_mse)) +
  geom_line() + xlab("Number of Predictors") + ylab("MSE") + ggtitle("Decision Tree MSE") +
  theme(plot.title = element_text(hjust = 0.5))

# Summary of the most optimum decision tree with optimum values
df_optimal_tree <- df[,impt_preds_rpart_temp]
opt_tree = rpart(Ratings ~ ., data=df_optimal_tree, control=rpart.control(cp=cp_value_temp))
rpart.plot(opt_tree)
summary(opt_tree)

# Since the optimum model contains 4 predictors, trying to incorporate first 4 important
# predictors analyzed by random forest feature importance; Resulted in higher MSE.
x1 = impt_preds[1:5]
y1 = impt_preds[2:5]
opt_tree1 = rpart(Ratings ~ ., data=train[,x1], control=rpart.control(cp=0.00001))
opt_cp1=opt_tree1$cptable[which.min(mytree$cptable[,'xerror']),'CP']
opt_tree1 <-rpart(Ratings ~ ., data =train[, x1], control=rpart.control(cp=opt_cp1))
pred = predict(opt_tree1, test[, y1])
mse = mean((test$Ratings - pred)**2)
mse
par(mfrow = c(1,1))
rpart.plot(opt_tree1)
summary(opt_tree1)


# Running the model by incorporating only the 4 predictors identified using the model.
x2 = c('Ratings', 'Host_Is_Superhost', 'Host_total_listings_count', 'Number_of_reviews', 'Host_Response_Rate')
y2 = c('Host_Is_Superhost', 'Host_total_listings_count', 'Number_of_reviews', 'Host_Response_Rate')
rt_start_time <- Sys.time()   #storing start time for calculating the runtime
opt_tree2 <-rpart(Ratings ~ ., data =train[, x2], control=rpart.control(cp=0.00453316623631064))
pred = predict(opt_tree2, test[, y2])
mse = mean((test$Ratings - pred)**2)
rt_stop_time <- Sys.time()   #storing stop time for calculating the runtime
rt_comp_time <- rt_stop_time - rt_start_time
mse
rpart.plot(opt_tree2)
summary(opt_tree2)


######################################
####### MODEL 2: RANDOM FOREST #######
######################################

library(randomForest)   #install.packages("randomForest")

min_rf_mse = vector()
no_of_predictors_rf_temp = vector()

# Loop for detecting the optimum number of predictors for the model running with 500 trees
for (i in 2:length(impt_preds)) {
  x = impt_preds[1:i]
  myforest_model <- randomForest(Ratings ~ ., ntree=500, data=df[,x], importance=TRUE,
                                 na.action=na.omit)
  temp_mse_rf = myforest_model$mse[length(myforest_model$mse)]
  min_rf_mse <- append(min_rf_mse, temp_mse_rf)
  no_of_predictors_rf_temp <- append(no_of_predictors_rf_temp, i-1)
  print(paste("# of Preds: ",(i-1),"MSE: ",temp_mse_rf))
}

print(paste("Minimum MSE is", min(min_rf_mse), "with", which.min(min_rf_mse),"predictors"))

# Plotting the number of predictors and their corresponding MSE
line_rf_df <- data.frame(no_of_predictors_rf_temp, min_rf_mse)
library(ggplot2)
ggplot(line_rf_df, aes(x = no_of_predictors_rf_temp, y = min_rf_mse)) +
  geom_line() + xlab("Number of Predictors") + ylab("MSE") + ggtitle("Random Forest MSE") +
  theme(plot.title = element_text(hjust = 0.5))

# Summary of the most optimum random forest
a = 42    # here, a = which.min(min_rf_mse) from the iterative model, which is 42 from the optimal model
rf_start_time <- Sys.time()   #storing start time for calculating the runtime
myforest_model_select <- randomForest(Ratings ~ ., ntree=500, data=df[,(1:(a+1))], importance=TRUE,
                                      na.action=na.omit, do.trace=100)                                                   
rf_stop_time <- Sys.time()   #storing stop time for calculating the runtime
rf_comp_time <- (rf_stop_time - rf_start_time)*60    #Since output coming as 13.29 mins, converting it to seconds for scale
myforest_model_select


#################################
####### MODEL 3: BOOSTING #######
#################################

library(gbm)   #install.packages("gbm")
set.seed (1)

no_of_predictors=vector()
depth=vector()
indi_mse=vector()

# Loop for detecting the optimum number of predictors for the model running with 10000 trees
for (j in 4:6) {
  min_boost_mse = vector()
  cat("\n")
  print(paste("Interaction Depth:",j))
  
  for (i in 2:length(impt_preds)) {
    x = impt_preds[1:i]
    y = impt_preds[2:i]
    boosted <-gbm(Ratings ~ ., data =train[, x], distribution="gaussian",
                  n.trees=10000, interaction.depth=j)
    
    pred = predict(boosted, test[, y], n.trees=10000)
    mse = mean((test$Ratings - pred)**2)
    print(paste("# of Preds: ",(i-1),"MSE: ",mse))
    
    no_of_predictors <- append(no_of_predictors, i-1)
    depth <- append(depth, j)
    indi_mse <- append(indi_mse, mse)
    min_boost_mse <- append(min_boost_mse, mse)
    
  }
  print(paste("Minimum MSE at interaction depth",j, "is", min(min_boost_mse), "at position", which.min(min_boost_mse)))
}

line_boosting_df <- data.frame(no_of_predictors, depth, indi_mse)
line_boosting_df$depth <- as.factor(line_boosting_df$depth)

# Plotting the number of predictors and their corresponding MSE
library(ggplot2)
ggplot(line_boosting_df, aes(x = no_of_predictors, y = indi_mse, color = depth)) +
  geom_line() + xlab("Number of Predictors") + ylab("MSE") + ggtitle("Boosting MSE") +
  theme(plot.title = element_text(hjust = 0.5))


num_preds_boosting = line_boosting_df[which.min(line_boosting_df$indi_mse),1]
depth_boosting = as.integer(as.character(line_boosting_df[which.min(line_boosting_df$indi_mse),2]))
indi_mse_boosting = line_boosting_df[which.min(line_boosting_df$indi_mse),3]

print(paste("Minimum MSE is", indi_mse_boosting, "at depth", depth_boosting,
            "with", num_preds_boosting, "predictors"))


# Summary of the most optimum boosted tree with optimum values
a = 4    # here, a = num_preds_boosting from the iterative model, which is 4 from the optimal boosting model
b = 5    # here, b = depth_boosting from the iterative model, which is 4 from the optimal boosting model
boosted_start_time <- Sys.time()   #storing start time for calculating the runtime
boosted_optimal <- gbm(Ratings ~ ., data =df[, (1:(a+1))],
                       distribution="gaussian", n.trees=10000,
                       interaction.depth=b)
boosted_stop_time <- Sys.time()   #storing start time for calculating the runtime
boosted_comp_time <- boosted_stop_time - boosted_start_time
summary(boosted_optimal)


############################################
####### PRINCIPAL COMPONENT ANALYSIS #######
############################################

library(ggplot2)
library(ggfortify)   #install.packages("ggfortify")
library(FactoMineR)   #install.packages("FactoMineR")
library(factoextra)   #install.packages("factoextra")

# Adding important predictors for PCA
num_preds <- c("Ratings", "Host_Is_Superhost",	"Host_total_listings_count",
               "Number_of_reviews",	"Host_Response_Rate", "Accommodates", "Price")

df_pca = df[,num_preds]
names(df_pca)

df_pca$Host_Is_Superhost <- as.integer(as.logical(df_pca$Host_Is_Superhost))

res.pca = prcomp(df_pca, scale=TRUE)

# Plotting PCA results
autoplot(res.pca, data = na.omit(df_pca), loadings = TRUE,
         col=ifelse(df_pca$Ratings>=9.5,"dark green","light blue"),
         loadings.label = TRUE)

##########################################
# Adding all numeric predictors for PCA

num_preds <- c("Ratings", "Host_Is_Superhost",	"Host_total_listings_count",	"Accommodates",
               "Price",	"Washer",	"Reviews_per_month",	"Hangers",	"Bathrooms",	"Dryer",
               "Microwave",	"Minimum_nights",	"Refrigerator",	"Host_Response_Rate",	"TV",
               "Cable_TV",	"Family_kid_friendly",	"Hot_water",	"Kitchen",
               "Garden_or_backyard",	"Smoke_detector",	"Wireless_Internet",	"Gym",
               "Suitable_for_events",	"Cats",	"Bathtub",	"Iron",	"Internet",
               "Patio_or_balcony",	"Indoor_fireplace",	"Breakfast",	"Pets_allowed",
               "Number_of_reviews",	"Dogs",	"Essentials",	"Pets_live_on_this_property",
               "Air_conditioning",	"Smoking_allowed",	"Hot_tub",	"Heating",
               "Host_greets_you",	"Pool")
df_pca = df[,num_preds]
names(df_pca)

df_pca$Host_Is_Superhost <- as.integer(as.logical(df_pca$Host_Is_Superhost))

res.pca = prcomp(df_pca, scale=TRUE)

# Plotting PCA results
autoplot(res.pca, data = na.omit(df_pca), loadings = TRUE,
         col="light grey", loadings.label = TRUE )


####### CLUSTERING ON PCA (important predictors) #######

# Dividing Ratings into classes and clustering the PCA results into clusters.
df_pca <- df[, "Ratings"]
df_pca$Ratings_Class <- 0
df_pca$Ratings <- as.numeric(df_pca$Ratings)
attach(df_pca)
max_y_c = max(df_pca$Ratings)
min_y_c = min(df_pca$Ratings)

for (i in 1:nrow(df_pca)) {
  df_pca[i, "Ratings_Class"] <- floor((((df_pca[i, "Ratings"] - min_y_c)/(max_y_c - min_y_c))*5))
}

df_pca$Ratings_Class <- as.factor(df_pca$Ratings_Class)
attach(df_pca)

set.seed(123)
var <- get_pca_var(res.pca)
res.km <- kmeans(var$coord, centers = 5, nstart = 25)
grp <- as.factor(res.km$cluster)

fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#868686FF", "#964B00", "#0073C2FF","#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

res.pca$rotation[,1:2]


#########################################
############## FINAL MODEL ##############
#########################################

library(tree)    #install.packages('tree')
library(rpart)   #install.packages('rpart.plot')
library(rpart.plot)
x2 = c('Ratings', 'Host_Is_Superhost', 'Host_total_listings_count', 'Number_of_reviews', 'Host_Response_Rate')
y2 = c('Host_Is_Superhost', 'Host_total_listings_count', 'Number_of_reviews', 'Host_Response_Rate')
opt_tree2 <-rpart(Ratings ~ ., data =df[, x2], control=rpart.control(cp=0.00453316623631064))
rpart.plot(opt_tree2)
summary(opt_tree2)

# Testing for random data points
Host_Is_Superhost <- c(FALSE, TRUE)
Host_total_listings_count <- c(10, 1)
Number_of_reviews <- c(0, 12)
Host_Response_Rate <- c(0.2,1)
df_final_test <- data.frame(Host_Is_Superhost, Host_total_listings_count,
                            Number_of_reviews, Host_Response_Rate)

df_final_test$Host_Is_Superhost <- as.factor(as.logical(df_final_test$Host_Is_Superhost))
attach(df_final_test)

predict(opt_tree2, Host_Is_Superhost=FALSE, Host_total_listings_count=10, Number_of_reviews=1, Host_Response_Rate=0.2)
predict(opt_tree2, df_final_test)

##################################################################
############################ APPENDIX ############################
# Classification Attempts: Linear/Quadratic Discriminant Analysis#
##################################################################

library(MASS)   #install.packages("MASS")
library(klaR)   #install.packages("klaR")

df_discri <- df[, c("Ratings", "Host_total_listings_count", "Host_Response_Rate")]
df_discri$Ratings <- as.factor(df_discri$Ratings)
#df_discri$Host_Is_Superhost <- as.integer(as.logical(df_discri$Host_Is_Superhost))
attach(df_discri)

df_discri$Ratings_Class <- 0
df_discri$Ratings <- as.numeric(df_pca$Ratings)
attach(df_discri)
max_y_c = max(df_discri$Ratings)
min_y_c = min(df_discri$Ratings)

for (i in 1:nrow(df_discri)) {
  df_discri[i, "Ratings_Class"] <- floor((((df_discri[i, "Ratings"] - min_y_c)/(max_y_c - min_y_c))*5))
}

df_discri$Ratings_Class <- as.factor(df_discri$Ratings_Class)
attach(df_discri)

# LDA: error rate is 0.311
partimat(Ratings_Class ~ Host_total_listings_count+Host_Response_Rate, method="lda",
         image.colors=c("white","light green", "light blue", "yellow", "light pink", "violet"))

# QDA: error rate is 0.327
partimat(Ratings_Class ~ Host_total_listings_count+Host_Response_Rate, method="qda",
         image.colors=c("white","light green", "light blue", "yellow", "light pink", "violet"))


####### Computational Comparison #######
Model_Name <- c('Regression Tree', 'Random Forest', 'Boosting')
Model_Computational_Time <- c(0.1320, 797.54, 42.5476)
Number_of_Predictors <- c(4, 42, 3)
MSE_Value <- c(0.7855, 0.7439, 0.7921)

df_computational <- data.frame(Model_Name, Model_Computational_Time, Number_of_Predictors, MSE_Value)

# number_of_predictors

ggplot(df_computational)  + 
  geom_bar(aes(x=Model_Name, y=Number_of_Predictors),stat="identity", fill="tan1", colour="sienna3")+
  geom_line(aes(x=Model_Name, y=MSE_Value*max(Number_of_Predictors), group=1),stat="identity")+
  geom_text(aes(label=MSE_Value, x=Model_Name, y=40*MSE_Value), colour="black")+
  geom_text(aes(label=Number_of_Predictors, x=Model_Name, y=1.05+Number_of_Predictors), colour="black")+
  scale_y_continuous(sec.axis = sec_axis(~./max(MSE_Value)))+xlab("Model Name")+ylab("Number of Predictors vs. MSE")


# compute_time

ggplot(df_computational)  + 
  geom_bar(aes(x=Model_Name, y=Model_Computational_Time),stat="identity", fill="tan1", colour="sienna3")+
  geom_line(aes(x=Model_Name, y=MSE_Value*max(Model_Computational_Time), group=1),stat="identity")+
  geom_text(aes(label=MSE_Value, x=Model_Name, y=780*MSE_Value), colour="black")+
  geom_text(aes(label=Model_Computational_Time, x=Model_Name, y=15+Model_Computational_Time), colour="black")+
  scale_y_continuous(sec.axis = sec_axis(~./max(MSE_Value)))+xlab("Model Name")+ylab("Model Compute Time (seconds) vs. MSE")