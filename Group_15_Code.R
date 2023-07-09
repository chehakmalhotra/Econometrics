# Libraries
library("dplyr")
library("tidyr")
library("skimr")
library("ggplot2")
library("moments")
library("car")
library("data.table")
library("fixest")
library("stargazer")
library("nlme")

# Loading the ground water dataset
ground_water_df <- read.csv("E:/Sem8/ECO/Project/Project 1/ZIP/7063/NDAP_REPORT_7063.csv")

# Removing rows for which ground water level is NA
ground_water_df <- ground_water_df[!is.na(ground_water_df$Ground.water.level),]

# Creating district year ID (dyID) (will become unique after aggregating month level data to year level)
# defined as dc*10000 + y
# where dc is District.lgd.code and y is year
ground_water_df <- ground_water_df %>% mutate(dyID = ground_water_df$District.lgd.code*10000 + ground_water_df$Yearcode)

# Aggregating data to be at year level
# Ground water level for the year is calculated as average of ground water level across months
ground_water_year_wise_df <-
  ground_water_df %>% 
  group_by(dyID) %>% 
  mutate(Ground.water.level.district.year = mean(Ground.water.level)) %>%
  ungroup()

# Adding this year wise ground water level data to original data frame and
# retaining only unique rows according to dyID
ground_water_district_year_level_df <- ground_water_year_wise_df[,c('Country','State.lgd.code','State','District.lgd.code','District','Yearcode','Year','dyID','Ground.water.level.district.year')]
ground_water_district_year_level_df <- ground_water_district_year_level_df %>% distinct(dyID, .keep_all = TRUE)

# Loading the SDP dataset
sdp_df <- read.csv("E:/Sem8/ECO/Project/Project 1/SDP.csv")

# Merging ground water and SDP dataset based on state and yearcode
df_merge_1 <- merge(x=ground_water_district_year_level_df, y=sdp_df, by=c("State","Yearcode"))

# Loading the gini index dataset
gini_df <- read.csv("E:/Sem8/ECO/Project/Project 1/GINI.csv")

# Merging previously merged dataset with gini index dataset based on district
df_merge_2 <- merge(x=df_merge_1, y=gini_df, by=c("District"))

fem_df=read.csv("E:/Sem8/ECO/Project/Project 1/Female worker population_new - Sheet1.csv")
colnames(fem_df)[2] <- "Yearcode"
df_merge_3 <- merge(x=df_merge_2, y=fem_df, by=c("State","Yearcode"))

winmarg <-read.csv("E:/Sem8/ECO/Project/Project 1/Marginelecnew.csv")
winmarg <-winmarg[ -c(1) ]
colnames(winmarg)[3] <- "2011"
colnames(winmarg)[4] <- "2012"
colnames(winmarg)[5] <- "2013"
colnames(winmarg)[6] <- "2014"
colnames(winmarg)[7] <- "2015"
colnames(winmarg)[8] <- "2016"
colnames(winmarg)[9] <- "2017"
colnames(winmarg)[10] <- "2018"
colnames(winmarg)[11] <- "2019"
colnames(winmarg)[12] <- "2020"
colnames(winmarg)[13] <- "2021"

winmarg_new <- pivot_longer(winmarg,cols=-c(PC.Name,State),
                            names_to='Yearcode',
                            values_to='margin')
colnames(winmarg_new)[1]<-"District"
df_merge_4 <- merge(x=df_merge_3, y=winmarg_new, by=c('District',"Yearcode"))
df_merge_4 <-df_merge_4[ -c(13) ]
colnames(df_merge_4)[3]<-"State"

sc_crime<-read.csv("E:/Sem8/ECO/Project/Project 1/SC_Crimes_newupdate - Table1.csv")
sc_crime<-sc_crime[-c(1:4,6,7,9,11,13,15)]
colnames(sc_crime) <- c("State","2011","2012","2013","2014","2015","2020","2018","2017","2019","2016","2021")
sc_crime<-sc_crime[-c(1:3),]
sc_crime$"2012"<-as.numeric(sc_crime$"2012")
sc_crime$"2013"<-as.numeric(sc_crime$"2013")
sc_crime$"2014"<-as.numeric(sc_crime$"2014")
sc_crime$"2015"<-as.numeric(sc_crime$"2015")
sc_crime$"2020"<-as.numeric(sc_crime$"2020")
sc_crime$"2019"<-as.numeric(sc_crime$"2019")
sc_crime_new <- pivot_longer(sc_crime,cols=-"State",
                             names_to='Yearcode',
                             values_to='crimerate')
sc_crime_new[is.na(sc_crime_new)] <- 0.0
sc_crime_new$Yearcode<-as.integer(sc_crime_new$Yearcode)
df_merge_5 <- merge(df_merge_4, y=sc_crime_new, by=c("State","Yearcode"))

rainfall_df <- read.csv("E:/Sem8/ECO/Project/Project 1/rainfall_new1.csv")
df_merge_6 <- merge(df_merge_5, y=rainfall_df, by=c("State","Yearcode"))

# Final dataset - df
df <- df_merge_6


# Removing rows where any of ground water level, SDP or gini index is NA
df <- df[!is.na(df$Ground.water.level.district.year),]
df <- df[!is.na(df$SDP),]
df <- df[!is.na(df$Gini.Index),]
df <- df[!is.na(df$FemWork),]
df <- df[!is.na(df$margin),]
df <- df[!is.na(df$crimerate),]
df <- df[!is.na(df$rainfall),]

# Verifying removal of NA values by checking number of non NA values across columns
colSums(!is.na(df))

# Summary of statistics of final dataset
df_summary = skim(df)
df_summary

# Saving final dataset in csv file with file name group_15.csv
write.csv(df,"E:/Sem8/ECO/Project/Project 1/group_15.csv", row.names = FALSE)

df$Ground.water.level.district.year <- as.numeric(df$Ground.water.level.district.year)
df$SDP <- as.numeric(df$SDP)
df$Gini.Index <- as.numeric(df$Gini.Index)
df$FemWork <- as.numeric(df$FemWork)
df$margin <- as.numeric(df$margin)
df$crimerate <- as.numeric(df$crimerate)
df$rainfall <- as.numeric(df$rainfall)
df$Yearcode <- as.factor(df$Yearcode)

# Plots for ground water level
ggplot(df, aes(x=Yearcode, y=Ground.water.level.district.year)) + geom_point() + xlab("Year") + ylab("Ground water level in meter")
ggplot(df, aes(x=Ground.water.level.district.year)) + geom_histogram(fill="blue", position="identity", binwidth=2) + xlab("Ground water level in meter") + ylab("Frequency")
ggplot(df, aes(x=Ground.water.level.district.year, color=Yearcode)) + geom_histogram(fill="green", alpha=0.5, position="identity", binwidth=2) + xlab("Ground water level in meter") + ylab("Frequency")
ggplot(df, aes(x=Yearcode, y=Ground.water.level.district.year)) + geom_boxplot(outlier.colour="blue", outlier.shape=4, outlier.size=1) + stat_summary() + xlab("Year") + ylab("Ground water level in meter")
skewness(df$Ground.water.level.district.year)
kurtosis(df$Ground.water.level.district.year)

# Plots for SDP
ggplot(df, aes(x=Yearcode, y=SDP)) + geom_point() + xlab("Year") + ylab("SDP in rupees crore")
ggplot(df, aes(x=SDP)) + geom_histogram(fill="blue", position="identity", binwidth=40000) + xlab("SDP in rupees crore") + ylab("Frequency")
ggplot(df, aes(x=SDP, color=Yearcode)) + geom_histogram(fill="green", alpha=0.5, position="identity", binwidth=40000) + xlab("SDP in rupees crore") + ylab("Frequency")
ggplot(df, aes(x=Yearcode, y=SDP)) + geom_boxplot(outlier.colour="blue", outlier.shape=4, outlier.size=1) + stat_summary() + xlab("Year") + ylab("SDP in rupees crore")
skewness(df$SDP)
kurtosis(df$SDP)

# Plots for gini index
ggplot(df, aes(x=Gini.Index)) + geom_histogram(fill="blue", position="identity", binwidth=0.01) + xlab("Gini Index") + ylab("Frequency")
ggplot(df, aes(x=factor(0), y=Gini.Index)) + geom_boxplot(outlier.colour="blue", outlier.shape=4, outlier.size=1) + stat_summary()
skewness(df$Gini.Index)
kurtosis(df$Gini.Index)

# Linear Regression model where
# dependent variable = ground water level in meters
# explanatory variable = SDP in rupees crore
model_1 <- lm(formula = Ground.water.level.district.year ~ SDP, data = df)
summary(model_1)

# yhat = predicted values according to regression model
df$model_1_yhat <- fitted.values(model_1)

# uhat = residuals
df$model_1_uhat <- resid(model_1)

# visualizing uhat and regression line with
# ground water level in meter on Y-axis and
# SDP on X-axis
ggplot(df, aes(SDP, Ground.water.level.district.year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_point(aes(SDP, model_1_uhat), color = "red", shape=4) +
  xlab("SDP in rupees crore") +
  ylab("Ground water level in meter")

# Scatter plot of residuals with
# uhat on Y-axis and
# SDP on X-axis
ggplot(df, aes(SDP, Ground.water.level.district.year)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_point(aes(SDP, model_1_uhat), color = "red", shape=4) +
  xlab("SDP in rupees crore") +
  ylab("Residual in meter")

# Scatter plot of predicted vs true values with
# yhat (predicted values) on Y-axis and
# y (true values) on X-axis
ggplot(df, aes(Ground.water.level.district.year, model_1_yhat)) +
  geom_point(color="purple") +
  xlab("True ground water level in meter") +
  ylab("Predicted ground water level in meter")

#qqnorm(df$model_1_uhat)
#qqline(df$model_1_uhat)
#plot(density(df$model_1_uhat))

# Histogram of uhat
ggplot(df, aes(x=model_1_uhat)) +
  geom_histogram(fill="blue", position="identity", binwidth=1) +
  geom_vline(aes(xintercept=mean(model_1_uhat)), color="blue",linetype="dashed", size=0.5) +
  xlab("Residual in meter") +
  ylab("Frequency")

# Verifying summation(uhats) = 0
summation_uhat <- sum(df$model_1_uhat)
print(summation_uhat)

# Multiple Linear Regression model where
# dependent variable = ground water level in meters
# explanatory variables = SDP in rupees crore,
#                         SDP^2 in rupees crore squared,
#                         SDP^3 in rupees crore squared,
#                         Gini
model_2 <- lm(formula = Ground.water.level.district.year ~ SDP + I(SDP^2) + I(SDP^3) + Gini.Index, data = df)
summary(model_2)

model_3 <- lm(formula = Ground.water.level.district.year ~ SDP, data = df)
summary(model_3)
sd = sd(resid(model_3))
mn = mean(resid(model_3))

## Parameters and seed
beta_0 = 8.2 # Intercept 
beta_1 = 2.423e-06 # Slope
set.seed(1)  # Seed
n = 5000     # Sample size
M = 100      # Number of experiments/iterations

## Storage vectors
slope_DT <- rep(0,M) # 0 is repeated M times in a vector
intercept_DT <- rep(0,M) # 0 is repeated M times in a vector

## Begin Monte Carlo

for (i in 1:M){ #  M is the number of iterations
  
  # Generate data
  U_i = rnorm(n, mean = 1.95e-15, sd = 11.512) # Error
  X_i = rnorm(n, mean = 599327, sd = 384013) # Independent or Explanatory variable
  Y_i = beta_0 + beta_1*X_i + U_i  # Dependent variable
  
  # Formulate data.table
  data_i = data.table(Y = Y_i, X = X_i)
  
  # Run regressions
  ols_i <- fixest::feols(data = data_i, Y ~ X)
  
  # Extract slope coefficient and save
  slope_DT[i] <- ols_i$coefficients[2]
  intercept_DT[i] <- ols_i$coefficients[1]
  
}


# Summary statistics
estimates_DT <- data.table(beta_1 = slope_DT, beta_0 = intercept_DT)
stargazer(estimates_DT[, c("beta_1", "beta_0")], type = "text")

# Visual inspection
hist(estimates_DT[, beta_0])
hist(estimates_DT[, beta_1])


# D_south
df$D_South <- ifelse(df$State == 'Andhra Pradesh' | df$State == 'Tamil Nadu' | df$State == 'Kerala' | df$State == 'Telangana' | df$State == 'Puducherry', 1, 0)
# D_East
df$D_East <- ifelse(df$State == 'Assam' | df$State == 'Manipur' | df$State == 'Tripura' | df$State == 'Sikkim' | df$State == 'West Bengal' | df$State == 'Odisha' | df$State == 'Arunachal Pradesh' | df$State == 'Meghalaya' | df$State == 'Nagaland', 1, 0)
# D_West
df$D_West <- ifelse(df$State == 'Gujarat' | df$State == 'Maharashtra' | df$State == 'Karnataka' | df$State == 'Goa', 1, 0)
# D_North
df$D_North <- ifelse(df$State == 'Jammu And Kashmir' | df$State == 'Himachal Pradesh' | df$State == 'Delhi' | df$State == 'Punjab' | df$State == 'Haryana' | df$State == 'Rajasthan' | df$State == 'Uttarakhand' | df$State == 'Chandigarh', 1, 0)
# D_Centre
df$D_Centre <- ifelse(df$State == 'Madhya Pradesh' | df$State == 'Uttar Pradesh' | df$State == 'Chhattisgarh' | df$State == 'Jharkhand' | df$State == 'Bihar', 1, 0)

model_4 <- lm(formula = Ground.water.level.district.year ~ SDP + I(SDP^2) + I(SDP^3) + Gini.Index + FemWork + margin + crimerate + rainfall + D_South + D_North + D_East + D_West, data = df)
summary(model_4)

model_5 <- lm(formula = Ground.water.level.district.year ~ SDP + I(SDP^2) + I(SDP^3) + Gini.Index + FemWork + margin + crimerate + rainfall, data = df)
summary(model_5)

SSR_rest = sum(resid(model_5)^2)
SSR_full = sum(resid(model_4)^2)
Q = 4
k = 8
N = 3240
chow_test = ((SSR_rest - SSR_full)*(N-k))/(SSR_full*Q)
qf(0.95, Q, N-k)

anova(model_5, model_4)
linearHypothesis(model_4, c("D_South=0", "D_North=0", "D_East=0", "D_West=0"))

gini2_df <- read.csv("E:/Sem8/ECO/Project/Project 1/Gini_New1.csv")
colnames(gini2_df) <- c("State","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")
gini_df_new <- pivot_longer(gini2_df,cols=-"State",
                            names_to='Yearcode',
                            values_to='gini')
colnames(gini_df_new)[3]<-"gini_state"
df_merge_x <- merge(x=df_merge_6, y=gini_df_new, by=c("State","Yearcode"))
df_merge_x <- df_merge_x[!is.na(df_merge_x$gini_state),]
df_merge_x$gini_state <- as.numeric(df_merge_x$gini_state)

model_x <- lm(formula = Ground.water.level.district.year ~ SDP + I(SDP^2) + I(SDP^3) + gini_state + FemWork + margin + crimerate + rainfall, data = df_merge_x)
summary(model_x)


# Extract the variables of interest
SDP <- df$SDP
SDP2 <- SDP^2
SDP3 <- SDP^3
Gini.Index <- df$Gini.Index
FemWork <- df$FemWork
margin <- df$margin
crimerate <- df$crimerate
rainfall <- df$rainfall
one <- rep(1, 3240)
y <- df$Ground.water.level

# Define the log-likelihood function for the multiple linear regression model
logLik_regression <- function(theta, X, y) {
  mu <- X %*% theta
  -sum(dnorm(y, mean = mu, log = TRUE))
}

# Define the negative log-likelihood function
negLogLik_regression <- function(theta, X, y) {
  -logLik_regression(theta, X, y)
}

# Use the optim function to maximize the negative log-likelihood
init_theta <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
result <- optim(init_theta, negLogLik_regression, X = cbind(1, SDP, SDP2, SDP3, Gini.Index, FemWork, margin, crimerate, rainfall), y = y, control = list(fnscale = -1))

# Extract the MLE estimates of the coefficients
alpha0 <- result$par[1]
alpha1 <- result$par[2]
alpha2 <- result$par[3]
alpha3 <- result$par[4]
alpha4 <- result$par[5]
alpha5 <- result$par[6]
alpha6 <- result$par[7]
alpha7 <- result$par[8]
alpha8 <- result$par[9]

# Print the MLE estimates of the coefficients
cat("alpha0 =", alpha0, "\n")
cat("alpha1 =", alpha1, "\n")
cat("alpha2 =", alpha2, "\n")
cat("alpha3 =", alpha3, "\n")
cat("alpha4 =", alpha4, "\n")
cat("alpha5 =", alpha5, "\n")
cat("alpha6 =", alpha6, "\n")
cat("alpha7 =", alpha7, "\n")
cat("alpha8 =", alpha8, "\n")


#df_temp <- data.frame(one, SDP, SDP2, SDP3, Gini.Index, FemWork, margin, crimerate, rainfall)
#X_matrix = data.matrix(df_temp)
#xx = t(X_matrix) %*% X_matrix
#xxx = solve(xx)

df_South <- df[df$D_South == 1, ]
df_East <- df[df$D_East == 1, ]
df_West <- df[df$D_West == 1, ]
df_North <- df[df$D_North == 1, ]
df_Centre <- df[df$D_Centre == 1, ]

mean_Overall <- mean(df$Ground.water.level)
mean_South <- mean(df_South$Ground.water.level.district.year)
mean_East <- mean(df_East$Ground.water.level.district.year)
mean_West <- mean(df_West$Ground.water.level.district.year)
mean_North <- mean(df_North$Ground.water.level.district.year)
mean_Centre <- mean(df_Centre$Ground.water.level.district.year)

var_Overall <- var(df$Ground.water.level)
var_South <- var(df_South$Ground.water.level.district.year)
var_East <- var(df_East$Ground.water.level.district.year)
var_West <- var(df_West$Ground.water.level.district.year)
var_North <- var(df_North$Ground.water.level.district.year)
var_Centre <- var(df_Centre$Ground.water.level.district.year)

gls_model <- gls(Ground.water.level.district.year ~ SDP + I(SDP^2) + I(SDP^3) + Gini.Index + FemWork + margin + crimerate + rainfall, data = df, correlation = corAR1(form = ~1))
summary(gls_model)

