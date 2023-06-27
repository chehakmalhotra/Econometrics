#Creating unique identifiers
quiz3$t <- quiz3$yearcode - 2010
quiz3$ID <- quiz3$districtcode*10000 + quiz3$yearcode
road_lm <- lm(road_sum ~ t, data = quiz3)
View(road_lm)
summary(road_lm)

#Trying to resolve the lm.fit error
quiz3_new <- quiz3
quiz3_new[is.na(quiz3_new) | quiz3_new == "Inf"] <- NA
road_lm <- lm(road_sum ~ t, data = quiz3)
summary(road_lm)

#Creating a new variable rdlength and running a regression model
rdlength <- df$road_sum
rdlength <- replace(rdlength, rdlength=='.','')
rdlength <-as.numeric(rdlength)
road_lm <- lm(rdlength ~ t, data = quiz3)
summary(rdlength)
summary(road_lm)

#Creating dummy variables
southern_states <- c("Andhra Pradesh", "Tamil Nadu", "Kerala", "Telangana")
quiz3$DSouth <- ifelse(quiz3$state %in% southern_states, 1, 0)
eastern_states <- c("Assam", "Manipur", "Tripura", "Sikkim", "West Bengal", "Odisha")
quiz3$DEast <- ifelse(quiz3$state %in% eastern_states, 1, 0)
western_states <- c("Gujarat", "Maharashtra", "Karnataka")
quiz3$DWest <- ifelse(quiz3$state %in% western_states, 1, 0)
northern_states <- c("Jammu and Kashmir", "Himachal Pradesh", "Delhi", "Punjab", "Haryana", "Rajasthan", "Uttarakhand")
quiz3$DNorth <- ifelse(quiz3$state %in% northern_states, 1, 0)
central_states <- c("Madhya Pradesh", "Uttar Pradesh", "Chhattisgarh", "Jharkhand")
quiz3$DCentre <- ifelse(quiz3$state %in% central_states, 1, 0)

#Another regression model
road_lm_2 <- lm(rdlength ~ DSouth, data = quiz3)
summary(road_lm_2)

#Multiple linear regression models
road_lm_3 <- lm(rdlength ~ DSouth+DNorth+DEast+DWest, data = quiz3)
summary(road_lm_3)

quiz3$new_var <- quiz3$DSouth*quiz3$t
road_lm_4 <- lm(rdlength ~ t+DSouth+new_var, data = quiz3)
summary(road_lm_4)

