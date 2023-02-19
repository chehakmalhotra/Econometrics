2+2
a=10
a
b=1:10
b
class(b)
c=1:50
c
x=c^2
x
?max
max(x)
min(x)
summary(x)
var(x)
(var(x))^(1/2)
y=x[x<200]
var(y)
y
e <- subset(x,x%%2==0)
e

o
o <- subset(x,x%%2 !=0)
o

roadlength_districtlevel <- read.csv(Users/chehakmalhotra/Downloads/df.csv)
df <- read.csv("~/Downloads/df.csv")
View(df)
class(df)
dim(df)
head(df)
df(1,"state")

df(1,"West Bengal")
df(1,"state")
head(df$state)
length(unique(df$state))
rdlength <- df$road_sum
hist(rdlength, main="roadlength", xlab="kms", col="pink", freq=TRUE)
rdlength <- replace(rdlength, rdlength=='.','')
hist(rdlength, main="roadlength", xlab="kms", col="darkmagenta", freq=TRUE)
rdlength <- as.numeric(rdlength)
hist(rdlength, main="roadlength", xlab="kms", col="darkmagenta", freq=TRUE)
hist(rdlength, main="roadlength", xlab="kms", col="cyan", freq=TRUE)
summary(rdlength)
install.packages("ggplot2")
library(ggplot2)
df[df== "."] <- ''
ggplot(df, aes(x))
library(dplyr)
library(dplyr)
df2<- df[df$road_sum<1000,]
View(df2)
df3<- df2 %>% mutate(trend=case_when(yearcode==2011~1,
                                     yearcode==2012~2,
                                     yearcode==2013~3,
                                     yearcode==2014~4,
                                     yearcode==2015~5,
                                     yearcode==2016~6,
                                     yearcode==2017~7,
                                     yearcode==2018~8,
                                     TRUE ~11))
View(df3)
road_lm <- lm(formula=road_sum ~ trend, data=df3)
df2$road_sum <- replace(df2$road_sum, df2$road_sum=='.','')
df2$road_sum <- as.numeric(df2$road_sum)
road_lm <- lm(formula=road_sum ~ trend, data=df3)
summary(road_lm)
Range:0.8~1000.
df2$road_log<-log(df2$road_sum)

df3$yhat=0.6
gw <-NDAP_REPORT_7065
gw <- subset(gw, select = c(District,District.LGD.Code,Ground.Water.Station.Name,Ground.Water.Station.Latitude,Ground.Water.Station.Longitude,Amount.of.Calcium,YearCode))
gw<-gw[!is.na(gw$District.LGD.Code),]
view(df3)
View(df3)
gw$dyid<-paste(gw$District.gw$YearCode)
gw$dyid<-paste(gw$District.gw$YearCode, sep="")
gw<-gw[!is.na(gw$District.LGD.Code),]
gw$dyid<-paste(gw$District,gw$YearCode)
gw$dyid<-paste(gw$District,gw$YearCode,sep = "")
gw[is.na(gw)] <- ''

gw$Amount.of.Calcium <- as.numeric(gw$Amount.of.Calcium)
gw_agg <- gw %>% group_by(dyid) %>%
  
  summarise(mean_calcium=mean(Amount.of.Calcium),
            
            .groups = 'drop')
gw_agg <- gw %>% group_by(dyid) %>% summarise(mean_calcium=mean(Amount.of.Calcium), .groups = 'drop')
summary(gw_agg$mean_calcium)
View(gw)
gw_agg <- gw %>% group_by(dyid) %>%
  
  summarise(mean_calcium=mean(Amount.of.Calcium, na.rm = TRUE),
            
            .groups = 'drop')
summary(gw_agg$mean_calcium)
length(unique(gw_agg$dyid))
df4<-gw_agg
df5 = merge(x = df3, y = df4, by = "dyid")
df6 = merge(x = df3, y = df4, by = "dyid", all.x = TRUE)
View(df6)
