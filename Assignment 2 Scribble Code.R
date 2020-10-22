library(xlsx)
library(readxl)
library(readr)
library(dplyr)
library(Hmisc)
library(lubridate)
library(outliers)
library(MVN)

df <- read_csv("Aus_crash.csv")
ddf <- read_csv("Aus_RDT.csv")

table(ddf$State)

#RDT Clean
RDT <- read_csv("BITRE_Roadside_drug_testing_data.csv")
head(RDT)
RDT <- RDT %>% select(-Licences)
colnames(RDT)[5] <- "Drug Related Crash Fatalities"
colSums(is.na(RDT))
summary_table <- RDT %>% group_by(State) %>% summarise("Mean RDT NA Removed"=mean(`Road side drug test`,na.rm=TRUE),
                                      "Mean Positives NA Kept"=mean(`Road side drug test`),
                                      "Mean Positives NA Removed"=mean(`Positive drug test`,na.rm=TRUE),
                                      "Mean PDT NA Kept"=mean(`Positive drug test`),
                                      "Mean Crash NA Removed"=mean(`Drug Related Crash Fatalities`,na.rm=TRUE),
                                      "Mean Crash NA Kept"=mean(`Drug Related Crash Fatalities`))

#### Missing Value Update

ACT <- RDT %>% filter(State == "ACT")
ACT
ACT$`Drug Related Crash Fatalities` <- impute(ACT$`Drug Related Crash Fatalities`, fun = mean)
#Missing Value for first four entries so we will remove
ACT <- ACT[5:12,]

NSW <- RDT %>% filter(State == "NSW")
NSW
NSW$`Drug Related Crash Fatalities` <- impute(NSW$`Drug Related Crash Fatalities`, fun = mean)

NT <- RDT %>% filter(State == "NT")
NT
NT$`Drug Related Crash Fatalities` <- impute(NT$`Drug Related Crash Fatalities`, fun = mean)
## Missing detail so only can assume positives were measured ##
NT$`Road side drug test` <- NT$`Positive drug test`

Qld <- RDT %>% filter(State == "Qld")
Qld$`Drug Related Crash Fatalities` <- impute(Qld$`Drug Related Crash Fatalities`, fun = mean)

SA <- RDT %>% filter(State == "SA")
SA$`Drug Related Crash Fatalities` <- impute(SA$`Drug Related Crash Fatalities`, fun = mean)

Tas <- RDT %>% filter(State == "Tas")
Tas
Tas$`Drug Related Crash Fatalities` <- impute(Tas$`Drug Related Crash Fatalities`, fun = mean)
## Tas Numbers are close so will perform a ration replacement ##
Tas$`Road side drug test` [is.na(Tas$`Road side drug test`)] <- round((412/211)*252)
Tas$`Positive drug test` [is.na(Tas$`Positive drug test`)] <- round(1427/(1678/573))

#No Data so we will input 0#
Vic_WA <- RDT %>% filter(State == "Vic" | State == "WA")
head(Vic_WA,15)
Vic_WA$`Drug Related Crash Fatalities` <- 0

### Reconstruction of RDT Data Frame ###

RDT <- bind_rows(ACT,NSW,NT,Qld,SA,Tas,Vic_WA)
RDT$`Drug Related Crash Fatalities` <- round(RDT$`Drug Related Crash Fatalities`)

colSums(is.na(RDT))
sapply(RDT, mode)
sapply(total_crash, mode)
total_crash$Year <- as.numeric(total_crash$Year)
total_crash$`All Road User Deaths` <- as.numeric(total_crash$`All Road User Deaths`)
#############

#Crash Data set

early_crash <- read_excel("Road_crash_2013.xls", sheet = 2, skip=5)
early_crash <- early_crash[8:13,1:10]
early_crash<- early_crash[,-2]
table(Aus_Road$State)

late_crash <- read_xlsx("Road_crash_2019.xlsx", sheet = 4,skip = 5)
late_crash <- late_crash[7:12,1:10]
late_crash<- late_crash[,-2]

total_crash<- bind_rows(early_crash,late_crash)
colnames(total_crash)[1]<- c("Year")

#Messy Data
#Column headers are values, not variable names:
total_crash <- total_crash %>%
  gather(`NSW`, `Vic`, `Qld`,`SA`,`WA`,`Tas`,
         `NT`,`ACT`,key = "State", value = "All Road User Deaths")


#Final Merge
Aus_Road <- merge(RDT, total_crash, by=c("State","Year"))

##Type Check
sapply(Aus_Road,mode)


test <- as.Date(as.character(Aus_Road$Year), format = "%Y")
Aus_Road$Year <- lubridate::ymd(Aus_Road$Year, truncated = 2L)




##Factor Check
levels(Aus_Road$State)
Aus_Road$State <- Aus_Road$State %>% 
  factor(levels = c("ACT","NSW","NT","Qld","SA","Tas","Vic","WA"), 
         labels = c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA"))

class(Aus_Road$State)
levels(Aus_Road$State)


Aus_Road$`All Road User Deaths` <- as.numeric(Aus_Road$`All Road User Deaths`)
Aus_Road <- Aus_Road %>% mutate("Percent of Drug Fatalities" = (`Drug Related Crash Fatalities`/`All Road User Deaths`)*100)

head(Aus_Road)


##Outliers

Aus_Road$`Positive drug test` %>%  
  boxplot(main="Box Plot of Positive Drug Tests", ylab="Count", col = "steelBlue")

par(mfrow=c(1,1))
boxplot(Aus_Road$`Road side drug test` ~ Aus_Road$State, main="Diamond carat by cut", ylab = "Carat", xlab = "Cut")
boxplot(Aus_Road$`Positive drug test` ~ Aus_Road$State, main="Diamond carat by cut", ylab = "Carat", xlab = "Cut")
boxplot(Aus_Road$`Drug Related Crash Fatalities` ~ Aus_Road$State, main="Diamond carat by cut", ylab = "Carat", xlab = "Cut")
boxplot(Aus_Road$`All Road User Deaths` ~ Aus_Road$State, main="Diamond carat by cut", ylab = "Carat", xlab = "Cut")
boxplot(Aus_Road$`Percent of Drug Fatalities` ~ Aus_Road$State, main="Diamond carat by cut", ylab = "Carat", xlab = "Cut")

sapply(Aus_Road, mode)

boxplot(Aus_Road$`All Road User Deaths` ~ Aus_Road$State, main="Road User Deaths by State", ylab = "Count", xlab = "State")

Aus_Road %>% 
  group_by(State) %>%
  summarise(UQR =  quantile(`All Road User Deaths`,probs = .75,na.rm
                                = TRUE))
Aus_Road2 <- Aus_Road %>% slice(-c(21,58))
which(Aus_Road$State == "NSW" & Aus_Road$`All Road User Deaths`>382)
Aus_Road2 <- Aus_Road %>% slice(-c(10,11,18,21,23,31,33,34,37,57,58,65))
Aus_Road_Numeric <- Aus_Road[,3:7]
Aus_Road_Numeric %>% mvn(multivariateOutlierMethod = "quan", showOutliers = TRUE)


library(forecast)
boxcox_salary<- BoxCox(Aus_Road$`All Road User Deaths`,lambda = "auto")
hist(boxcox_salary)


ARUD <- 
  
  
hist((log10(Aus_Road2$`Road side drug test`))^2)
boxcox_salary<- BoxCox(Aus_Road2$`All Road User Deaths`,lambda = "auto")
hist(boxcox_salary)
hist((Aus_Road2$`All Road User Deaths`)^(1/2))
hist((Aus_Road2$`Percent of Drug Fatalities`))
####### MIGHT NEED ######## ######## ######## ######## ######## ########


#Crash Clean
crash <- read_csv("Aus_crash.csv")
crash <- crash %>% select(Year,State,`Number Fatalities`)
colSums(is.na(crash))
crash <- crash %>% group_by(State,Year)
crash <- crash %>% summarise("Crash Fatalities" = sum(`Number Fatalities`))

##Subset for Data frame merge
crash <- crash %>% subset(Year > 2007)
crash <- crash %>% subset(Year < 2020)
crash <- crash[5:96,]


#### Data Frame Merge
Aus_Road <- merge(RDT, crash, by=c("State","Year"))










colnames(RDT)


















