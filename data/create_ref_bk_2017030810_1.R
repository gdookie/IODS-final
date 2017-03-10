#install.packages("readr")
library(dplyr)
library(readr)
#install.packages("readxl")
library(readxl)
library(stringr)
setwd("/Users/gyandookie/IODS-final/data")


### 1. GET THE TOTAL POPULATION NUMBERS (2015)
### Importing data
### Population dataset by World Bank
pop_totals <- read_excel("pop_total.xls", sheet=1, skip=3, col_names=TRUE)
str(pop_totals)
dim(pop_totals)
names(pop_totals)
#Selecting specific variables from the dataset
pop_totals2015 <- select(pop_totals, starts_with("Country"), contains("2015"))
pop_totals2015
names(pop_totals2015) = c("Country","Country.Code","Pop2015")
pop_totals2015
countries <- c("Belgium", "Denmark","Finland", "Germany", "United Kingdom", "United States")
pop_total2015_ref <- filter(pop_totals2015, Country %in%  countries)
pop_total2015_ref

### 2. GET THE GNI PER COUNTRY
### Importing data
### GNI dataset by World Bank
gni <- read_delim("GNI_wb.csv", delim="," ,skip=4)
dim(gni)

#Selecting specific variables from the dataset
gni <- select(gni, 1, 59)
# Changing variable names and doublechecking everything is ok
gni
names(gni) = c("Country", "GNI")
names(gni)
gni

# Exploring the produced dataframe
gni <- arrange(gni, desc(GNI))
head(gni, 25)
tail(gni, 30)

# hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
# names(hd)
# 
# hdGNI <- select(hd, Country, GNI.per.Capita.Rank.Minus.HDI.Rank)

# names(hdGNI) = c("Country", "GNI")
# hdGNI

### 3. GET THE ARMS EXPORTS PER COUNTRY
### Importing data
### MILEX (Military export) dataset by SIPRI
# arms_ex <- read_csv("TIV-Exp-50-2016.csv", skip=10, col_names=TRUE)
# names(arms_ex)
# str(arms_ex)
# arms_ex <- select(arms_ex, one_of("Supplier", "2016"))
# 
# names(arms_ex) <- c("Country", "ArM2016")
# 
# names(arms_ex)
# arms_ex

arms_ex2 <- read_excel("SIPRI-2015.xlsx", sheet=6, skip=4, col_names=TRUE)
names(arms_ex2)
arms_ex2 <- select(arms_ex2, one_of("Country", "2015"))
names(arms_ex2)
str(arms_ex2)
dim(arms_ex2)
glimpse(arms_ex2)
#Remove all rows with missing values
data.frame(arms_ex2, comp = complete.cases(arms_ex2))
arms_ex2 <- filter(arms_ex2, complete.cases(arms_ex2) != FALSE)
names(arms_ex2) = c("Country", "ArM2015")
names(arms_ex2)
arms_ex2 <- arrange(arms_ex2, desc(ArM2015))
# arms_ex2[arms_ex2$Country == "USA"] <- "United States"
# filter(arms_ex2, Country== "United States")
# arms_ex2 <- filter(arms_ex2, !is.na(ArM2015))
# dim(arms_ex2)

### 4. & 5. TERRORIST ATTACKS PER COUNTRY & COUNTRIES BOMBED 2015-2016
### Importing data
### Islamist terrorist attacks data was gathered from wikipedia and transformed into a scv-format by me
teratt <- read_csv("itae20152016.csv", col_names=TRUE)
dim(teratt)
str(teratt)
teratt
names(teratt) = c("Country", "TerAtt", "CtryBomb")
teratt

### 6. FRAGILE STATES INDEX
### Importing data
frgstat <- read_delim("frg-stat-02.csv", delim=";")
frgstat
names(frgstat)

### Removing commas and changing the datatype to numeric so the variables can be used as continuous later on
frgstat$Leg.Stat <- str_replace(frgstat$Leg.Stat, pattern=",", replace ="")
frgstat$Leg.Stat <- as.numeric(frgstat$Leg.Stat )
frgstat$Hum.R <- str_replace(frgstat$Hum.R, pattern=",", replace ="")
frgstat$Hum.R <- as.numeric(frgstat$Hum.R )
### Selecting the variables I need in my analysis
frgstatI <- select(frgstat, Country, Total)
frgstatI
### Exploring the structure of the data
tail(frgstatI, 20)
names(frgstatI)



### 7. REFUGESS & TOP REFUGEE ORIGIN COUNTRIES

### Importing data
### This data shows the amount of refugee originating per country
ref_orig <- read.csv("ref_orig2.csv", sep=";", header=TRUE, stringsAsFactors=FALSE)
names(ref_orig)
ref_orig <- select(ref_orig, 1:3)

ref_orig
names(ref_orig) = c("Country", "PerOfPop", "RefOrig")
names(ref_orig)

names(ref_asylum) = c("Country", "Per1000", "RefAsyl")
names(ref_asylum)
ref_orig

### Removing commas and changing the datatype to numeric so the variables can be used as continuous later on
ref_orig$RefOrig<- str_replace(ref_orig$RefOrig, pattern=",", replace ="") 
ref_orig
ref_orig$RefOrig<- str_replace(ref_orig$RefOrig, pattern=",", replace ="")
ref_orig
ref_orig$RefOrig <- as.numeric(ref_orig$RefOrig)
#ref_orig$PerOfPop <- as.numeric(ref_orig$PerOfPop)
ref_orig <- select(ref_orig, Country, RefOrig)

ref_orig <-  arrange(ref_orig, RefOrig)
ref_orig <- filter(ref_orig, !is.na(RefOrig))
ref_orig

### Importing data
### This data shows the amount of refugees in a country of asylum
ref_asylum  <- read_excel("ref_asyl_wb.xls", sheet=1, skip=3, col_names=TRUE)
names(ref_asylum)
ref_asylum <- select (ref_asylum, 1, 60)
names(ref_asylum) = c("Country", "RefAsyl")
names(ref_asylum) 
glimpse(ref_asylum)

### Filtering out the NA rows
ref_asylum <- filter(ref_asylum, !is.na(RefAsyl))
ref_asylum_test <- filter(ref_asylum, Country == "United States")
ref_asylum_test




### JOIN DATAFRAMES
glimpse(ref_asylum)

total <- merge(frgstat,pop_totals2015,by="Country")
print(total)


# refBomb0 <-inner_join(frgstat, arms_ex, by = "Country")
# refBomb0[is.na(refBomb0)] <- 0
# refBomb0
arms_ex
#refBomb <- inner_join(frgstat, pop_totals2015, by = "Country")
#refBomb <- inner_join(frgstat, arms_ex, by = "Country")

refBomb <- left_join(frgstat, arms_ex, by = "Country") %>% left_join(pop_totals2015, by = "Country") %>% left_join(teratt, by = "Country")  %>% left_join(ref_asylum, by = "Country")  %>% left_join(ref_orig, by = "Country") %>% left_join(gni, by = "Country") 

#refBomb <- inner_join(refBomb, pop_totals2015, by = "Country")
refBomb <- mutate(refBomb,  refPer = round(RefAsyl * 100 /Pop2015, digits=2))
names(refBomb)
dim(refBomb)
summary(refBomb$refPer)
refBomb
refBomb <- select(refBomb, c(Country, Total, TerAtt, CtryBomb, ArM2016, RefOrig,GNI, refPer))
names(refBomb) = c("Country", "FSI", "TerAtt", "CtryBomb", "ArmSale","RefOrig", "GNI", "refPer")
names(refBomb)
refBomb$TerAtt[is.na(refBomb$TerAtt)] <- 0
refBomb$TerAtt
refBomb$CtryBomb[is.na(refBomb$CtryBomb)] <- 0
refBomb$CtryBomb
refBomb$ArmSale[is.na(refBomb$ArmSale)] <- 0
refBomb$ArmSale
refBomb$Country
data.frame(refBomb, comp = complete.cases(refBomb))
refBomb<- filter(refBomb, complete.cases(refBomb) != FALSE)
dim(refBomb)
refBomb$Country

refBomb <- rbind(refBomb, c("Syria", as.numeric(1110), as.numeric(0), as.numeric(1), as.numeric(0),as.numeric(3869626), as.numeric(5120), as.numeric(13,4))) 

refBomb$Country
filter(refBomb, Country=="Syria")
names(refBomb)
refBomb$FSI <- as.numeric(refBomb$FSI)
refBomb$TerAtt <- as.numeric(refBomb$TerAtt)
refBomb$CtryBomb <- as.numeric(refBomb$CtryBomb)
refBomb$ArmSale <- as.numeric(refBomb$ArmSale)
refBomb$RefOrig<- as.numeric(refBomb$RefOrig)
refBomb$GNI<- as.numeric(refBomb$GNI)
refBomb$refPer <- as.numeric(refBomb$refPer)

str(refBomb)
refBomb

write.csv(refBomb, "/Users/gyandookie/IODS-final/data/refBomb.csv")
