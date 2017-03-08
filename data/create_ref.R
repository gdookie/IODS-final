#install.packages("readr")
library(dplyr)
library(readr)
#install.packages("readxl")
library(readxl)
library(stringr)
setwd("/Users/gyandookie/IODS-final/data")


### GET THE TOTAL POPULATION NUMBERS (2015)
### dataset: World Bank
pop_totals <- read_excel("pop_total.xls", sheet=1, skip=3, col_names=TRUE)
str(pop_totals)
dim(pop_totals)
names(pop_totals)
#pop_totals2015 <- select(pop_totals, 1,60)
pop_totals2015 <- select(pop_totals, starts_with("Country"), contains("2015"))
pop_totals2015
names(pop_totals2015) = c("Country","Country.Code","Pop2015")
pop_totals2015
countries <- c("Belgium", "Denmark","Finland", "Germany", "United Kingdom", "United States")
pop_total2015_ref <- filter(pop_totals2015, Country %in%  countries)
pop_total2015_ref

### GET THE GNI PER COUNTRY

### GET THE ARMS EXPORTS PER COUNTRY
### dataset: SIPRI
arms_ex <- read_csv("TIV-Exp-50-2016.csv", skip=10, col_names=TRUE)
names(arms_ex)
str(arms_ex)
arms_ex <- select(arms_ex, one_of("Supplier", "2016"))

names(arms_ex) <- c("ArmSelCntry", "$M2016")

names(arms_ex)



### REFUGESS & TOP REFUGEE ORIGIN COUNTRIES

#1
# ref_asylum<- read_excel("ref_orig.xlsx", sheet=1, col_names= c("Country", "refugees per 1000", "Refugees N"))
# ref_orig <- read_excel("ref_orig.xlsx", sheet=2, col_names=TRUE)

#ref_orig <- read.csv("ref_orig2.csv", sep=";", header=TRUE)
# ref_orig <- read_csv("ref_orig2.csv", delim=";", header=TRUE, col_types= cols( Country = col_character(), PerOfPop = col_double(), RefOrig = col_integer())
ref_orig <- read_csv("ref_orig2.csv", delim=";", header=TRUE)
names(ref_orig)
ref_orig <- select(ref_orig, 1:3)

ref_asylum <- read.csv("ref_asylum.csv", sep=";", header=TRUE)
names(ref_asylum)
ref_asylum <- select(ref_orig, 1:3)
ref_orig
ref_asylum
names(ref_orig) = c("Country", "PerOfPop", "RefOrig")
names(ref_orig)

names(ref_asylum) = c("Country", "Per1000", "RefAsyl")
names(ref_asylum)
#ref_orig <- arrange(ref_orig, PerOfPop)
ref_orig

ref_orig$RefOrig<- str_replace(ref_orig$RefOrig, pattern=",", replace ="") 

ref_orig$RefOrig<- str_replace(ref_orig$RefOrig, pattern=",", replace ="") 
#ref_orig$PerOfPop <- as.double(ref_orig$PerOfPop)
ref_orig <- arrange(ref_orig, PerOfPop)
ref_orig
summarise(ref_orig, sum(PerOfPop))
#2
# #ref_num <- read_csv("unhcr_asylum_seekers.csv",skip=2, col_names=TRUE, col_types = "cccciiiiiiii")
# ref_num <- read_csv("unhcr_asylum_seekers.csv", col_names=TRUE, col_types= cols( Country= col_character(),)
# names(ref_num)
# dim(ref_num)
# str(ref_num)
# glimpse(ref_num)
# names(ref_num)= c("Year", "CtryAsylum", "Orig", "RSD","TotalStY","AppDY","DecRec", "DecOth", "Rejected", "OthClos", "TotDec", "TotEndY" )

names(ref_num)

ref_num <- select(ref_num, one_of("CtryAsylum", "Orig", "TotalStY", "DecRec", "DecOth", "Rejected", "TotDec"))

glimpse(ref_num)

ref_num$Rejected = as.numeric(ref_num$Rejected)
ref_num$TotalStY = as.numeric(ref_num$TotalStY)
glimpse(ref_num)
#data.frame(ref_num[-1], comp = complete.cases(ref_num))
#ref_num <- filter(ref_num, complete.cases(ref_num) != FALSE)
GER <- filter(ref_num, CtryAsylum =="Germany") %>% group_by(Orig) %>% filter(Orig =="Afganistan") %>%summarise(sum(Rejected))
GER

#ref_num4 <- filter(ref_num, CtryAsylum %in% c("Germany")) %>% group_by(Orig) %>% summarise(sum(Rejected))

# ref_num_cho <- ref_num %>% filter(CtryAsylum %in% c("Germany", "Sweden", "Finland"))
# ref_num_cho %>% group_by(CtryAsylum) %>% summarise(sum(Rejected))
ref_num <- filter(ref_num, CtryAsylum %in% c("Germany") & Orig %in% c("Afganistan"))
ref_num <- group_by(ref_num, CtryAsylum) %>% summarise(sum(Rejected))
ref_num


ref_num_2 <- select(ref_num, one_of("Year", "CtryAsylum", "Orig", "TotalStY", "DecRec","DecOth", "Rejected", "TotDec"))
names(ref_num_2)
head(ref_num_2)
ref_num_2gr <- group_by(ref_num_2, CtryAsylum)
summarise(ref_num_2gr, max("Rejected"))

### JOIN DATAFRAMES
