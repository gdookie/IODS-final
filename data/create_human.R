# Author: Gyan Dookie
# Date: 24.02.2017
# Description: IODS Chapter 5 wrangling exercise. Here I'm combining the human development and
# gender inequlity datasets and thus creating the human dataset which is then further wrangled and
# finally saved as the human.csv file for further use.

# Read the “Human development” and “Gender inequality” datas into R
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# library(stringr)
library(stringr)
# load dplyr
library(dplyr)
# library(plyr) I chose another why to change the variable names, so I don't need the plyr package
# Explore the datasets
# Human development - structure and dimensions
str(hd)
dim(hd)

# Gender inequality - structure and dimensions
str(gii)
dim(gii)
# Create summaries of variables
# Summary of Human development data
summary(hd)
summary(gii)

#Look at the meta files and rename the variables with (shorter) descriptive names. (1 point)
# Current names of variables - Human development
names(hd)
str(hd)

# Change  Human Development dataframe variable names to the following: HumDevRank, Country, HumDevIndex, LifeExpBirth, EduExpYrs, EduMeanYrs, GNI, GNIMinusHDI
names(hd) <- c("HumDevRank", "Country", "HumDevIndex", "LifeExpBirth", "EduExpYrs", "EduMeanYrs", "GNI", "GNIMinusHDI")

# Double checking that the names were changed
names(hd)
str(hd)

# Current names of variables - Gender inequality
names(gii)

# Change the Gender Inequality dataframe variable names to the following: GendIneqRank, Country, GendIneqIndex, MatMortRat, AdolBirthRate, ParlPercRepres, EduSecondFemale, EduSecondMale, LabParticipFemale, LabParticipMale
names(gii) <- c("GendIneqRank", "Country", "GendIneqIndex", "MatMortRat", "AdolBirthRate", "ParlPercRepres", "EduSecondFemale", "EduSecondMale", "LabParticipFemale", "LabParticipMale")

# Double checking that the names were changed
names(gii)
str(gii)

#Mutate the “Gender inequality” data and create two new variables. The first one should be the ratio of Female and Male populations with secondary education in each country. (i.e. edu2F / edu2M). The second new variable should be the ratio of labour force participation of females and males in each country (i.e. labF / labM). (1 point)
gii <- mutate(gii, EduF2M = EduSecondFemale/EduSecondMale, LabF2M = LabParticipFemale/LabParticipMale)
names(gii)

#join together the two datasets using the variable Country as the identifier. Keep only the countries in both data sets (Hint: inner join). Call the new joined data human and save it in your data folder. (1 point)
# Here we join the two datasets by the selected identifier
#human <- inner_join(hd, gii, by = "Country", suffix=c(".hd", ".gii")) Let's not include the suffix argument, because it wasn't asked for in this exercise
human <- inner_join(hd, gii, by = "Country")

# Let's take a look at the new joined dataset to be sure everything is ok
dim(human)
glimpse(human)
names(human)
str(human)

# Mutate the data: transform the Gross National Income (GNI) variable to numeric (Using string manipulation. Note that the mutation of 'human' was not done on DataCamp).


# Here the comma character is removed from the GNI variable's values and then it's datatype is changed to numeric
human$GNI<- str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric()

str(human$GNI)
str(human)

# Now we can exclude unneeded variables and keep only the columns matching the following variable names: "Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F"

human <- dplyr::select(human, Country, EduSecondFemale, LabParticipFemale, EduExpYrs, LifeExpBirth, GNI,MatMortRat, AdolBirthRate, ParlPercRepres)

# Here I change the variable names I created earlier to shorter names (because I found out that long variable names are tedious to write and prone to typos later in the analysis exercise)
names(human) <- c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
str(human)
complete.cases(human)

#Remove all rows with missing values
data.frame(human[-1], comp = complete.cases(human))
human <- filter(human, complete.cases(human) != FALSE)

#Remove the observations which relate to regions instead of countries. 
last <- nrow(human) - 7
last
human <- human[1:last,]
dim(human)

# Define the row names of the data by the country names and remove the country name column from the data. The data should now have 155 observations and 8 variables (corrected 21.2.) . Save the human data in your data folder including the row names. You can overwrite your old ‘human’ data. (1 point)
rownames(human) <- human$Country
human


# remove the Country variable
# human <- select(human, EduSecondFemale, LabParticipFemale, EduExpYrs, LifeExpBirth, GNI, MatMortRat, AdolBirthRate, ParlPercRepres)
human <- select(human, -Country)

dim(human)
names(human)
str(human$GNI)
glimpse(human)

# Next we'll save the wrangled and modified data as a .csv file

write.csv(human, "/Users/gyandookie/IODS-project/data/human.csv", row.names = TRUE)



