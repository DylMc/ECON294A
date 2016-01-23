#' ---
#'  title: Assignment 2 script
#' author: Dylan McCombs
#' date: Jan. 20, 2016
#' assignment: (Github Repo link)
#' ---

 # setting working directory for github
setwd("~/Desktop/ECON 294A/ECON294A/ECON 294A Assignments") 

############################################
# Question 0
DylanMcCombsAssignment2<-list(
  firstName = "Dylan",
  lastName = "McCombs",
  email = "dmccombs@ucsc.edu",
  studentID = 1505117
)


############################################
# Question 1
# (installed RCurl")
require(RCurl); # RCurl must be used to load the following dataset for Mac users
diamondsURL <- getURL("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/diamonds.CSV")
diamonds <- read.csv(  
  text = diamondsURL
)
rm(diamondsURL)
View(diamonds)

DylanMcCombsAssignment2$s1a <- nrow(diamonds) # counting observations
DylanMcCombsAssignment2$s1b <- ncol(diamonds) # counting columns
DylanMcCombsAssignment2$s1c <- names(diamonds) # names of columns
DylanMcCombsAssignment2$s1d <- summary(diamonds$price)

############################################
# Question 2
# Loading the TSV file
library(foreign)
NHIS_2007 <- read.table("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt",
                        header = TRUE) # ensures variable names are transfered over
View(NHIS_2007)

DylanMcCombsAssignment2$s2a <- nrow(NHIS_2007)
DylanMcCombsAssignment2$s2b <- ncol(NHIS_2007) 
DylanMcCombsAssignment2$s2c <- names(NHIS_2007)
DylanMcCombsAssignment2$s2d <- mean(NHIS_2007$weight) # mean weight
DylanMcCombsAssignment2$s2e <- median(NHIS_2007$weight)

hist(NHIS_2007$weight)
table(NHIS_2007$weight) # 996-999 is code for missing values
?ifelse # must use ifelse to create new column with NAs

## Creating a new column that satisfies the ifelse condition
NHIS_2007$weightnew <- ifelse(NHIS_2007$weight>=996, NA, NHIS_2007$weight)
View(NHIS_2007)

hist(NHIS_2007$weightnew) # distribution looks better now
table(NHIS_2007$weightnew)

# adjusted mean and median
DylanMcCombsAssignment2$s2f <- mean(NHIS_2007$weightnew, na.rm=TRUE)
DylanMcCombsAssignment2$s2g <- median(NHIS_2007$weightnew, na.rm=TRUE)

# subsetting the data separately for men and women:
NHIS_2007women <- subset(NHIS_2007, SEX==2)
NHIS_2007men <- subset(NHIS_2007, SEX==1)

# weight of women and men adjusted
DylanMcCombsAssignment2$s2h <- summary(NHIS_2007women$weightnew)
DylanMcCombsAssignment2$s2i <- summary(NHIS_2007men$weightnew)

############################################
# Question 3

# changing letters to factors to extract index numbers as values
vec <- c(letters,LETTERS)
vec
vec_factors <- as.factor(vec)
vec_factors
class(vec_factors)
unclass(vec_factors)
levels(vec_factors)
vec_numeric <- as.numeric(vec_factors)

even_vec_index <- vec_numeric[vec_numeric %% 2 == 0]
DylanMcCombsAssignment2$s3a <- even_vec_index


DylanMcCombsAssignment2$s3b <- 
  paste(vec[c(30,25,12)], collapse="") # collapses the quotation marks

# messing with the array function
arr <- array( c(letters,LETTERS), dim = c(3,3,3))
arr
DylanMcCombsAssignment2$s3c <- arr[1:3,1,2]
DylanMcCombsAssignment2$s3d <- arr[2,2,1:3]
DylanMcCombsAssignment2$s3d <- 
  paste(arr[1,2,1],arr[1,3,3],arr[3,1,2], sep="")

############################################
# Question 4
library(foreign)
Org <- read.dta("/Users/DylanMccombs/Desktop/ECON 217/org_example.dta")
View(Org)

sort(unique(Org$year))
sort(unique(Org$month))
sort(unique(Org$educ))

DylanMcCombsAssignment2$s4 <- 
  aggregate(Org$rw,list(year=Org$year,month=Org$month,
                        educ=Org$educ),mean,na.rm=TRUE)

save(DylanMcCombsAssignment2, file = 
       "/Users/DylanMccombs/Desktop/ECON 294A/ECON294A/ECON 294A Assignments/DylanMcCombsAssignment2.RData")
