#' title: Assignment 1
#' author: Dylan McCombs
#' date: Winter 2016
#' assignment: (Github Repository link)


# Question 0
firstname<- "Dylan"
lastname<- "McCombs"

print(paste(firstname, lastname))
studentID<-"1505117"
print(studentID)

# Question 1
# Loading different types of data files
library(foreign)
df.dta<-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")
df.csv<-read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")
df.td<-read.table("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")
df.rdata<-load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))

# Question 2
print("The .dta file is 193 KB.")
print("The .csv file is 142 KB.")
print("The .txt file is also 142 KB.")
print("The .RData file is 46 KB.")
print("The smallest is the .RData file.")
print("I believe that what accounts for the file size 
variability is the fact that the data in each is structured differently somehow.")

# Question 3
typeof(NHIS_2007_RData)
class(NHIS_2007_RData)
print("For the NHIS_2007_RData, typeof is list and the class is a data.frame")
print(length(NHIS_2007_RData))
print(dim(NHIS_2007_RData))
print(nrow(NHIS_2007_RData))
print(ncol(NHIS_2007_RData))
print(summary(NHIS_2007_RData))

# Question 4
# Loading a dta file and finding basic summary stats
df<-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
print(str(df))
print("1119754 obs. and  30 variables")
summary(df$rw)
print("min_rw=1.8, mean_rw=19.8 median_rw=15.9 max_rw=354.8")
print("firstQtl_rw=10.7 thirdQtl_rw=24.4")
print("There are 521279 NA's for the variable rw")

# Question 5
# Creating a vector and reporting the mean while removing "NA"
v<-c(1,2,3,4,5,6,7,4,NULL,NA)
summary(v)
print(length(v))
print("The number of values in the vector v don't match the number
reported using the length command because the word NULL is treated as
non-existant in R.")
print(mean(v, na.rm=TRUE))
print("The mean while ignoring the NA is 4.")


# Question 6
# Creating a 3x3 matrix and transposing it
# Also, finding the eigenvectors and eigenvalues
x<-matrix(c(1,2,3,4,5,6,7,8,9), ncol=3, byrow=TRUE)
x
print("To find a matrix transpose, use the following: t(x).")
t(x)

eigen(x)
print(eigen(x)$vec)
print(eigen(x)$val)

# Another matrix, and solving for it via inverse
y<-matrix(c(1,2,3,3,2,1,2,3,0), ncol=3, byrow=TRUE)
y
y_inv<-solve(y)
y_inv
y_inv%*%y # Matrix multiplication
print("The new matrix as a result of the original being multiplied
by its inverse is called the identity matrix.")

# Question 7
# Creating a dataframe from scratch via vectors
carat<-c(5,2,0.5,1.5,5,NA,3)
cut<-c("fair", "good", "very good", "good", "fair", "Ideal", "fair")
clarity<-c("SI1", "I1", "VI1", "VS1", "IF", "VVS2", NA)
price<-c(850,450,450,NA,750,980,420)

diamonds<-data.frame(carat, cut, clarity, price)
print(diamonds)

print(mean(price, na.rm=TRUE))
print("The mean price is 650")

cut_fair<-subset(diamonds, cut=="fair")
cut_fair
mean(cut_fair$price)
print("The mean price of cut 'fair' is 673.3333.")

cut_gVgI<-subset(diamonds, cut!="fair")
cut_gVgI
mean(cut_gVgI$price, na.rm=TRUE)
print("The mean price of cut 'good', 'very good', and 'Ideal' is 626.6667.")

median_price_subset<-subset(diamonds, carat>2&(cut=="Ideal"|cut=="very good"))
median_price_subset
print("The median price for diamonds with greater than 2 carats, and cut 'Ideal' 
or 'very good' is zero; no diamonds in the dataframe meet this condition.")

