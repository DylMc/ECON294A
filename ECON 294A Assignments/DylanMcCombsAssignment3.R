#' title: Assignment 3
#' author: Dylan McCombs
#' date: Winter 2016
#' DylanMcCombsAssignment3.R
#' Github link: *******(Github Repository link)******


# Question 0
print("Dylan McCombs")
print("1505117")
print("dmccombs@ucsc.edu")

# Question 1
library(foreign)
df.ex<-read.dta(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
View(df.ex)

# Question 2: Filter (operations performed by rows)
install.packages("dplyr") # pkg allows for easy data frame manipulation
library(dplyr)
browseVignettes(package = "dplyr") # helpful documentation for this pkg

df.ex2a <- filter(df.ex, year==2013 & month==12) # filter is similar to subsetting
print(nrow(df.ex2a)) # observations in Dec.of 2013: n=13,261

df.ex2b <- filter(df.ex, year==2013 & (month==7 | month==8 | month==9))
   # condition: Summer of 2013 defined by the above three months
   # 2013 and month 7, or 2013 and month 8, or 2013 and month 9 must be programmed
print(nrow(df.ex2b)) # observations in Summer of 2013: n=39,657


# Question 3: Arrange (similar to regular sorting)
df.ex.3a <- arrange(df.ex, year, month)
   # sorts year ascendingly, and month within year

# Question 4: Select (keeping specified columns)
df.ex.4a <- select(df.ex, year:age) #keeping year through age columns
df.ex.4b <- select(df.ex, year, month, starts_with("i"))
   # keeping year, month, and those variables that start with "i"

print(distinct(select(df.ex, state)))
   # printing disctinct values of the "state" variable in original dataset


# Question 5: Mutate
stndz <- function(x){
  (x - mean(x, na.rm = T))  /  sd(x, na.rm = T)
} # creates a function that standardizes specified vectors, x
nrmlz <- function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
} # creates a function that normalizes specified vectors, x

df.ex.5a <- mutate(df.ex, rw.stndz = stndz(rw), rw.nrmlz = nrmlz(rw))
   # above code adds columns of different manipulations of rw
   # to original dataframe (both standardized and normalized rw)

df.ex.5b <- df.ex %>%
  group_by(year, month) %>%
  mutate(rw.stndz = stndz(rw), rw.nrmlz = nrmlz(rw), count = n())
   # above uses magrittr to chain commands to one data frame called df.ex.5b


# Question 6: Summarize
df.ex.6 <- df.ex %>%
  group_by(year, month, state) %>%
  summarise(
    rw_min = min(rw, na.rm = T),
    rw_Q1 = quantile(rw, 0.25, na.rm = T),
    rw_mean = mean(rw, na.rm = T),
    rw_median = median(rw, na.rm = T),
    rw_Q3 = quantile(rw, 0.75, na.rm = T),
    rw_max = max(rw, na.rm = T),
    count = n()
  )
   # the above code seemingly collapses by the year, month, and state 
   # level, and then performs the operations within that grouping.
   # In the resulting data frame, only one state for each month of the
   # year exists
summary(df.ex.6$rw_mean) # so I know what value should be the max rw_mean

print(df.ex.6 %>% ungroup() %>% arrange(desc(rw_mean)) %>% 
        select(year, month, state) %>% head(1))
   # above code prints the the year, month, and state combo that has
   # the highest mean real wage. The real wage is about 40.63 for this
   # observation.


# Question 7: Challenge question
str(df.ex$state) # state is a factor with 51 levels
df.ex$state_as_char <- as.character(df.ex$state)
str(df.ex$state_as_char) # now states are characters to be arranged
                         # alphabetically
df.ex.7a <- arrange(df.ex, year, month, desc(state_as_char))
