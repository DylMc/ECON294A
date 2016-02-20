#' title: Assignment 4
#' author: Dylan McCombs
#' date: Winter 2016
#' DylanMcCombsAssignment4.R
# Github link: git@github.com:DylMc/ECON294A.git

# Question 0
print("Dylan McCombs, 1505117, dmccombs@ucsc.edu")


# Question 1
library(foreign)
flights <- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/flights.csv",
                    stringsAsFactors=F)
planes <- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/planes.csv",
                   stringsAsFactors=F)
weather <-read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/weather.csv",
                   stringsAsFactors=F)
airports <- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/airports.csv",
                     stringsAsFactors=F)
  # In order for csv data to be loaded, must change "blob" in URL (if present) to
  # "raw".

str(flights)
str(planes)
str(weather)
str(airports)

# Question 2
# converting "date" vars from character type to date 
flights$date <- as.Date(flights$date)
str(flights)
weather$date <- as.Date(weather$date)
str(weather)

# Question 3
airports <- airports %>% arrange(city) # SFO is SF airport; OAK is Oakland airport
flights.2a <- flights %>% filter(dest=="SFO" | dest=="OAK")
print(nrow(flights.2a)) # 3,508 obs.

flights.2b <- flights %>% mutate(tot_delay_time = dep_delay+arr_delay) %>%
  filter(tot_delay_time>=60) # total del. time equal to an hour or more
print(nrow(flights.2b)) # 22,755 observations


flights.2c <- flights %>% filter(dep_delay>0 & arr_delay>(2*dep_delay))
print(nrow(flights.2c)) # 13,424 observations

# Question 4
# 3 diff. ways to select "delay" vars. from flights df
?dplyr::select

flights.4a <- flights %>% select(contains("delay"))
flights.4b <- flights %>% select(6:7)
flights.4c <- flights %>% select(ends_with("delay"))

# Question 5
# 5a
flights %>% arrange(desc(dep_delay)) %>%
  head(5) %>% print() # flight #'s 1, 1740, 3786, 855, and 3859

# 5b
flights %>% mutate(catch_time = dep_delay-arr_delay) %>%
  arrange(-catch_time) %>% head(5) %>% print() 
     # flights that caught up the most in absolute time (catch_time)
     # are flight numbers 1669, 62, 632, 1, and 39


# Question 6
# adding new variables to the existing flights df:
flights <- flights %>% mutate(speed = dist/(time/60))
flights <- flights %>% mutate(delta = dep_delay-arr_delay)

# 6a
flights %>% arrange(-speed) %>% head(5) %>% print()
    # top 5 flights by speed: flight numbers 1646, 5229, 944, 4634, and 500 

# 6b
flights %>% arrange(-delta) %>% head(5) %>% print()
    # top 5 flights by time made up in flight (delta): flight numbers 2804, 1669,
    # 1552, 1712, 4591; these flight numbers match those found in 5b

# 6c
flights %>% arrange(delta) %>% head(5) %>% print()
    # top 5 flights by time lost in flight (delta): flight numbers 2216, 1944,
    # 113, 1418, and 2496

# Question 7
?dplyr::summarize

flights.7a <- flights %>% group_by(carrier) %>% 
  summarise(
    cancelled             = sum(cancelled),
    flight_count          = n(),
    percent_cancelled     = (cancelled/flight_count)*100,
    delta_min             = min(delta, na.rm=T),
    delta_1stQrt          = quantile(delta, 0.25, na.rm=T),
    delta_med             = median(delta, na.rm=T),
    delta_avg             = mean(delta, na.rm=T),
    delta_3rdQrt          = quantile(delta, 0.75, na.rm=T),
    delta_90thQnt         = quantile(delta, 0.90, na.rm=T),
    delta_max             = max(delta, na.rm=T)
  )

flights.7a %>%
  arrange(-percent_cancelled) %>%
  print()

cat( "The block of code on the assignment handout (which is updated using dplyr
      below) creates a data frame called 'day_delay' which filters through
      the original flights data set, removing any NAs from the dep_delay
      variable. Then, the df is grouped by date with two summary statistics
      being created: 'delay' (the mean dep_delay) and 'n' (counting the number
      of days with the same date). Lastly, the chain of functions filters the 
      new data frame by only grabbing observations that have more than days
      of the same date.")

day_delay <- flights %>% filter(!is.na(dep_delay)) %>%
  group_by(date) %>%
  summarise(
    delay = mean(dep_delay),
    n     = n()
  ) %>%
  filter(n > 10)

# Question 8
?dplyr::lag
day_delay <- day_delay %>%
  mutate(lag_delay = lag(delay)) %>% # extra column added to ensure lag() works
  mutate(lag_diff = delay - lag_delay) %>%
  arrange(-lag_diff)

day_delay %>% head(5) %>% print()


# Question 9
dest_delay <- flights %>%
  group_by(dest) %>%
  summarise(
    mean_arr_delay = mean(arr_delay, na.rm=T),
    flights        = n()
  )


# Question 10

# Question 11