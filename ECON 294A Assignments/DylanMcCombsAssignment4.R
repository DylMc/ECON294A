#' title: Assignment 4
#' author: Dylan McCombs
#' date: Winter 2016
#' DylanMcCombsAssignment4.R
# Github link: git@github.com:DylMc/ECON294A.git

# Question 0
cat("Dylan McCombs, 
    1505117, 
    dmccombs@ucsc.edu"
)


# Question 1
library(foreign)
library(dplyr)
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
      column. Then, the df is grouped by date with two summary statistics
      being created: 'delay' (the mean dep_delay) and 'n' (counting the number
      of days with the same date, which is just the total number of flights for
      that given day). Lastly, the chain of functions filters the 
      new data frame by only grabbing observations that have more than 10 
      flights of the same date.")

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

airports.9 <- airports %>% 
  select(-country) %>%
  rename(dest = iata, name = airport)

dest_delay <- dest_delay %>% arrange(dest)
airports.9 <- airports.9 %>% arrange(dest)

   # left_join: join matching rows from b to a.
df.9a <- left_join(dest_delay, airports.9, "dest")

df.9a %>% arrange(-mean_arr_delay) %>%
  head(5) %>% 
  select(state, city, mean_arr_delay) %>%
  print() # cities and states with the highest mean_arr_delay:
          # Anchorage, AK; Cedar Rapids, IA; Des Moines, IA; SF, CA; and 
          # Beaumont/Port Arthur, TX.
          # Obs. = 116


    # inner_join: join only rows present in both a and b.
df.9b <- inner_join(dest_delay, airports.9, "dest") # Obs. = 114
cat("The number of observations joining the two datasets using left_join do
    not match the number found in the new dataset using inner_join, which
    makes sense.")

df.9c <- right_join(dest_delay, airports.9, "dest")
cat("There are 3,376 observations in this new dataset; only 116 existed using
    left_join because the firt data frame (dest_delay) had 116 observations.
    Almost every observation in the new dataset has NA in the mean_arr_delay
    column using right_join. What I think is going on is that the bigger data
    frame is the airports one, and when we right_joined dest_delay to it only 
    116 mean_arr_delays were added (which came from the dest_delay data frame).")

df.9d <- full_join(dest_delay, airports.9, "dest")
cat("Just as I expected, the number of observations in this new dataset is more
    than both the number of observations in the first two datasets (when using 
    left_join and right_join individually). The new number is 3,378. Again,
    many NAs exist in this resulting dataset. full_join joins the two data frames
    by all rows; no rows are left out as is the case using left_ and right_join.
    This process does result in NAs, however."
)

# Question 10

hourly_delay <- flights %>% filter(!is.na(dep_delay)) %>%
  group_by(date, hour) %>%         # resulting data frame (w/o filtering n> 10 as in day_delay)
  summarise(                       # is 6,872.
    dep_delay = mean(dep_delay),
    n     = n()
  ) 

hourly_delay.10 <- inner_join(hourly_delay, weather) # natural join; date and hour are used
                                                     # in this case as they have common names
hourly_delay.10 %>% 
  arrange(-dep_delay) %>%
  select(dep_delay, conditions) %>%
  head(5) %>%
  tbl_df() # weather condition most related to biggest delays is some form of cloudiness.


# Question 11
# using both tidyr & dplyr

# 11a
df.11a <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df.11a

install.packages("tidyr")
library(tidyr)

?gather

df.11a <- df.11a %>% gather("subject", "value", 2:3) %>%
  select(subject, treatment, value) %>%
  mutate(subject = c(1,1,2,2))

df.11a

# 11b
df.11b <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"), value = c(3,4,5,6)
)

df.11b
?spread

df.11b <- df.11b %>% spread(subject, value)
df.11b



