#' title: Assignment 5
#' author: Dylan McCombs
#' date: Winter 2016
#' DylanMcCombsAssignment5.R
# Github link: 

# Question 0
cat("Dylan McCombs, 1505117, dmccombs@ucsc.edu")

# Question 1
library(ggplot2)

 # part a
View(diamonds)
Q1.a <- ggplot(diamonds, aes(x=x*y*z, y=price), margins=T) + 
  geom_point(aes(color=clarity, size=carat), alpha=0.2) +
  scale_y_log10() + scale_x_log10() +
  scale_size(range = c(1, 8.5)) +
  scale_color_discrete(guide=guide_legend(override.aes=list(alpha=1))) +
  theme(axis.title=element_text(size=13)) +
  theme(axis.text=element_text(size=11)) +
  theme(legend.text=element_text(size=11))
Q1.a



 # part b

Q1.b <- ggplot(diamonds, aes(carat, fill=clarity)) + 
  geom_histogram(aes(y = ..density..), bins = 25) +
  labs(x="Carat", y="Density") +
  facet_grid(cut ~ ., margins=T) +
  scale_y_discrete(breaks=seq(0,12,4)) +
  theme(axis.title=element_text(size=13)) +
  theme(axis.text=element_text(size=11)) +
  theme(legend.text=element_text(size=11))
Q1.b

# ?facet_grid
# ?stat_bin


 # part c
Q1.c <- ggplot(diamonds, aes(x=cut, y=price)) +
  geom_violin() +
  geom_jitter(size=2.2, alpha=0.015)
Q1.c

# ?geom_jitter


# Question 3(really? Can you count, Curtis? This is problem 2)
 # part a
library(foreign)
library(dplyr)

org <- read.dta("/Users/DylanMccombs/Desktop/ECON 217/org_example.dta")

org_sub.2a <- org %>%
  group_by(year, month) %>%
    summarise(
      median_rw     = median(rw, na.rm=T),
      rw_1stQrt     = quantile(rw, 0.25, na.rm=T),
      rw_3rdQrt     = quantile(rw, 0.75, na.rm=T),
      rw_1stDec     = quantile(rw, 0.1, na.rm=T),
      rw_9thDec     = quantile(rw, 0.9, na.rm=T),
      total         = n()
    ) %>%
      mutate(
        Date = paste(year, month, "01", sep="-" ),
        Date = as.Date(Date, format="%Y-%m-%d")
      ) %>% 
        arrange(Date)

View(org_sub.2a)
# sum(org_sub$total): results in 1119754 obs., the same as the original
# dataset


Q2.a <- ggplot(org_sub.2a, aes(x=Date, y=median_rw)) +
  geom_line(size=0.7) +
  ylim(0, 50) +
  labs(x="Date", y="Median.RW") +
  geom_ribbon(aes(ymin = rw_1stQrt, ymax = rw_3rdQrt), alpha=.5) +
  geom_ribbon(aes(ymin = rw_1stDec, ymax = rw_9thDec), alpha=0.3) +
  theme(axis.title=element_text(size=13)) +
  theme(axis.text=element_text(size=11))
Q2.a

# ?geom_ribbon
# ?labs

 # part b
org_sub.2b <- org %>%
  group_by(year, month, educ) %>%
    summarise(
      median_rw     = median(rw, na.rm=T)
    ) %>%
     mutate(
       Date = paste(year, month, "01", sep="-" ),
       Date = as.Date(Date, format="%Y-%m-%d")
     ) %>% 
      arrange(Date)

View(org_sub.2b)

Q2.b <- ggplot(org_sub.2b, aes(x=Date, y=median_rw)) +
  geom_line(aes(color=educ), size=0.7) +
  theme(axis.title=element_text(size=13)) +
  theme(axis.text=element_text(size=11)) +
  labs(x="Date", y="Median.RW") +
  theme(legend.text=element_text(size=11)) +
  theme(plot.margin = unit(c(2, 0, 2, 0), "cm"))
Q2.b






