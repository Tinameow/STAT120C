##########################################
# CODE for HW 3 Problem 4
#   STAT 120 C, Pluta, UCI Spring 2019
##########################################

# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("readr")
# install.packages("stringr")
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

### Read in iron data
dat_raw <- 
  read_csv(url("https://raw.githubusercontent.com/dspluta/STAT120C/master/Data/Chapter%2012/iron.csv"))

### Look at head (first 6 rows) and tail (last 6 rows) of data frame

head(dat_raw)
tail(dat_raw)

### Transform to long form
dat <- dat_raw %>% 
  gather(key = "trt", value = "Y") %>% 
  mutate(form = stringr::str_split(trt, " ", simplify = TRUE)[, 1],
         dose = stringr::str_split(trt, " ", simplify = TRUE)[, 2])
dat

# Marginal mean and sd of retention by form
dat %>% group_by(form) %>% summarize(mean_Y = mean(Y), sd_Y = sd(Y))

# Marginal mean and sd retention by dose
dat %>% group_by(dose) %>% summarize(mean_Y = mean(Y), sd_Y = sd(Y))

# Cell means and sd
dat %>% group_by(form, dose) %>% summarize(mean_Y = mean(Y), sd_Y = sd(Y))

### Create Boxplots
ggplot(dat) + 
  geom_boxplot(aes(x = trt, y = Y, group = trt, fill = trt), alpha = 0.5)

# Alternative way
ggplot(dat) + 
  geom_boxplot(aes(x = form, y = Y, group = form, fill = form), alpha = 0.5) + 
  facet_grid(cols = vars(dose))
