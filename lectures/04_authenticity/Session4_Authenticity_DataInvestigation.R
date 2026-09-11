# PSC 2300: Data and Politics I
# Lecture 4: Should We Believe the Claim?
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Make sure the following are in the same folder and are named as follows:
#
# Session4_Authenticity_DataInvestigation.R
# GKG_Study4_Class.csv
#
# Then choose Session > Set Working Directory > To Source File Location


# Load the tidyverse package.
# Replace package_name with the library used in Lectures 2 and 3.

# library(package_name)

library(tidyverse)
dat = read.csv("GKG_Study4_Class.csv")

glimpse(dat)

# INSERT CODE HERE

dat |> 
  count(condition)

dat |>
  filter(condition == "ProAttitudinal") |>
  summarize(AvgClean = mean(av_products_clean, na.rm=TRUE),
            AvgNeutral = mean(av_products_neutral, na.rm=TRUE),
            n = n())

# Table 3

dat |>
  group_by(condition) |>
  summarize(AvgClean = mean(av_products_clean, na.rm=TRUE),
            AvgNeutral = mean(av_products_neutral, na.rm=TRUE),
            n = n())

# Let's go

View(dat)
dat |> count(yearSchool)

dat = dat |>
  mutate(Weird = if_else(yearSchool == "harvard" | yearSchool == "Harvard",1,0))

dat |> count(Weird)
table(dat$Weird)

dat |>
  summarize(AvgClean = mean(av_products_clean, na.rm=TRUE),
            AvgNeutral = mean(av_products_neutral, na.rm=TRUE))

dat |>
  filter(Weird == 0) |>
  summarize(AvgClean = mean(av_products_clean, na.rm=TRUE),
            AvgNeutral = mean(av_products_neutral, na.rm=TRUE))

dat |>
  filter(Weird == 1) |>
  summarize(AvgClean = mean(av_products_clean, na.rm=TRUE),
            AvgNeutral = mean(av_products_neutral, na.rm=TRUE),
            n = n())

dat |>
  group_by(Weird) |>
  summarize(AvgClean = mean(av_products_clean, na.rm=TRUE),
            AvgNeutral = mean(av_products_neutral, na.rm=TRUE),
            n = n())


# Count weird and non-weird observations by Dissonance.
dat |> 
  count(condition,Weird)


dat |>
  group_by(condition,Weird) |>
  summarize(AvgClean = mean(av_products_clean, na.rm=TRUE),
            AvgNeutral = mean(av_products_neutral, na.rm=TRUE),
            n = n())

dat |>
  filter(Weird == 0) |>
  group_by(condition) |>
  summarize(AvgClean = mean(av_products_clean, na.rm=TRUE),
            AvgNeutral = mean(av_products_neutral, na.rm=TRUE),
            n = n())

dat |>
  filter(Weird == 1) |>
  group_by(condition) |>
  summarize(AvgClean = mean(av_products_clean, na.rm=TRUE),
            AvgNeutral = mean(av_products_neutral, na.rm=TRUE),
            n = n())

