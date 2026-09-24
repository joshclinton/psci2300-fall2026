# PSC 2300: Data and Politics I
# Session 6: Uncertainty: Is the Difference Real?
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Keep this script, CES2024_GenderGap.rds, athens.csv, and olympics.csv in the
# same folder. Then choose Session > Set Working Directory > To Source File Location.

# ---- Uncertainty in Univariate Statistics ----

# -- A Familiar Number: The Margin of Error --

library(tidyverse)

# ---- Return to the 2024 CES ----

ces_gender = readRDS("CES2024_GenderGap.rds")

glimpse(ces_gender)

ces_gender |>
  summarize(
    N = n(),
    PctHarris = mean(HarrisVoter)
  )

# -- Samples, Populations, and Uncertainty --

set.seed(42)

# -- How Does slice_sample() Work? --

ces_demo = ces_gender |>
  select(gender4, presvote) |>
  head(5) |>
  mutate(case_id = 1:n())

ces_demo

ces_demo |>
  slice_sample(n = 3)

ces_demo |>
  slice_sample(prop = .4)

ces_demo |>
  slice_sample(prop = 1)

ces_demo |>
  slice_sample(prop = 1, replace = TRUE)

# -- One Imaginary New Poll --

ces_gender |>
  slice_sample(prop = 1, replace = TRUE) |>
  summarize(PctHarris = mean(HarrisVoter))

# -- What Does a Loop Do? --

for (i in 1:5) {
  print(i)
}

# -- Do the Work Once Before Repeating It --

one_estimate = ces_gender |>
  slice_sample(prop = 1, replace = TRUE) |>
  summarize(PctHarris = mean(HarrisVoter))

one_estimate

# -- What Does bind_rows() Do? --

SampledHarrisSupport = NULL

SampledHarrisSupport = bind_rows(
  SampledHarrisSupport,
  one_estimate
)

SampledHarrisSupport

another_estimate = ces_gender |>
  slice_sample(prop = 1, replace = TRUE) |>
  summarize(PctHarris = mean(HarrisVoter))

SampledHarrisSupport = bind_rows(
  SampledHarrisSupport,
  another_estimate
)

SampledHarrisSupport

# -- Put the Steps Inside a Loop --

SampledHarrisSupport = NULL
for (i in 1:5) {

  one_estimate = ces_gender |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(
      Iteration = i,
      PctHarris = mean(HarrisVoter)
    )

  SampledHarrisSupport = bind_rows(
    SampledHarrisSupport,
    one_estimate
  )
}

SampledHarrisSupport

# -- Repeat the Poll 1,000 Times --

SampledHarrisSupport = NULL

for (i in 1:1000) {

  one_estimate = ces_gender |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(PctHarris = mean(HarrisVoter))

  SampledHarrisSupport = bind_rows(
    SampledHarrisSupport,
    one_estimate
  )
}

summary(SampledHarrisSupport)

# -- From a Margin of Error to an Interval --

newvar = seq(0, 1000)

median(newvar)
quantile(newvar, p = .5)

quantile(newvar, p = .25)
quantile(newvar, p = .75)

quantile(newvar, p = c(.25,.75))

quantile(newvar, p = c(.025,.975))

SampledHarrisSupport |>
  summarize(
    Lower = quantile(PctHarris, p = .025),
    Estimate = mean(PctHarris),
    Upper = quantile(PctHarris, p = .975),
    MarginOfError = (Upper - Lower) / 2
  )

# ---- A Harder Question: Is the Gender Gap "Real"? ----

ces_gender |>
  group_by(woman) |>
  summarize(
    N = n(),
    PctHarris = mean(HarrisVoter)
  )

Women = ces_gender |>
  filter(woman == 1) |>
  summarize(PctHarris = mean(HarrisVoter))

Men = ces_gender |>
  filter(woman == 0) |>
  summarize(PctHarris = mean(HarrisVoter))

GenderGap = Women$PctHarris - Men$PctHarris
GenderGap

# -- Bootstrap the Gender Gap --

SampledGenderGaps = NULL

for (i in 1:1000) {

  one_sample = ces_gender |>
    slice_sample(prop = 1, replace = TRUE)

  one_women = one_sample |>
    filter(woman == 1) |>
    summarize(PctHarris = mean(HarrisVoter))

  one_men = one_sample |>
    filter(woman == 0) |>
    summarize(PctHarris = mean(HarrisVoter))

  one_gap = tibble(
    GenderGap = one_women$PctHarris - one_men$PctHarris
  )

  SampledGenderGaps = bind_rows(SampledGenderGaps, one_gap)
}

summary(SampledGenderGaps)

SampledGenderGaps |>
  summarize(
    Lower = quantile(GenderGap, p = .025),
    Estimate = mean(GenderGap),
    Upper = quantile(GenderGap, p = .975)
  )

# -- ASIDE: What if our survey was smaller? --

ces_gender_small = ces_gender |>
    slice_sample(n = 1000, replace = FALSE)

# Now replicate the code from above using this smaller data.

# INSERT CODE HERE

# ---- Same Method, New Question ----

athens = read.csv("athens.csv") |>
  mutate(
    red_win = if_else(winner == "Red", 1, 0),
    blue_win = if_else(winner == "Blue", 1, 0)
)

# -- The Red-Blue Difference of Means --

athens |>
  summarize(
    RedWinRate = mean(red_win),
    BlueWinRate = mean(blue_win),
    Difference = RedWinRate - BlueWinRate
  )

# -- Bootstrap the Red-Blue Difference --

SampledDifferences = NULL

for (i in 1:1000) {

  one_difference = athens |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(
      Difference = mean(red_win) - mean(blue_win),
      N = n()
    )

  SampledDifferences = bind_rows(
    SampledDifferences,
    one_difference
  )
}

summary(SampledDifferences)

SampledDifferences |>
  summarize(
    Lower = quantile(Difference, p = .025),
    Estimate = mean(Difference),
    Upper = quantile(Difference, p = .975)
  )

# -- POSSIBLE ASIDE: Testing the Difference Against Zero --

ObservedDifference = athens |>
  summarize(Difference = mean(red_win) - mean(blue_win)) |>
  pull(Difference)

NullDifferences = NULL
fair_coin = tibble(red_win = c(0, 1))

for (i in 1:1000) {

  one_null = fair_coin |>
    slice_sample(n = nrow(athens), replace = TRUE) |>
    summarize(
      Difference = mean(red_win) - mean(1 - red_win)
    )

  NullDifferences = bind_rows(NullDifferences, one_null)
}

summary(NullDifferences)

NullDifferences |>
  summarize(
    p_value = mean(abs(Difference) >= abs(ObservedDifference))
  )

# -- What Happens When We Add More Data? --

olympics = read.csv("olympics.csv") 

olympics = olympics |>
  mutate(
    red_win = if_else(winner == "Red", 1, 0),
    blue_win = if_else(winner == "Blue", 1, 0)
  )

olympics |>
  group_by(year) |>
  summarize(
    N = n(),
    Difference = mean(red_win) - mean(blue_win)
  )

olympics |>
  summarize(
    RedWinRate = mean(red_win),
    BlueWinRate = mean(blue_win),
    Difference = RedWinRate - BlueWinRate
  )

AllYearsDifferences = NULL

for (i in 1:1000) {

  one_difference = olympics |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(
      Difference = mean(red_win) - mean(blue_win)
    )

  AllYearsDifferences = bind_rows(
    AllYearsDifferences,
    one_difference
  )
}

AllYearsDifferences |>
  summarize(
    Lower = quantile(Difference, p = .025),
    Estimate = mean(Difference),
    Upper = quantile(Difference, p = .975)
  )

# -- What if we looked by sport? --

olympics |>
  group_by(sport) |>
  summarize(
    RedWinRate = mean(red_win),
    BlueWinRate = mean(blue_win),
    Difference = RedWinRate - BlueWinRate
  )

# EXERCISE: Adapt the following code to compare differences by sport.
# THE CODE THAT FOLLOWS NEEDS TO BE CHANGED.

AllYearsBySportDifferences = NULL

for (i in 1:1000) {

  one_difference = olympics |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(
      Difference = mean(red_win) - mean(blue_win)
    )

  AllYearsDifferences = bind_rows(
    AllYearsDifferences,
    one_difference
  )
}

AllYearsDifferences |>
  summarize(
    Lower = quantile(Difference, p = .025),
    Estimate = mean(Difference),
    Upper = quantile(Difference, p = .975)
  )
