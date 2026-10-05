# PSC 2300: Data and Politics I
# Session 6: Uncertainty: Is the Difference Real?
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Make sure the following are in the same folder and are named as follows:
#
# Session6_Resampling_GenderGap_RedBlue.R
# CES2024_GenderGap.rds
# athens.csv
# olympics.csv
#
# Then choose Session > Set Working Directory > To Source File Location


# Load the tidyverse package.
library(tidyverse)


# ---- 2. Return to the 2024 CES ----------------------------------------------

# Gender gap data from Session 3, saved as an .rds file.
ces_gender = readRDS("CES2024_GenderGap.rds")

glimpse(ces_gender)

# Mean of a 0/1 variable = proportion voting for Harris.
ces_gender |>
  summarize(
    N = n(),
    PctHarris = mean(HarrisVoter)
  )

# Always set a seed if you are sampling!
set.seed(42)


# ---- 3. How does slice_sample() work? ---------------------------------------

# First five respondents, plus an identification number.
ces_demo = ces_gender |>
  select(gender4, presvote) |>
  head(5) |>
  mutate(case_id = 1:n())

ces_demo

# n = how many rows to draw. Sampling without replacement.
ces_demo |>
  slice_sample(n = 3)

# prop = proportion of rows to draw. 40 percent of five rows is two.
ces_demo |>
  slice_sample(prop = .4)

# prop = 1 without replacement: same rows, new order.
ces_demo |>
  slice_sample(prop = 1)

# replace = TRUE: a row is put back and can be drawn again.
ces_demo |>
  slice_sample(prop = 1, replace = TRUE)

# One imaginary new poll.
ces_gender |>
  slice_sample(prop = 1, replace = TRUE) |>
  summarize(PctHarris = mean(HarrisVoter))


# ---- 4. Loops and bind_rows() -----------------------------------------------

# A loop repeats the code within the braces -- here 5 times.
for (i in 1:5) {
  print(i)
}

# Do the work once to make sure it works!
one_estimate = ces_gender |>
  slice_sample(prop = 1, replace = TRUE) |>
  summarize(PctHarris = mean(HarrisVoter))

one_estimate

# Begin with an empty object.
SampledHarrisSupport = NULL

# bind_rows() places the rows from one object underneath another.
SampledHarrisSupport = bind_rows(
  SampledHarrisSupport,
  one_estimate
)

SampledHarrisSupport

# Another estimate. Bind it to the first.
another_estimate = ces_gender |>
  slice_sample(prop = 1, replace = TRUE) |>
  summarize(PctHarris = mean(HarrisVoter))

SampledHarrisSupport = bind_rows(
  SampledHarrisSupport,
  another_estimate
)

SampledHarrisSupport

# Put the steps inside a loop. Start with five iterations.
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


# ---- 5. Repeat the poll 1,000 times -----------------------------------------

# Change only the number of repetitions.
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

# Each row is Harris support from one imaginary new poll.
summary(SampledHarrisSupport)


# ---- 6. From a margin of error to an interval -------------------------------

# A simple variable that ranges from 0 to 1000.
newvar = seq(0, 1000)

# The median two ways.
median(newvar)
quantile(newvar, p = .5)

# 25th and 75th percentiles: the inter-quartile range.
quantile(newvar, p = .25)
quantile(newvar, p = .75)

# Both at the same time.
quantile(newvar, p = c(.25,.75))

# The 95% interval.
quantile(newvar, p = c(.025,.975))

# Middle 95 percent of our resampled estimates.
SampledHarrisSupport |>
  summarize(
    Lower = quantile(PctHarris, p = .025),
    Estimate = mean(PctHarris),
    Upper = quantile(PctHarris, p = .975),
    MarginOfError = (Upper - Lower) / 2
  )


# ---- 7. A harder question: is the gender gap "real"? ------------------------

# Harris support among women and men.
ces_gender |>
  group_by(woman) |>
  summarize(
    N = n(),
    PctHarris = mean(HarrisVoter)
  )

# Gender gap = a difference of means. Recall how we computed it.
Women = ces_gender |>
  filter(woman == 1) |>
  summarize(PctHarris = mean(HarrisVoter))

Men = ces_gender |>
  filter(woman == 0) |>
  summarize(PctHarris = mean(HarrisVoter))

GenderGap = Women$PctHarris - Men$PctHarris
GenderGap

# Bootstrap the gender gap. Only the code within the loop changes.
# tibble() makes the gap an object we can add.
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

# Does the interval include zero?
SampledGenderGaps |>
  summarize(
    Lower = quantile(GenderGap, p = .025),
    Estimate = mean(GenderGap),
    Upper = quantile(GenderGap, p = .975)
  )

# ASIDE: What if our survey was smaller? Take just 1000 observations.
ces_gender_small = ces_gender |>
    slice_sample(n = 1000, replace = FALSE)

# Exercise: Now replicate the code from above using this smaller data.




# ---- 8. Same method, new question: does red beat blue? ----------------------

# Each row is one contest. red_win = 1 when red won; blue_win the reverse.
athens = read.csv("athens.csv") |>
  mutate(
    red_win = if_else(winner == "Red", 1, 0),
    blue_win = if_else(winner == "Blue", 1, 0)
)

# The mean of each indicator is the win rate for that color.
athens |>
  summarize(
    RedWinRate = mean(red_win),
    BlueWinRate = mean(blue_win),
    Difference = RedWinRate - BlueWinRate
  )

# Now resample/bootstrap the analysis.
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

# Does this interval include zero?
SampledDifferences |>
  summarize(
    Lower = quantile(Difference, p = .025),
    Estimate = mean(Difference),
    Upper = quantile(Difference, p = .975)
  )


# ---- 9. POSSIBLE ASIDE: testing the difference against zero -----------------

# Not covered, but included in case you are interested.
# Null hypothesis: each bout is like a fair coin flip.
ObservedDifference = athens |>
  summarize(Difference = mean(red_win) - mean(blue_win)) |>
  pull(Difference)

# Simulate 1,000 imaginary Olympics in which color has no advantage.
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

# How often does chance produce a difference this large?
NullDifferences |>
  summarize(
    p_value = mean(abs(Difference) >= abs(ObservedDifference))
  )


# ---- 10. What happens when we add more data? --------------------------------

# Olympics held between 1996 and 2020.
olympics = read.csv("olympics.csv")

olympics = olympics |>
  mutate(
    red_win = if_else(winner == "Red", 1, 0),
    blue_win = if_else(winner == "Blue", 1, 0)
  )

# Difference for each Olympic year.
olympics |>
  group_by(year) |>
  summarize(
    N = n(),
    Difference = mean(red_win) - mean(blue_win)
  )

# Difference using all of the contests.
olympics |>
  summarize(
    RedWinRate = mean(red_win),
    BlueWinRate = mean(blue_win),
    Difference = RedWinRate - BlueWinRate
  )

# Same bootstrap one more time.
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


# ---- 11. What if we looked by sport? ----------------------------------------

# Difference by sport.
olympics |>
  group_by(sport) |>
  summarize(
    RedWinRate = mean(red_win),
    BlueWinRate = mean(blue_win),
    Difference = RedWinRate - BlueWinRate
  )

# Exercise: Adapt the following code to compare differences by sport.
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
