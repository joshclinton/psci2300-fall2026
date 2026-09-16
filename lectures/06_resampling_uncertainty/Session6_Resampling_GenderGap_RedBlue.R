# PSC 2300: Data and Politics I
# Session 6: Uncertainty: Is the Difference Real?
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Keep this script, CES2024_GenderGap.rds, athens.csv, and olympics.csv in the
# same folder. Then choose Session > Set Working Directory > To Source File Location.

library(tidyverse)


# ---- 2. Harris support and resampling ---------------------------------------

ces_gender = readRDS("CES2024_GenderGap.rds")

glimpse(ces_gender)

ces_gender |>
  summarize(
    N = n(),
    PctHarris = mean(HarrisVoter)
  )

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

set.seed(1)

ces_demo |>
  slice_sample(prop = 1, replace = TRUE)

set.seed(42)

ces_gender |>
  slice_sample(prop = 1, replace = TRUE) |>
  summarize(PctHarris = mean(HarrisVoter))


# ---- 3. Build the bootstrap loop -------------------------------------------

for (i in 1:5) {
  print(i)
}

one_estimate = ces_gender |>
  slice_sample(prop = 1, replace = TRUE) |>
  summarize(PctHarris = mean(HarrisVoter))

one_estimate

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

SampledHarrisSupport = NULL
set.seed(2300)

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


# ---- 4. Bootstrap Harris support -------------------------------------------

SampledHarrisSupport = NULL
set.seed(2300)

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

SampledHarrisSupport |>
  summarize(
    Lower = quantile(PctHarris, .025),
    Estimate = mean(PctHarris),
    Upper = quantile(PctHarris, .975),
    MarginOfError = (Upper - Lower) / 2
  )


# ---- 5. Bootstrap the gender gap -------------------------------------------

ces_gender |>
  group_by(female) |>
  summarize(
    N = n(),
    PctHarris = mean(HarrisVoter)
  )

Women = ces_gender |>
  filter(female == 1) |>
  summarize(PctHarris = mean(HarrisVoter))

Men = ces_gender |>
  filter(female == 0) |>
  summarize(PctHarris = mean(HarrisVoter))

GenderGap = Women$PctHarris - Men$PctHarris
GenderGap

SampledGenderGaps = NULL
set.seed(2300)

for (i in 1:1000) {

  one_sample = ces_gender |>
    slice_sample(prop = 1, replace = TRUE)

  one_women = one_sample |>
    filter(female == 1) |>
    summarize(PctHarris = mean(HarrisVoter))

  one_men = one_sample |>
    filter(female == 0) |>
    summarize(PctHarris = mean(HarrisVoter))

  one_gap = tibble(
    GenderGap = one_women$PctHarris - one_men$PctHarris
  )

  SampledGenderGaps = bind_rows(SampledGenderGaps, one_gap)
}

summary(SampledGenderGaps)

SampledGenderGaps |>
  summarize(
    Lower = quantile(GenderGap, .025),
    Estimate = mean(GenderGap),
    Upper = quantile(GenderGap, .975)
  )


# ---- 6. Red versus blue in Athens ------------------------------------------

athens = read_csv("athens.csv") |>
  mutate(
    red_win = if_else(winner == "Red", 1, 0),
    blue_win = if_else(winner == "Blue", 1, 0)
  )

athens |>
  summarize(
    RedWinRate = mean(red_win),
    BlueWinRate = mean(blue_win),
    Difference = RedWinRate - BlueWinRate
  )

SampledDifferences = NULL
set.seed(2300)

for (i in 1:1000) {

  one_difference = athens |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(
      Difference = mean(red_win) - mean(blue_win)
    )

  SampledDifferences = bind_rows(
    SampledDifferences,
    one_difference
  )
}

summary(SampledDifferences)

SampledDifferences |>
  summarize(
    Lower = quantile(Difference, .025),
    Estimate = mean(Difference),
    Upper = quantile(Difference, .975)
  )


# ---- 7. Test against a fair-coin null --------------------------------------

ObservedDifference = athens |>
  summarize(Difference = mean(red_win) - mean(blue_win)) |>
  pull(Difference)

NullDifferences = NULL
fair_coin = tibble(red_win = c(0, 1))
set.seed(2300)

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


# ---- 8. Add the 1996-2020 Olympic data -------------------------------------

olympics = read_csv("olympics.csv") |>
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
set.seed(2300)

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
    Lower = quantile(Difference, .025),
    Estimate = mean(Difference),
    Upper = quantile(Difference, .975)
  )
