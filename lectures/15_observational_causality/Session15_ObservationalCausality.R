# PSC 2300: Data and Politics I
# Session 15: Causality in Observational Studies
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Make sure the following are in the same folder and are named as follows:
#
# Session15_ObservationalCausality.R
# bundesliga.csv
# minwage.csv
# medicaid.csv
#
# Then choose Session > Set Working Directory > To Source File Location


# Load the tidyverse package.
library(tidyverse)


# ---- 2. Home-field advantage: the Bundesliga without fans -------------------

# Each row is one of the 306 matches of the 2019-20 season.
# Crowd: "Fans" before the COVID shutdown, "No fans" on or after May 16, 2020.
# HomeWin: 1 if the home team won. GoalDifference: home goals minus away goals.
bundesliga = read.csv("bundesliga.csv")

glimpse(bundesliga)

# A before-and-after comparison.
BundesligaComparison = bundesliga |>
  group_by(Crowd) |>
  summarize(
    Matches = n(),
    PctHomeWin = mean(HomeWin),
    AvgHomeGoals = mean(FTHG),
    AvgAwayGoals = mean(FTAG),
    AvgGoalDifference = mean(GoalDifference)
  )

BundesligaComparison

BundesligaComparison |>
  ggplot(aes(x = Crowd, y = PctHomeWin)) +
  geom_col() +
  labs(
    x = "Crowd Type",
    y = "Percent Matches won by home team",
    title = "Home Wins Before and After Bundesliga Play Resumed"
  ) +
  theme_bw()

# Exercise: Only 82 matches were played without fans. Resample the matches
# with fans and the matches without fans 1,000 times each, calculate the
# difference in the percentage of home wins each time, and report the 2.5th,
# 50th, and 97.5th percentiles. Does the interval include zero? Even if it
# does not, does that settle the causal question?




# ---- 3. Minimum wage: read in the data --------------------------------------

# Each row is a fast-food restaurant in NJ or PA surveyed before and after
# New Jersey's 1992 minimum-wage increase (Card and Krueger 1994).
# Treatment: "Minimum-wage increase (NJ)" or "No change (PA)".
# PctFullTimeBefore / PctFullTimeAfter: share of employees working full time.
# ChangePctFullTime: after minus before.
minwage = read.csv("minwage.csv")

glimpse(minwage)

# Did the treatment produce a change? Share of restaurants paying at least
# $5.05 before and after.
minwage |>
  mutate(
    PreAt505 = wageBefore >= 5.05,
    PostAt505 = wageAfter >= 5.05
  ) |>
  group_by(Treatment) |>
  summarize(
    PctAt505Before = mean(PreAt505),
    PctAt505After = mean(PostAt505)
  )

# Were New Jersey and Pennsylvania comparable before the increase?
minwage |>
  group_by(Treatment) |>
  summarize(
    Restaurants = n(),
    AvgWageBefore = mean(wageBefore),
    AvgEmploymentBefore = mean(TotalEmploymentBefore),
    PctFullTimeBefore = mean(PctFullTimeBefore)
  )


# ---- 4. Minimum wage: three comparisons -------------------------------------

# Method 1: Cross-section. Compare NJ and PA after the increase.
CrossSection = minwage |>
  group_by(Treatment) |>
  summarize(PctFullTimeAfter = mean(PctFullTimeAfter))

CrossSection

# Separate tibbles for the NJ and PA restaurants; we use each several times.
NJ = minwage |>
  filter(Treatment == "Minimum-wage increase (NJ)")

PA = minwage |>
  filter(Treatment == "No change (PA)")

# Method 2: Before-and-after. Compare NJ before and after the increase.
BeforeAfter = NJ |>
  summarize(
    Before = mean(PctFullTimeBefore),
    After = mean(PctFullTimeAfter),
    Change = mean(ChangePctFullTime)
  )

BeforeAfter

# Method 3: Difference-in-differences. Compare the change in NJ with the
# change in PA.
ChangesByState = minwage |>
  group_by(Treatment) |>
  summarize(Change = mean(ChangePctFullTime))

ChangesByState

# pull() returns the number itself rather than a one-row data frame.
NJChange = ChangesByState |>
  filter(Treatment == "Minimum-wage increase (NJ)") |>
  pull(Change)

PAChange = ChangesByState |>
  filter(Treatment == "No change (PA)") |>
  pull(Change)

MinimumWageDiD = NJChange - PAChange

MinimumWageDiD


# ---- 5. Minimum wage: how uncertain is the difference-in-differences? -------

# Resample the NJ and PA restaurants separately and calculate the
# difference-in-differences in each resample.
set.seed(42)
n_iterations = 500
MinimumWageDiDSamples = NULL

for (i in 1:n_iterations) {
  NJsample = NJ |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(Change = mean(ChangePctFullTime)) |>
    pull(Change)

  PAsample = PA |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(Change = mean(ChangePctFullTime)) |>
    pull(Change)

  Diff = NJsample - PAsample

  MinimumWageDiDSamples = c(MinimumWageDiDSamples, Diff)
}

quantile(MinimumWageDiDSamples, c(.025, .975))

# Visualizing the difference-in-differences (the plot shown in the lecture).
# pivot_longer() stacks the Before and After columns so ggplot can draw one
# line per state.
minwage |>
  group_by(Treatment) |>
  summarize(
    Before = mean(PctFullTimeBefore),
    After = mean(PctFullTimeAfter)
  ) |>
  pivot_longer(Before:After, names_to = "Period", values_to = "PctFullTime") |>
  mutate(Period = factor(Period, levels = c("Before", "After"))) |>
  ggplot(aes(x = Period, y = PctFullTime, group = Treatment,
             color = Treatment)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "",
    y = "Percent of employees working full time",
    color = "",
    title = "Employment Before and After the Minimum-Wage Increase"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()


# ---- 6. Medicaid expansion: read in the data --------------------------------

# Each row is a county (Clinton and Sances 2018).
# Treatment: "Expand Medicaid" or "Not Expand Medicaid" (the state's decision).
# pcti2013-pcti2015: percent insured. registration / turnout: percent of the
# voting-age population. dregistration20162012, dturnout20162012: 2016 minus
# 2012. Some counties are missing values, so the code uses na.rm = TRUE.
medicaid = read.csv("medicaid.csv")

glimpse(medicaid)

# Were expansion and non-expansion counties comparable before treatment?
medicaid |>
  group_by(Treatment) |>
  summarize(
    n = n(),
    PctPoverty = mean(pctpoverty),
    Pct65Plus = mean(pct65plus),
    PctHighSchool = mean(pcths),
    MedianIncomeLogged = mean(lmedianincome),
    PresDem2012 = mean(PresDem2012, na.rm = TRUE)
  )

# Did expansion change insurance coverage? (The first stage.)
medicaid |>
  group_by(Treatment) |>
  summarize(
    Coverage2013 = mean(pcti2013, na.rm = TRUE),
    Coverage2014 = mean(pcti2014, na.rm = TRUE),
    Coverage2015 = mean(pcti2015, na.rm = TRUE)
  )


# ---- 7. Medicaid expansion: three comparisons for registration -------------

# Method 1: Cross-section.
medicaid |>
  group_by(Treatment) |>
  summarize(
    n = n(),
    Registration2016 = mean(registration2016, na.rm = TRUE)
  )

# Separate tibbles for expansion and non-expansion counties.
Expansion = medicaid |>
  filter(Treatment == "Expand Medicaid")

NonExpansion = medicaid |>
  filter(Treatment == "Not Expand Medicaid")

# Method 2: Before and after.
Expansion |>
  summarize(
    Registration2012 = mean(registration2012, na.rm = TRUE),
    Registration2016 = mean(registration2016, na.rm = TRUE),
    Change = mean(dregistration20162012, na.rm = TRUE)
  )

# Method 3: Difference-in-differences.
RegistrationChanges = medicaid |>
  group_by(Treatment) |>
  summarize(Change = mean(dregistration20162012, na.rm = TRUE))

RegistrationChanges

ExpansionChange = RegistrationChanges |>
  filter(Treatment == "Expand Medicaid") |>
  pull(Change)

NonExpansionChange = RegistrationChanges |>
  filter(Treatment == "Not Expand Medicaid") |>
  pull(Change)

RegistrationDiD = ExpansionChange - NonExpansionChange

RegistrationDiD


# ---- 8. Medicaid expansion: uncertainty in the difference-in-differences ----

set.seed(43)
n_iterations = 500
RegistrationDiDSamples = NULL

for (i in 1:n_iterations) {
  ExpansionSample = Expansion |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(Change = mean(dregistration20162012, na.rm = TRUE)) |>
    pull(Change)

  NonExpansionSample = NonExpansion |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(Change = mean(dregistration20162012, na.rm = TRUE)) |>
    pull(Change)

  Diff = ExpansionSample - NonExpansionSample

  RegistrationDiDSamples = c(RegistrationDiDSamples, Diff)
}

quantile(RegistrationDiDSamples, c(.025, .975))


# ---- 9. Other political outcomes --------------------------------------------

# The same design applied to turnout and Democratic presidential support.
medicaid |>
  group_by(Treatment) |>
  summarize(
    RegistrationChange = mean(dregistration20162012, na.rm = TRUE),
    TurnoutChange = mean(dturnout20162012, na.rm = TRUE),
    DemocraticSupportChange = mean(PresDem2016 - PresDem2012, na.rm = TRUE)
  )

# Exercise: Calculate the difference-in-differences for turnout the same way
# we did for registration. Is it larger or smaller than the registration
# effect? What would you need to assume to interpret it causally?



# Exercise: Bootstrap the turnout difference-in-differences and report a 95
# percent interval. Does it include zero?

