# PSC 2300: Data and Politics I
# Session 13: Causality and Randomized Experiments
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Make sure the following are in the same folder and are named as follows:
#
# Session13_RandomizedExperiments.R
# social.csv
#
# Then choose Session > Set Working Directory > To Source File Location


# Load the tidyverse package.
library(tidyverse)


# ---- 2. Social pressure experiment: read in the data -----------------------

# Each row is a registered voter in Michigan, 2006 primary.
# messages: the postcard received (Civic Duty, Control, Neighbors, Hawthorne).
social = read.csv("social.csv")

glimpse(social)

social |>
  count(messages) |>
  mutate(Pct = n / sum(n))


# ---- 3. Did randomization work? ---------------------------------------------

# Compare past turnout (2004 primary) across the groups.
social |>
  group_by(messages) |>
  summarize(PctVote2004Primary = mean(primary2004, na.rm = TRUE))

# Exercise: Check randomization of another variable. Why is it important to
# check that?




# ---- 4. Average turnout by group --------------------------------------------

AvgTurnout = social |>
  group_by(messages) |>
  summarize(AvgVote2006 = mean(primary2006, na.rm = TRUE))

AvgTurnout

# Plotting a statistic rather than raw data: stat = "identity".
AvgTurnout |>
  ggplot(aes(x = messages, y = AvgVote2006)) +
  geom_bar(stat = "identity") +
  labs(x = "Postcard Message",
       y = "Percent Voting in 2006 Primary",
       title = "Social Pressure Experiment: Michigan 2006 Primary") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()


# ---- 5. Calculate the treatment effects (SATE) ------------------------------

# Subtract the Control group average from each group.
# pull() returns the number; select() would keep a one-row data frame.
ControlTurnout = AvgTurnout |>
  filter(messages == "Control") |>
  pull(AvgVote2006)

TreatmentEffects = AvgTurnout |>
  mutate(SATE = AvgVote2006 - ControlTurnout)

TreatmentEffects

# Percent change relative to the Control baseline.
AvgTurnout |>
  mutate(PctChange = 100 * (AvgVote2006 - ControlTurnout) / ControlTurnout)

# Exercise: Instead of printing a table to the screen, create a visualization
# of the effect. Does it make sense to drop the control group?



# One way to do this is:
TreatmentEffects |>
  filter(messages != "Control") |>
  ggplot(aes(x = reorder(messages, SATE), y = SATE)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(
    x = "Postcard message",
    y = "Percentage Point Change in turnout relative to control",
    title = "Estimated Effect of Each Postcard Message"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()


# ---- 6. Are the differences statistically distinguishable? Resample --------

set.seed(42)
n_iterations = 1000
Control_means = NULL
CivicDuty_means = NULL
Hawthorne_means = NULL
Neighbors_means = NULL

for (i in 1:n_iterations) {
  sampC = social |>
    filter(messages == "Control") |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(AvgC = mean(primary2006))

  Control_means = bind_rows(Control_means, sampC)

  sampCD = social |>
    filter(messages == "Civic Duty") |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(AvgCD = mean(primary2006))

  CivicDuty_means = bind_rows(CivicDuty_means, sampCD)

  sampH = social |>
    filter(messages == "Hawthorne") |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(AvgH = mean(primary2006))

  Hawthorne_means = bind_rows(Hawthorne_means, sampH)

  sampN = social |>
    filter(messages == "Neighbors") |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarize(AvgN = mean(primary2006))

  Neighbors_means = bind_rows(Neighbors_means, sampN)
}

# bind_cols() puts the four sets of means side by side.
SampledData = bind_cols(Control_means, CivicDuty_means,
                        Hawthorne_means, Neighbors_means)
glimpse(SampledData)

# SATE for each resample.
SampledData = SampledData |>
  mutate(SATE_CD = AvgCD - AvgC,
         SATE_H = AvgH - AvgC,
         SATE_N = AvgN - AvgC)

glimpse(SampledData)

# Quantify the uncertainty.
SummarySATE = SampledData |>
  summarize(CD_SATE = mean(SATE_CD),
            p025_CD = quantile(SATE_CD, .025),
            p975_CD = quantile(SATE_CD, .975),
            H_SATE = mean(SATE_H),
            p025_H = quantile(SATE_H, .025),
            p975_H = quantile(SATE_H, .975),
            N_SATE = mean(SATE_N),
            p025_N = quantile(SATE_N, .025),
            p975_N = quantile(SATE_N, .975))

SummarySATE

# Probability the Hawthorne effect is bigger than the Civic Duty effect.
SampledData |>
  mutate(HbiggerCD = if_else(SATE_H > SATE_CD, 1, 0)) |>
  summarize(PctGreater = mean(HbiggerCD))

# Exercise: What is the probability that the percentage point change as a
# result of social messages is greater than 8 percentage points?



