# PSC 2300: Data and Politics I
# Session 14: Workshop: The Mark of a Criminal Record
# What Can an Audit Experiment Tell Us About Discrimination?
# Prof. Josh Clinton
# Vanderbilt University
#

# ---- 0. Setup ---------------------------------------------------------------

# Make sure the following are in the same folder and are named as follows:
#
# Session14_AuditExperiments.R
# audit.csv
# largeaudit.csv
#
# Then choose Session > Set Working Directory > To Source File Location

library(tidyverse)

# ---- Load and inspect the Pager data -------------------------------------

# QUESTION: What do you expect one row to be? Which variables should identify the treatment, outcome, and race of the tester?

# ---- REVEAL ----

audit = read.csv("audit.csv")

glimpse(audit)


# ---- How many applications are in each group? ---------------------------

# QUESTION: Are the groups the same size? Why should we care before comparing their means?

# ---- REVEAL ----

audit |>
  count(Race, CriminalRecord)


# ---- What pattern do you expect? -----------------------------------------

# QUESTION: Write down the expected ordering before calculating anything.
#   - Which group should have the highest callback rate?
#   - Which group should have the lowest?
#   - If criminal records matter, what should we observe within each race?
#   - If race matters, what should we observe within each criminal-record
#     group?


# ---- 7. What is the "treatment"? --------------------------------------------

# QUESTION: Criminal record? Race? Both? What does each comparison tell us?

# QUESTION: How do we compute the Sample Average Treatment Effect (SATE)?




# ---- REVEAL ----

# Criminal record?
pager_rates = audit |>
  group_by(CriminalRecord) |>
  summarize(
    Applications = n(),
    CallbackRate = mean(Callback)
  )

pager_rates

# Race?
pager_rates = audit |>
  group_by(Race) |>
  summarize(
    Applications = n(),
    CallbackRate = mean(Callback)
  )

pager_rates

# Both?
pager_rates = audit |>
  group_by(Race, CriminalRecord) |>
  summarize(
    Applications = n(),
    CallbackRate = mean(Callback)
  )

pager_rates


# ---- 8. Plot the comparison we care about -----------------------------------

# QUESTION: Which of Race, CriminalRecord, and CallbackRate goes on the
#   x-axis, which gives the bar heights, and which splits the bars by color?
#
# ggplot(
#   pager_rates,
#   aes(x = ________, y = ________, fill = ________)
# ) +
#   geom_bar(stat = "identity", position = "dodge") + ...

# ---- REVEAL ----

ggplot(
  pager_rates,
  aes(x = , y = , fill = )
) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(
    limits = c(0, .4),
    breaks = seq(0, .4, by = .1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "How Do Callback Rates Vary by Race and Criminal Record?",
    subtitle = "Pager audit experiment, Milwaukee, 2001",
  ) +
  theme_bw()


# ---- 9. Percentage points or percent change? --------------------------------

# QUESTION: How do we calculate the SATE in percentage points? In percent?





# ---- REVEAL ----

# White applicants:
white_rates = pager_rates |>
  filter(Race == "White")

WhiteNoRecord = white_rates |>
  filter(CriminalRecord == "No record") |>
  pull(CallbackRate)

white_rates |>
  mutate(
    PercentagePointChange = 100 * (CallbackRate - WhiteNoRecord),
    PercentChange = 100 * (CallbackRate - WhiteNoRecord) / WhiteNoRecord
  )

# Black applicants.
black_rates = pager_rates |>
  filter(Race == "Black")

BlackNoRecord = black_rates |>
  filter(CriminalRecord == "No record") |>
  pull(CallbackRate)

black_rates |>
  mutate(
    PercentagePointChange = 100 * (CallbackRate - BlackNoRecord),
    PercentChange = 100 * (CallbackRate - BlackNoRecord) / BlackNoRecord
  )

# QUESTION: Which race has the larger percentage-point change? Which has the
#   larger relative change? Why can both answers be correct?
# QUESTION: Which summary better answers the question you care about?


# ---- 10. How certain are we about that ordering? ----------------------------

# QUESTION: Would that ordering appear in different samples produced by the
#   same process?

# QUESTION: How do we do this?

# ---- REVEAL ----

set.seed(42)

white_record = audit |>
  filter(Race == "White", CriminalRecord == "Record")

black_no_record = audit |>
  filter(Race == "Black", CriminalRecord == "No record")

pager_bootstrap = NULL

for (i in 1:1000) {
  sampled_white_record = white_record |>
    slice_sample(prop = 1, replace = TRUE)

  sampled_black_no_record = black_no_record |>
    slice_sample(prop = 1, replace = TRUE)

  one_difference = sampled_white_record |>
    summarize(
      Difference = mean(Callback) - mean(sampled_black_no_record$Callback)
    )

  pager_bootstrap = bind_rows(pager_bootstrap, one_difference)
}

# Uncertainty three ways.
summary(pager_bootstrap$Difference)

quantile(
  pager_bootstrap$Difference,
  probs = c(.025, .975)
)

pager_bootstrap |>
  summarize(
    ProbabilityWhiteRecordGreater = mean(Difference > 0)
  )

# QUESTION: Is the probability greater than 95 percent? Does the 95 percent
#   interval exclude zero?
# QUESTION: What does this tell us about sampling uncertainty?


# ---- 11. How far does Pager's finding travel? -------------------------------

# ---- 12. What would a broader study look like? ------------------------------

# Kline, Rose, and Walters sent more than 83,000 fictitious applications to
# more than 11,000 entry-level jobs at 108 large U.S. employers. Applicant
# characteristics were randomly assigned to resumes.

# QUESTION: Does that address the limits we identified in Pager's study?
#   Which ones? What limits remain?

# NOTE: This is not an exact replication. Race is signaled by names, resumes
# are fictitious, there is no criminal-record treatment, and the outcome is
# employer contact across many firms and jobs.


# ---- 13. What does the larger study cover? ----------------------------------


# ---- REVEAL ----

large_audit = read.csv("largeaudit.csv")

glimpse(large_audit)


# ---- 14. Does the extension find the same basic pattern? --------------------

# QUESTION: What is the treatment? What is the outcome?

# ---- REVEAL ----

large_rates = large_audit |>
  group_by(NameRace) |>
  summarize(
    Applications = n(),
    ContactRate = mean(Contact)
  )

large_rates

# Separately, so we can take the difference (and resample it later).
BlackRate = large_rates |>
  filter(NameRace == "Black") |>
  pull(ContactRate)

WhiteRate = large_rates |>
  filter(NameRace == "White") |>
  pull(ContactRate)

WhiteRate - BlackRate

# QUESTION: What do we observe? How many fewer employer contacts would a
#   difference of about .021 produce for 1,000 applications?


# ---- 15. Bootstrap: resample individual applications ------------------------

# QUESTION: Using the same bootstrap logic as before, what do we resample,
#   what do we calculate, and what do we save?

# ---- REVEAL ----

set.seed(42)

black_name_applications = large_audit |>
  filter(NameRace == "Black")

white_name_applications = large_audit |>
  filter(NameRace == "White")

application_bootstrap = NULL

for (i in 1:1000) {
  sampled_black = black_name_applications |>
    slice_sample(prop = 1, replace = TRUE)

  sampled_white = white_name_applications |>
    slice_sample(prop = 1, replace = TRUE)

  one_difference = sampled_white |>
    summarize(
      Difference = mean(Contact) - mean(sampled_black$Contact)
    )

  application_bootstrap = bind_rows(
    application_bootstrap,
    one_difference
  )
}

quantile(
  application_bootstrap$Difference,
  probs = c(.025, .975)
)

ggplot(application_bootstrap, aes(x = Difference)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(0, .04),
    labels = scales::percent_format(accuracy = .1)
  ) +
  labs(
    title = "What If We Resample Individual Applications?",
    subtitle = paste(nrow(application_bootstrap), "bootstrap samples"),
    x = "White-name minus Black-name contact rate",
    y = "Number of bootstrap samples"
  ) +
  theme_bw()


# ---- 16. Is the gap the same in every industry? -----------------------------

# The 108 employers come from 24 industries.

# QUESTION: Would you expect the gap to be the same at a car dealership and a
#   bank? Why or why not?

# ---- REVEAL ----

large_audit |>
  count(Industry)

two_industries = large_audit |>
  filter(Industry %in% c("Auto dealers and gas stations", "Banks"))

two_industries |>
  group_by(Industry, NameRace) |>
  summarize(
    Applications = n(),
    ContactRate = mean(Contact)
  )


# ---- 17. Does the pooled result describe every employer? --------------------

# QUESTION: Would every one of the 108 employers show the pooled gap? What
#   would you expect to see for a single employer?


# ---- REVEAL ----

# Replace 001 with the three-digit employer number you want
employer_choice = "Employer_001"

chosen_employer = large_audit |>
  filter(Employer == employer_choice)

chosen_employer |>
  count(NameRace)

# Its two contact rates and the difference.
chosen_employer_rates = chosen_employer |>
  group_by(NameRace) |>
  summarize(
    Applications = n(),
    ContactRate = mean(Contact)
  )

chosen_employer_rates

ChosenBlackRate = chosen_employer_rates |>
  filter(NameRace == "Black") |>
  pull(ContactRate)

ChosenWhiteRate = chosen_employer_rates |>
  filter(NameRace == "White") |>
  pull(ContactRate)

ChosenWhiteRate - ChosenBlackRate


# ---- 18. EXTRA: What else was randomized? -----------------------------------

# NameGender, Over40, AcademicClub, PoliticalClub, and LGBTQClub were also
# randomly assigned.

# QUESTION: Which of these signals changed contact rates, and by how much
#   compared to race?

# ---- REVEAL ----




# ---- 20. What can we conclude? ----------------------------------------------

# QUESTION: What does an audit experiment allow us to say about
#   discrimination, and how far does that conclusion travel?
# QUESTION: What can we now say? What can we still not say?
