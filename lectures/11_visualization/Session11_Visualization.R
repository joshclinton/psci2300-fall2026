# PSC 2300: Data and Politics I
# Session 11: Visualizing With the Grammar of Graphics
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Keep this script, insurance.csv, polls2020.rds, and CES2024_GenderGap.rds in
# the same folder. Then choose Session > Set Working Directory > To Source File Location.

# ---- Application 1: Insurance Mileage ----

# -- What is our question? --

library(tidyverse)

insurance = read.csv("insurance.csv")

glimpse(insurance)

# -- Start with a numeric summary --

insurance |>
  select(miles_driven) |>
  summary()

# -- The plot we use depends on the type of data we have! --

# - Build the graph in stages -

insurance |>
  ggplot(aes(x = miles_driven))

insurance |>
  ggplot(aes(x = miles_driven)) +
  geom_histogram()

insurance |>
  ggplot(aes(x = miles_driven)) +
  geom_histogram(bins = 25, color = "black", fill = "gold")

# - Label what the graph means -

mileage_plot = insurance |>
  ggplot(aes(x = miles_driven)) +
  geom_histogram(bins = 25, color = "black", fill = "gold") +
  labs(
    title = "Reported Miles Driven for Car 1",
    x = "Updated Odometer minus Baseline Odometer",
    y = "Number of Policies"
  ) +
  scale_x_continuous(breaks = seq(0, 50000, by = 10000)) +
  theme_bw()

mileage_plot

# -- Do the reported numbers look like numbers people report? --

insurance |>
  count(baseline_last_digit)

insurance |>
  ggplot(aes(x = baseline_last_digit)) +
  geom_bar(color = "black", fill = "gray70") +
  labs(
    title = "Last Digit of the Baseline Odometer Reading",
    x = "Last Digit",
    y = "Number of Policies"
  ) +
  scale_x_continuous(breaks = 0:9) +
  theme_bw()

# -- One more piece of information --

insurance |>
  filter(font == "Calibri") |>
  ggplot(aes(x = baseline_last_digit)) +
  geom_bar(color = "black", fill = "gray70") +
  labs(
    title = "Last Digit of Baseline Mileage: Calibri Rows",
    x = "Last Digit",
    y = "Number of Policies"
  ) +
  scale_x_continuous(breaks = 0:9) +
  theme_bw()

insurance |>
  filter(font == "Cambria") |>
  ggplot(aes(x = baseline_last_digit)) +
  geom_bar(color = "black", fill = "gray70") +
  labs(
    title = "Last Digit of Baseline Mileage: Cambria Rows",
    x = "Last Digit",
    y = "Number of Policies"
  ) +
  scale_x_continuous(breaks = 0:9) +
  theme_bw()

insurance |>
  ggplot(aes(x = baseline_last_digit)) +
  geom_bar(color = "black", fill = "gold") +
  facet_wrap(~font) +
  labs(
    title = "Last Digit of Baseline Mileage by Font",
    x = "Last Digit",
    y = "Number of Policies"
  ) +
  scale_x_continuous(breaks = 0:9) +
  theme_bw()

# ---- Application 2: 2020 Election Polls ----

Pres2020.PV = readRDS(file = "polls2020.rds")
glimpse(Pres2020.PV)

# -- What is our question? --

Pres2020.PV = Pres2020.PV |>
  mutate(
    margin = Biden - Trump,
    biden_error = Biden - DemCertVote
  )

# -- What did the polls show? --

poll_margin_plot = Pres2020.PV |>
  ggplot(aes(x = margin)) +
  geom_histogram(bins = 10, color = "black", fill = "gold") +
  geom_vline(xintercept = 0, linewidth = 1) +
  labs(
    title = "Margins in 2020 National Popular Vote Polls",
    x = "Biden Percentage minus Trump Percentage",
    y = "Number of Polls"
  ) +
  theme_bw()

poll_margin_plot

# -- Density --

Pres2020.PV |>
  ggplot(aes(x = biden_error)) +
  geom_density(color = "black", linewidth = 1) +
  geom_vline(xintercept = 0, linewidth = 1) +
  labs(
    title = "Density of Biden Polling Error",
    x = "Biden Poll Percentage minus Certified Vote Percentage",
    y = "Density"
  ) +
  theme_bw()

# ---- Conditional Relationships ----

# -- Discrete Variable By Discrete Variable (Barplot) --

# - Each observation is a survey respondent -

library(scales)

ces_gender = readRDS("CES2024_GenderGap.rds")

ces_gender = ces_gender |>
  mutate(
    age_group = case_when(
      age < 30 ~ "18–29",
      age < 65 ~ "30–64",
      age >= 65 ~ "65+"
    )
  )

glimpse(ces_gender)

ces_gender |>
  ggplot() +
  geom_bar(aes(x = presvote), color = "black") +
  labs(
    title = "Reported 2024 Presidential Vote",
    x = "Candidate",
    y = "Number of Respondents"
  ) +
  theme_bw()

# - Adding a second variable using fill -

ces_gender |>
  ggplot() +
  geom_bar(
    aes(x = presvote, fill = gender2),
    color = "black",
    position = "dodge"
  ) +
  labs(
    title = "Reported 2024 Presidential Vote by Gender",
    x = "Candidate",
    y = "Number of Respondents",
    fill = "Gender"
  ) +
  theme_bw()

# - Counts or proportions? -

ces_gender |>
  ggplot() +
  geom_bar(
    aes(x = gender2, fill = presvote),
    color = "black",
    position = "fill"
  ) +
  labs(
    title = "Reported 2024 Presidential Vote Within Gender",
    x = "Gender",
    y = "Percentage of Respondents",
    fill = "Reported Vote"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Harris" = "blue", "Trump" = "red")) +
  theme_bw()

# - Adding another comparison using facet_wrap -

ces_gender |>
  drop_na(age_group) |>
  ggplot() +
  geom_bar(
    aes(x = gender2, fill = presvote),
    color = "black",
    position = "fill"
  ) +
  labs(
    title = "Reported 2024 Presidential Vote by Gender and Age",
    x = "Gender",
    y = "Percentage of Respondents",
    fill = "Reported Vote"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Harris" = "blue", "Trump" = "red")) +
  facet_wrap(~age_group) +
  theme_bw()

# -- Continuous Variable By Discrete Variable --

# - Each observation is now a poll -

Pres2020.PV = readRDS(file = "polls2020.rds")

Pres2020.PV = Pres2020.PV |>
  mutate(
    EndDate = as.Date(EndDate, "%m/%d/%Y"),
    Trump = Trump / 100,
    Biden = Biden / 100,
    margin = Biden - Trump,
    actual_margin = (DemCertVote - RepCertVote) / 100,
    margin_error = margin - actual_margin,
    absolute_error = abs(margin_error)
  )

glimpse(Pres2020.PV)

Pres2020.PV |>
  count(Mode, sort = TRUE)

# - Boxplots -

common_modes = c("Online", "Live phone - RDD", "IVR/Online")

Pres2020.PV |>
  filter(Mode %in% common_modes) |>
  ggplot() +
  geom_boxplot(aes(x = Mode, y = margin), fill = "slateblue") +
  labs(
    title = "2020 Popular Vote Poll Margin by Interview Mode",
    x = "Mode of Survey Interview",
    y = "Biden–Trump Margin"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_flip() +
  theme_bw()

# -- Continuous Variable By Continuous Variable (Scatterplot) --

Pres2020.PV |>
  filter(SampleSize < 50000) |>
  ggplot() +
  geom_point(
    aes(x = SampleSize, y = absolute_error),
    color = "purple",
    alpha = .4
  ) +
  labs(
    title = "Absolute Polling Error and Sample Size",
    x = "Sample Size in Poll",
    y = "Absolute Error in Biden–Trump Margin"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw()

# - Overplotting and geom_jitter -

Pres2020.PV |>
  filter(SampleSize < 50000) |>
  ggplot() +
  geom_jitter(
    aes(x = SampleSize, y = absolute_error),
    color = "purple",
    alpha = .4,
    height = .001
  ) +
  labs(
    title = "Absolute Polling Error and Sample Size",
    x = "Sample Size in Poll",
    y = "Absolute Error in Biden–Trump Margin"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw()

# -- POSSIBLE ASIDE: Visualizing More Dimensions -- And Introducing Dates! --

# - Plot every poll over time -

raw_poll_plot = Pres2020.PV |>
  ggplot(aes(x = EndDate, y = margin)) +
  geom_jitter(color = "purple", alpha = .4, height = .002) +
  geom_hline(
    yintercept = unique(Pres2020.PV$actual_margin),
    linewidth = 1
  ) +
  labs(
    title = "Margin in 2020 National Popular Vote Polls Over Time",
    x = "Poll Ending Date",
    y = "Biden–Trump Margin"
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw()

raw_poll_plot

# - Should we connect the polls? -

Pres2020.PV |>
  ggplot(aes(x = EndDate, y = margin)) +
  geom_point(color = "purple", alpha = .4) +
  geom_line() +
  labs(
    title = "Individual Poll Margins Connected Over Time",
    x = "Poll Ending Date",
    y = "Biden–Trump Margin"
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw()

# - Change the observation: polls to weeks -

weekly_poll_average = Pres2020.PV |>
  mutate(week = as.Date(cut(EndDate, breaks = "week"))) |>
  group_by(week) |>
  summarize(
    average_margin = mean(margin, na.rm = TRUE),
    number_of_polls = n()
  )

weekly_poll_average

weekly_poll_average |>
  ggplot(aes(x = week, y = average_margin)) +
  geom_point(aes(size = number_of_polls), color = "purple") +
  geom_line(color = "purple") +
  geom_hline(
    yintercept = unique(Pres2020.PV$actual_margin),
    linewidth = 1
  ) +
  labs(
    title = "Weekly Average of 2020 National Popular Vote Polls",
    x = "Week",
    y = "Average Biden–Trump Margin",
    size = "Number of Polls"
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw()
