# PSC 2300: Data and Politics I
# Session 11: Visualizing With the Grammar of Graphics
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Make sure the following are in the same folder and are named as follows:
#
# Session11_Visualization.R
# insurance.csv
# polls2020.rds
# CES2024_GenderGap.rds
#
# Then choose Session > Set Working Directory > To Source File Location


# Load the tidyverse package.
library(tidyverse)

# scales: percent_format() and comma for axis labels.
library(scales)


# ---- 2. Application 1: the insurance mileage data ---------------------------

# Each row is the first car on a policy. miles_driven = updated - baseline.
insurance = read.csv("insurance.csv")

# glimpse() shows variable names, types, and sample values.
glimpse(insurance)

# Always start with a numeric summary.
insurance |>
  select(miles_driven) |>
  summary()


# ---- 3. Build a histogram in stages -----------------------------------------

# A blank canvas!
insurance |>
  ggplot(aes(x = miles_driven))

# Continuous variable = histogram. Default is 30 bins.
insurance |>
  ggplot(aes(x = miles_driven)) +
  geom_histogram()

# Choose the bins. color outlines each bar.
insurance |>
  ggplot(aes(x = miles_driven)) +
  geom_histogram(bins = 25, color = "black", fill = "gold")

# Exercise: What do you predict will happen if we use 10 bins? What about 100?



# Never use variable names as labels. Also define the scale of the x-axis.
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


# ---- 4. Do the numbers look like numbers people report? ---------------------

# People often round. What should the final digit look like?
insurance |>
  count(baseline_last_digit)

# Discrete variable = geom_bar(). A graph of the count() output.
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

# Exercise: Now make the same graph using updated_last_digit. What do you
# predict? What do you observe?




# ---- 5. One more piece of information: the font -----------------------------

# Two fonts: half Calibri, half Cambria. Look separately.
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

# facet_wrap(): a graph for each value of font. Comes after ggplot().
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


# ---- 6. Application 2: 2020 election polls ----------------------------------

# Each row is a 2020 national poll.
Pres2020.PV = readRDS(file = "polls2020.rds")
glimpse(Pres2020.PV)

# A little bit of wrangling: margin between Biden and Trump.
Pres2020.PV = Pres2020.PV |>
  mutate(
    margin = Biden - Trump,
    biden_error = Biden - DemCertVote
  )

# What did the polls show? geom_vline() adds a vertical line at 0.
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

# Exercise: Plot the distribution of polling error for Biden. Add a vertical
# line at zero so that accurate polls have a visible reference point.



# Density: no need to choose bins. Area under the curve sums to 1.
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


# ---- 7. Discrete by discrete: vote and gender in the 2024 CES ---------------

# ces_gender from Sessions 3 and 6. Each row is a Harris or Trump voter.
ces_gender = readRDS("CES2024_GenderGap.rds")

# New variable: age group.
ces_gender = ces_gender |>
  mutate(
    age_group = case_when(
      age < 30 ~ "18–29",
      age < 65 ~ "30–64",
      age >= 65 ~ "65+"
    )
  )

glimpse(ces_gender)

# Start with what we already know how to do.
ces_gender |>
  ggplot() +
  geom_bar(aes(x = presvote), color = "black") +
  labs(
    title = "Reported 2024 Presidential Vote",
    x = "Candidate",
    y = "Number of Respondents"
  ) +
  theme_bw()

# Add a second variable using fill. Label it in labs().
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

# Exercise: Redo the code without position = "dodge". What happens? Do you
# like this better or worse? Why?



# Counts or proportions? position = "fill" compares within gender.
# scale_fill_manual() chooses the colors.
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

# Same gender gap among younger, middle-aged, and older voters?
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

# Exercise: Use count() to determine how many respondents appear in every
# gender-by-age group. Does the graph show those differences in cell size?




# ---- 8. Continuous by discrete: poll margins by interview mode --------------

# Each observation is now a poll. Same file, read in again:
# dates converted and vote shares now proportions.
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

# Polls were done using lots of different methods in 2020.
Pres2020.PV |>
  count(Mode, sort = TRUE)

# Boxplot: median, 25th and 75th percentiles, whiskers, outliers.
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


# ---- 9. Continuous by continuous: sample size and accuracy ------------------

# Were polls with more respondents more accurate? alpha = transparency.
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

# geom_jitter() adds a tiny bit of randomness to each point.
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


# ---- 10. POSSIBLE ASIDE: polls over time ------------------------------------

# Each poll's margin at the date the poll ended.
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

# Should we connect the polls? Does this make sense?
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

# Change the observation: polls to weeks. Average margin in each week.
weekly_poll_average = Pres2020.PV |>
  mutate(week = as.Date(cut(EndDate, breaks = "week"))) |>
  group_by(week) |>
  summarize(
    average_margin = mean(margin, na.rm = TRUE),
    number_of_polls = n()
  )

weekly_poll_average

# The line now connects weekly summaries. Point size = number of polls.
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

# Exercise: Change breaks = "week" to breaks = "2 weeks" or change mean() to
# median(). How does the graph change? Which version do you prefer, and why?
