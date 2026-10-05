# PSC 2300: Data and Politics I
# Session 11: Univariate Data Visualization in ggplot
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Keep this script, insurance.csv, and polls2020.rds in the same folder.
# Then choose Session > Set Working Directory > To Source File Location.

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
