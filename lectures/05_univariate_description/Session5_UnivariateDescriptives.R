# PSC 2300: Data and Politics I
# Lecture 5: Univariate Description: The Cases of Himmicanes and Congressional Redistricting
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Make sure the following are in the same folder and are named as follows:
#
# Session5_UnivariateDescriptives.R
# hurricanes.csv
# cd2026_by_presvote.csv
#
# Then choose Session > Set Working Directory > To Source File Location


# Load the tidyverse package.
library(tidyverse)


# ---- 2. Motivating Claim 1: are female-named hurricanes deadlier? ----------

# Each row is a named U.S. hurricane that made landfall between 1950 and 2012.
hurr = read.csv("hurricanes.csv")

# glimpse() shows variable names, types, and sample values.
glimpse(hurr)

# summary() reports the minimum, maximum, mean, and the 25th, 50th, and 75th
# percentiles for every numeric variable. The 50th percentile is the median.
summary(hurr)

# Apply summary() to one variable using select().
hurr |>
  select(BaseDam2014) |>
  summary()

# Non-tidyverse version: reference a variable with $.
summary(hurr$BaseDam2014)

# How deadly is the "typical" hurricane? It depends on what typical means.
hurr |>
  summarize(mean_deaths   = mean(deaths, na.rm = TRUE),
            median_deaths = median(deaths, na.rm = TRUE))

# The mean is many times the median. Sort the storms from deadliest down to
# see why. arrange() sorts smallest to largest; desc() reverses that.
hurr |>
  arrange(desc(deaths)) |>
  select(Name, Year, mf, deaths) |>
  head(10)


# ---- 3. Does the claim hold up? ---------------------------------------------

# "Female hurricanes are deadlier" is a claim about a grouped average.
# group_by(mf) splits the data into one group per value of mf (f and m), so
# the summarize() that follows is computed separately for each group.
hurr |>
  group_by(mf) |>
  summarize(n = n(),
            mean_deaths   = mean(deaths),
            median_deaths = median(deaths))

# The means support the claim. Do the medians?

# Male names did not enter the rotation until 1979. Restrict the comparison
# to the years when both kinds of names were in use.
hurr |>
  filter(Year >= 1979) |>
  group_by(mf) |>
  summarize(n = n(),
            mean_deaths   = mean(deaths),
            median_deaths = median(deaths))

# The mean gap does not go away. Which storms are driving it?


# ---- 4. Motivating Claim 2: the 2024 presidential vote by district ---------

# Each row is one of the 435 congressional districts. Variables ending in
# _2024map use the lines from the 2024 election; variables ending in _2026map
# use the lines for the 2026 election. Both measure the SAME 2024 votes.
cd = read.csv("cd2026_by_presvote.csv")

glimpse(cd)


# ---- 5. Continuous: the Democratic margin -----------------------------------

# The 10 most Democratic districts. dem_margin_2024map is Harris percent
# minus Trump percent, so positive values are Harris districts.
cd |>
  select(district, member, party, dem_margin_2024map) |>
  arrange(desc(dem_margin_2024map)) |>
  head(10)

# Exercise: Do the same thing for the 10 most Republican districts. How do
# they compare to the 10 most Democratic districts?



# Mean versus median margin across all 435 districts.
cd |>
  summarize(mean_margin = mean(dem_margin_2024map),
            median_margin = median(dem_margin_2024map))

# The average district is essentially tied; the median district leans
# Republican. Why do they differ? Look back at the 10 most Democratic
# districts.

# Exercise: Determine the typical value of harris_pct_2024map. Which measure
# would you prefer, and why?



# One state at a time is a pain.
cd |>
  filter(state == "TN") |>
  summarize(mean_margin = mean(dem_margin_2024map),
            median_margin = median(dem_margin_2024map))

# group_by(state) computes the summary separately for every state.
cd |>
  group_by(state) |>
  summarize(n_districts = n(),
            mean_margin = mean(dem_margin_2024map),
            median_margin = median(dem_margin_2024map))

# Exercise: Which states have the biggest gap between the mean and the median
# district? This needs a mutate() -- before or after the summarize()?



# ---- 6. Categorical (ordered): competitiveness ------------------------------

# Create an ordered categorical variable with case_when(). abs() gives the
# size of the margin regardless of who won.
cd = cd |>
  mutate(competitive_2024map = case_when(
    abs(dem_margin_2024map) < 5 ~ "Toss-up (<5)",
    abs(dem_margin_2024map) < 10 ~ "Lean (5-10)",
    abs(dem_margin_2024map) < 20 ~ "Likely (10-20)",
    abs(dem_margin_2024map) >= 20 ~ "Safe (20+)"))

# Count the categories. The most common category is the mode.
cd |>
  count(competitive_2024map)

# Add the percentage in each category.
cd |>
  count(competitive_2024map) |>
  mutate(Pct = 100*n/sum(n))


# ---- 7. Categorical (unordered): state --------------------------------------

# A mean or median of state makes no sense. count() gives the mode.
cd |>
  count(state) |>
  arrange(desc(n)) |>
  head(10)


# ---- 8. Categorical (binary): redrawn districts and Harris districts --------

# redistricted is 1 if the district's lines were redrawn for 2026, 0 if not.
cd |>
  count(redistricted)

# The mean of a 0/1 variable is the proportion of 1s. Multiply by 100 for a
# percentage.
cd |>
  summarize(PctRedistricted = 100*mean(redistricted))

# Create our own binary variables: did Harris win the district? Once for each
# set of lines.
cd = cd |>
  mutate(harris_won_2024map = if_else(dem_margin_2024map > 0, 1, 0),
         harris_won_2026map = if_else(dem_margin_2026map > 0, 1, 0))

# sum() counts the districts; mean() gives the proportion.
cd |>
  summarize(HarrisDistricts = sum(harris_won_2024map),
            PctHarris = 100*mean(harris_won_2024map))

# Harris carried about 47% of districts while losing the national popular
# vote by about 1.5 points. What is the unit of analysis here?


# ---- 9. What did redistricting do? -----------------------------------------

# Same votes, different lines. Compare the mean, the median, and the number of
# Harris districts under the old (2024) and new (2026) lines.
cd |>
  summarize(mean_old = mean(dem_margin_2024map),
            mean_new = mean(dem_margin_2026map),
            median_old = median(dem_margin_2024map),
            median_new = median(dem_margin_2026map),
            seats_old = sum(harris_won_2024map),
            seats_new = sum(harris_won_2026map))

# The mean barely moves. It cannot: the same votes are being added up in
# different piles. The median and the seat count do move.

# By state. sum(redistricted) counts the redrawn districts in each state;
# filtering on that count AFTER summarize() keeps only the states that
# changed something.
cd |>
  group_by(state) |>
  summarize(n_redrawn = sum(redistricted),
            mean_old = mean(dem_margin_2024map),
            mean_new = mean(dem_margin_2026map),
            median_old = median(dem_margin_2024map),
            median_new = median(dem_margin_2026map),
            seats_old = sum(harris_won_2024map),
            seats_new = sum(harris_won_2026map)) |>
  filter(n_redrawn > 0)

# Texas: mean and median essentially unchanged, Harris districts 11 to 8.
# California: mean unchanged, median falls, Harris districts 41 to 47.
# How can the typical district move toward Republicans while Democrats win
# more of them?

# Texas district by district, sorted by the old margin. Find the districts
# where the sign flips (cracked) and the safe districts that got safer
# (packed).
cd |>
  filter(state == "TX") |>
  select(district, party, dem_margin_2024map, dem_margin_2026map) |>
  arrange(dem_margin_2024map)

# Exercise: Do the same district-by-district look for Florida. Which districts
# were cracked? Which were packed?



# Exercise: Redo the competitiveness categories using dem_margin_2026map. Did
# redistricting make districts more or less competitive?



# ---- 10. Percentage points versus percent change ----------------------------

# Toss-up districts: 44 under the 2024 lines, 37 under the 2026 lines. As a
# share of 435 districts that is 10.1% and 8.5%.

# Percentage point change: new percent minus old percent.
8.5 - 10.1

# Percent change: 100 * (new - old) / old.
100*(37 - 44)/44

# Same change, very different-sounding numbers.


# ---- 11. A counterfactual: turning vote shifts into seats -------------------

# Uniform swing: every district moves by the same amount. A 4-point shift
# toward Democrats is +4 on every margin. Count districts above zero under
# each set of lines. A House majority is 218 seats.
cd |>
  mutate(harris_won_old_shift4 = if_else(dem_margin_2024map + 4 > 0, 1, 0),
         harris_won_new_shift4 = if_else(dem_margin_2026map + 4 > 0, 1, 0)) |>
  summarize(seats_old = sum(harris_won_2024map),
            seats_old_shift4 = sum(harris_won_old_shift4),
            seats_new = sum(harris_won_2026map),
            seats_new_shift4 = sum(harris_won_new_shift4))

# The same 4-point shift wins a majority under the old lines but not the new
# ones. Where did the swing go?
cd |>
  mutate(harris_won_old_shift4 = if_else(dem_margin_2024map + 4 > 0, 1, 0),
         harris_won_new_shift4 = if_else(dem_margin_2026map + 4 > 0, 1, 0)) |>
  group_by(state) |>
  summarize(n_redrawn = sum(redistricted),
            seats_old = sum(harris_won_2024map),
            seats_old_shift4 = sum(harris_won_old_shift4),
            seats_new = sum(harris_won_2026map),
            seats_new_shift4 = sum(harris_won_new_shift4)) |>
  filter(n_redrawn > 0)

# In Texas and Florida a 4-point swing changes nothing under the new lines.

# A 13-point wave. Same code, different number. This time save the new
# variables to cd.
cd = cd |>
  mutate(harris_won_old_shift13 = if_else(dem_margin_2024map + 13 > 0, 1, 0),
         harris_won_new_shift13 = if_else(dem_margin_2026map + 13 > 0, 1, 0))

cd |>
  summarize(seats_old_shift13 = sum(harris_won_old_shift13),
            seats_new_shift13 = sum(harris_won_new_shift13))

cd |>
  group_by(state) |>
  summarize(n_redrawn = sum(redistricted),
            seats_new = sum(harris_won_2026map),
            seats_new_shift4 = sum(if_else(dem_margin_2026map + 4 > 0, 1, 0)),
            seats_new_shift13 = sum(harris_won_new_shift13)) |>
  filter(n_redrawn > 0)

# Now the cushions give way: Texas 8 to 11, Florida 4 to 9. A map built to
# withstand a normal election can hand over seats in a wave.

# Exercise: What happens with a 4-point shift toward Republicans? With a
# 2-point shift toward Democrats? How big a Democratic swing is needed to
# reach 218 under the new lines?



# A uniform swing is an assumption, not a fact. Real swings vary across
# districts. Next session: uncertainty.
