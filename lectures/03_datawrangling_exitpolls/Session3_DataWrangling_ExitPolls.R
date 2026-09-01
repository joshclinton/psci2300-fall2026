# PSC 2300: Data and Politics I
# Lecture 3: Data wrangling and the gender gap
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Make sure the following are in the same folder and are named as follows:
#
# Session3_DataWrangling_ExitPolls.R
# ces2024.csv
#
# Then choose Session > Set Working Directory > To Source File Location


# Load tidyverse package:
# We installed this in the GUI:
# install.packages("tidyverse")

library(tidyverse)


# ---- 2. Load and inspect the CES data ---------------------------------------

# Each row is a survey respondent. Each column is a variable.
ces = read.csv("ces2024.csv")

# How many rows and columns are in the data?
dim(ces)

# glimpse() shows variable names, types, and sample values.
glimpse(ces)

# summary() helps reveal unusual values and missing data.
summary(ces)

# What is the deal with TS_g2024?


# ---- 3. Rename variables ----------------------------------------------------

# What does this do?
 ces |>
   rename(selfreport_voted = CC24_401,
          validated_voted = TS_g2024)

# Assign the result back to ces so the new names stick.
ces = ces |>
  rename(selfreport_voted = CC24_401,
         validated_voted = TS_g2024)

# Check the variable names.
names(ces)


# ---- 4. Create age ----------------------------------------------------------

# Calculate age in 2024 from the respondent's birth year.
ces = ces |>
  mutate(age = 2024 - birthyr)

# Check the new variable.
summary(ces$age)


# ---- 5. Count possible values ----------------------------------------------

# R is case-sensitive. Presvote and presvote are not the same name.
ces |>
   count(Presvote)

# Count reported presidential vote choices.
ces |>
  count(presvote)

# The codebook says gender4 is coded as follows:
# 1 = Man, 2 = Woman, 3 = Non-binary, 4 = Other
ces |>
  count(gender4)

# Exercise: Use count() to inspect pid3. Are its values stored as numbers or
# character labels?



# ---- 6. Missing values and validated turnout -------------------------------

# Inspect the voter-file variable before calculating anything.
ces |>
  count(validated_voted)

# What happens if we treat -99 as a meaningful number?
ces |>
  summarize(PctValidated = mean(validated_voted))

# One possibility: treat unmatched respondents as nonvoters. This changes -99
# to 0 temporarily, without saving the change to ces.
ces |>
  mutate(validated_voted = if_else(validated_voted == -99,
                                   0,
                                   validated_voted)) |>
  summarize(PctValidated = mean(validated_voted))

# Another possibility: calculate turnout only among matched respondents.
ces |>
  filter(validated_voted != -99) |>
  summarize(PctValidatedAmongMatched = mean(validated_voted))

# Which approach answers the question we care about? This requires an argument,
# not just code.

# For today's analysis, treat unmatched respondents as missing. Remember:
# one = assigns a value; two == compare values.
ces = ces |>
  mutate(validated_voted = if_else(validated_voted == -99,
                                   NA,
                                   validated_voted))

# Always check that a mutation did what we intended.
ces |>
  count(validated_voted)


# ---- 7. Self-reported turnout ----------------------------------------------

# Inspect the self-reported voting variable.
ces |>
  count(selfreport_voted)

# Add a proportion for each response category.
ces |>
  count(selfreport_voted) |>
  mutate(Pct = n / sum(n))

# Optional: Count validated and self-reported voting together.
# ces |>
#   count(validated_voted, selfreport_voted)


# ---- 8. Create a two-candidate dataset -------------------------------------

# Character values must be placed inside quotation marks. This would fail:
# ces |>
#   filter(presvote == Harris)

# Keep respondents who reported voting for Harris OR Trump.
ces_2cand = ces |>
  filter(presvote == "Harris" | presvote == "Trump")

# How many observations remain?
dim(ces_2cand)

# Create a binary variable: 1 = Harris voter and 0 = Trump voter.
ces_2cand = ces_2cand |>
  mutate(HarrisVoter = if_else(presvote == "Harris", 1, 0))

# Check the new variable against the original vote-choice variable.
ces_2cand |>
  count(presvote, HarrisVoter)

# The mean of a zero-one variable is the proportion equal to one.
ces_2cand |>
  summarize(PctHarris = mean(HarrisVoter))

# The raw, unweighted CES data imply that Harris won the two-party vote. She
# did not. We will return to that problem below.


# ---- 9. Calculate the descriptive gender gap -------------------------------

# Focus this comparison on respondents coded as men or women. Then create a
# binary variable: 1 = woman and 0 = man.
ces_gender = ces_2cand |>
  filter(gender4 == 1 | gender4 == 2) |>
  mutate(woman = if_else(gender4 == 2, 1, 0))

# Save the row-level data for Lecture 6. The official course copy will also be
# provided, so Lecture 6 does not depend on this classroom file surviving.
saveRDS(ces_gender, "CES2024_GenderGap.rds")

# How many respondents are behind each comparison?
ces_gender |>
  count(gender4)

# Proportion of women who reported voting for Harris.
Women = ces_gender |>
  filter(woman == 1) |>
  summarize(PctHarris = mean(HarrisVoter))

Women

# Proportion of men who reported voting for Harris.
Men = ces_gender |>
  filter(woman == 0) |>
  summarize(PctHarris = mean(HarrisVoter))

Men

# The descriptive gender gap is the difference between the proportions.
GenderGap = Women$PctHarris - Men$PctHarris

round(GenderGap, digits = 3)
round(100 * GenderGap, digits = 1)

# Interpretation: Among men and women in the two-party CES sample, women were
# about 5 percentage points more likely than men to report voting for Harris.
# This describes a difference in the sample. It does not establish that gender
# caused the difference.


# ---- 10. Does the gender gap differ by age? ---------------------------------

# Create one dataset for respondents ages 18-24 and another for ages 65+.
Voters18to24 = ces_gender |>
  filter(age >= 18 & age <= 24)

Voters65Plus = ces_gender |>
  filter(age >= 65)

# How much data do we have in each comparison?
Voters18to24 |>
  count(woman)

Voters65Plus |>
  count(woman)

# Gender gap among respondents ages 18-24.
Women18to24 = Voters18to24 |>
  filter(woman == 1) |>
  summarize(PctHarris = mean(HarrisVoter))

Men18to24 = Voters18to24 |>
  filter(woman == 0) |>
  summarize(PctHarris = mean(HarrisVoter))

GenderGap18to24 = Women18to24$PctHarris - Men18to24$PctHarris

Women18to24
Men18to24
round(GenderGap18to24, digits = 3)

# Gender gap among respondents ages 65 and older.
Women65Plus = Voters65Plus |>
  filter(woman == 1) |>
  summarize(PctHarris = mean(HarrisVoter))

Men65Plus = Voters65Plus |>
  filter(woman == 0) |>
  summarize(PctHarris = mean(HarrisVoter))

GenderGap65Plus = Women65Plus$PctHarris - Men65Plus$PctHarris

Women65Plus
Men65Plus
round(GenderGap65Plus, digits = 3)

# Finding different gaps across age groups remains descriptive. It does not
# establish that either gender or age caused vote choice.


# ---- 11. Can we trust the survey's account? ---------------------------------

# Restrict the two-candidate sample to validated voters.
ces_2cand |>
  filter(validated_voted == 1) |>
  summarize(PctHarris = mean(HarrisVoter))

# Compare that result with the entire two-candidate sample.
ces_2cand |>
  summarize(PctHarris = mean(HarrisVoter))

# The discrepancy does not disappear. A large sample can still misrepresent
# the population of interest. Survey weights are not included in this teaching
# extract, so the gender gap should be described as a feature of this sample.


# ---- 12. Optional extension: education -------------------------------------

# Use case_when() to replace numeric education codes with meaningful labels.
# ces_educ = ces |>
#   mutate(education = case_when(
#     educ == 1 ~ "No HS",
#     educ == 2 ~ "High school graduate",
#     educ == 3 ~ "Some college",
#     educ == 4 ~ "2-year degree",
#     educ == 5 ~ "4-year degree",
#     educ == 6 ~ "Post-graduate"
#   ))
#
# ces_educ |>
#   count(educ, education)

# Define college graduates as respondents with a four-year or post-graduate
# degree.
# ces_gender = ces_gender |>
#   mutate(collegegrad = if_else(educ == 5 | educ == 6, 1, 0))

# Calculate Harris support among college graduates.
# ces_gender |>
#   filter(collegegrad == 1) |>
#   summarize(PctHarris = mean(HarrisVoter))

# Exercise: Repeat the analysis among respondents without a four-year degree.
# What is the descriptive education gap?
