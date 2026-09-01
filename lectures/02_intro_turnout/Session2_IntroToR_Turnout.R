# PSC 2300: Data and Politics I
# Lecture 2: Introduction to R and voter turnout
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Make sure the following are in the same folder and are named as follows:
#
# Session2_IntroToR_Turnout.R
# turnout.csv
#
# Then choose Session > Set Working Directory > To Source File Location


# Load tidyverse package:
# We installed this in the GUI: 
# install.packages(tidyverse)  

library(tidyverse)  

# ---- 2. Load the turnout data -----------------------------------------------

# read.csv() reads the file; = assigns the result to an object named df.
df = read.csv("turnout.csv")

# Type df in the Console to print the data.
df


# ---- 3. Inspect the dataset -------------------------------------------------

# Each row is an election year. Each column is a variable.
names(df)

# View() opens a spreadsheet-style viewer in RStudio. Capitalization matters.
View(df)

# glimpse() shows variable types and sample values.
glimpse(df)

# head() shows the first six rows.
head(df)

# The $ operator extracts one named variable from a data frame.
df$ANES.Est


# ---- 4. Create turnout measures ---------------------------------------------

# VEP turnout: ballots divided by the voting-eligible population.
VEPturnout = df$Total.Ballots.Counted / df$VEP

# Store the new measure as a column in df.
df$VEPturnout = df$Total.Ballots.Counted / df$VEP

# Always inspect a newly created variable.
df$VEPturnout

# Exercise: Create turnout using the voting-age population (VAP).
df$VAPturnout = df$Total.Ballots.Counted / df$VAP

# Assignment makes a copy of the entire data frame.
copy.df = df


# ---- 5. Filter rows and select columns --------------------------------------

# The pipe |> means "and then." This keeps only the 2024 row.
df |>
  dplyr::filter(year == 2024)

# Exercise: Save the 2024 row as a new object.



# Keep only four variables from the 2024 data.
df2024 |>
  dplyr::select(year, Total.Ballots.Counted, VAP, VEP)

# How much larger is the voting-age population than the voting-eligible
# population?
df2024$VAP - df2024$VEP

# Noncitizens and ineligible felons are excluded from VEP; eligible overseas
# voters are added.
df2024$Noncit + df2024$Felons - df2024$Overseas

# So how close was the 2024 popular vote margin?
margin24thousands = 2284967/1000

# What is the right denominator?
margin24thousands/df2024$Total.Ballots.Counted
margin24thousands/df2024$VAP
margin24thousands/df2024$VEP


# Recent elections from 2014 through 2020. The & operator means AND.
df |>
  dplyr::filter(year >= 2014 & year <= 2020) |>
  select(year, VAPturnout, VEPturnout, ANES.Est)

# The three most recent presidential elections. The | operator means OR.
df |>
  dplyr::filter(year == 2016 | year == 2020 | year == 2024) |>
  select(year, VAPturnout, VEPturnout, ANES.Est)

# Exercise: Now do the three most recent midterm election: 2014, 2018, 2022


# ---- 6. Summarize turnout ---------------------------------------------------

# Calculate one summary at a time.
mean(df$VEPturnout)
mean(df$VAPturnout)

# What about for the survey data?
mean(df$ANES.Est)

# na.rm = TRUE tells mean() to remove missing observations first.
mean(df$ANES.Est, na.rm = TRUE)

# summarize() is useful when calculating one or more statistics
df |>
    summarize(AvgANES = mean(ANES.Est, na.rm = TRUE))

# Why do this? So we can stack commands

df |>
  summarize(
    AvgVAP = mean(VAPturnout),
    AvgVEP = mean(VEPturnout)
  )

# Components of the 2024 VAP, expressed first as proportions...
df2024 |>
  summarize(
    VEPPct = VEP / VAP,
    NoncitizenPct = Noncit / VAP,
    FelonPct = Felons / VAP,
    OverseasPct = Overseas / VAP
  )

# ...and then as percentages.
df2024 |>
  summarize(
    VEPPct = 100 * VEP/VAP,
    NoncitizenPct = 100 * Noncit / VAP,
    FelonPct = 100 * Felons / VAP,
    OverseasPct = 100 * Overseas / VAP
  )

# Compare all three turnout measures.
df |>
  summarize(
    AvgANES = mean(ANES.Est, na.rm = TRUE),
    AvgVEP = mean(VEPturnout, na.rm = TRUE),
    AvgVAP = mean(VAPturnout, na.rm = TRUE)
  )

# ---- 7. Compare turnout over time -------------------------------------------

# Average VEP turnout before 2010.
df |>
  dplyr::filter(year < 2010) |>
  summarize(AvgVEPturnout = mean(VEPturnout, na.rm = TRUE))

# Average VEP turnout from 2010 onward.
df |>
  dplyr::filter(year >= 2010) |>
  summarize(AvgVEPturnout = mean(VEPturnout, na.rm = TRUE))

# Exercise: Make the same comparison using ANES self-reported turnout. What do you think?




# Discussion: Is that a fair comparison? Check out the variable itself...



# ---- 8. Challenge questions -------------------------------------------------

# Using the tools above, investigate:
# 1. Is turnout increasing over time? If so, by how much?
# 2. How does turnout differ between presidential and midterm elections?
# 3. Based on these patterns, what would you expect turnout to be in 2026?
