# PSC 2300: Data and Politics I
# Session 17: From Polls to Seats: Who Wins the House?
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Make sure the following are in the same folder and are named as follows:
#
# Session17_PollsToSeats.R
# approvalvote.csv
# midterm2026_poll.csv
#
# Then choose Session > Set Working Directory > To Source File Location


# Load the tidyverse package.
library(tidyverse)


# ---- 2. Where we left off: the approval model -------------------------------

approvalvote = read.csv("approvalvote.csv")

midterms = approvalvote |>
  filter(Election == "Midterm")

# Monday's scatterplot: approvalvote? midterms?

# HouseSeatShare?
# HouseSeatChange?
# HouseVoteShare?
# HouseVoteChange?

approvalvote |>
  ggplot(aes(x = JuneApproval, y = HouseSeatChange)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    x = "President's approval in June (%)",
    y = "Change in the president's party's House seats"
  ) +
  theme_bw() 

midterm.reg = lm(HouseSeatChange ~ JuneApproval, data = midterms)

midterm.reg

# Republicans won 220 seats in 2024, so add the predicted change (a negative
# number) to 220 to get seats. Is 218 in reach?
predict(midterm.reg, newdata = tibble(JuneApproval = c(35, 40, 45)))

220 + predict(midterm.reg, newdata = tibble(JuneApproval = c(35, 40, 45)))


# ---- 3. The poll: presidential approval -------------------------------------

# Each row is a respondent. Fielded Sept 28 - Oct 5, 2026.
poll = read.csv("midterm2026_poll.csv")

glimpse(poll)

poll |>
  count(trump_approve)

# Dealing with missing data, and creating the variables we need.
# approve: 1 if they approve of Trump (strongly or somewhat).
# demvote: 1 if the respondent would vote for the Democrat for the House.
poll = poll |>
  drop_na(trump_approve, generic_ballot) |>
  mutate(
    approve = if_else(trump_approve %in% c("Strongly approve", "Somewhat approve"), 1, 0),
    demvote = if_else(generic_ballot == "Democrat", 1, 0)
  )

# Take the poll at face value. What share approve?
poll |>
  summarize(Approve = 100 * mean(approve))

# Plug it into Monday's model.
predict(midterm.reg, newdata = tibble(JuneApproval = 35.6))

220 + predict(midterm.reg, newdata = tibble(JuneApproval = 35.6))

# Should we trust it? Who answered? What party do they identify with?
# Counting leaners as partisans, one estimate is that voters are about 46%
# Democrat, 46% Republican, and 8% Independent.
poll |>
  count(party3) |>
  mutate(Pct = 100 * n / sum(n))


# ---- 4. Fixing it by hand (the template) ------------------------------------

# This time by party, which is simpler than 2024 vote (see the optional section
# at the end). Targets: 46% Democrat, 46% Republican, 8% Independent. Unlike an
# election result, nobody knows these -- they are a guess.

by_party = poll |>
  filter(!is.na(party3)) |>
  group_by(party3) |>
  summarize(
    N = n(),
    PctApprove = mean(approve),
    PctDem = mean(demvote)
  ) |>
  mutate(
    SampleShare = N / sum(N),
    ActualShare = case_when(
      party3 == "Democrat" ~ .46,
      party3 == "Republican" ~ .46,
      party3 == "Independent" ~ .08
    ),
    Weight = ActualShare / SampleShare
  )

by_party

# Each Republican counts as about 1.2 people, each Democrat as about 0.9.

# Multiply each group's answer by its share in the sample and add them up.
# Adjusted multiplies each group's share by its weight. (SampleShare * Weight
# is just ActualShare -- weighting makes each group the size it should be.)
by_party |>
  summarize(
    ApproveUnadjusted = 100 * sum(PctApprove * SampleShare),
    ApproveAdjusted = 100 * sum(PctApprove * SampleShare * Weight)
  )

# What does Monday's model predict now? 

220 + predict(midterm.reg, newdata = tibble(JuneApproval = 41.0))

 


# ---- 5. But approval is not a vote ------------------------------------------

# Presidential approval is not a ballot. Our poll also asks how people will
# vote for the House. Why not just use that? 

poll |>
  ggplot(aes(x = generic_ballot, fill = approve == 1)) +
  geom_bar(position = "fill") +
  labs(
    x = "Generic ballot",
    y = "Share of respondents",
    fill = "Approve of Trump?"
  ) +
  theme_bw()

# To use the House vote, we need to get from votes to seats.


# ---- 6. From votes to seats -------------------------------------------------

# Save the scatterplot so we can add to it later.

SeatsVotes = approvalvote |>
  ggplot(aes(x = HouseVoteShare, y = HouseSeatShare)) +
  geom_hline(yintercept = 50) +
  geom_vline(xintercept = 50) +
  geom_point() +
  labs(
    x = "President's party's share of the two-party House vote (%)",
    y = "President's party's share of House seats (%)"
  ) +
  theme_bw()

SeatsVotes

cor(approvalvote$HouseVoteShare, approvalvote$HouseSeatShare)

# Add the regression line to the saved plot.
SeatsVotes +
  geom_smooth(method = "lm")

seats.reg = lm(HouseSeatShare ~ HouseVoteShare, data = approvalvote)

seats.reg

# Predicted share of seats (a majority is 50%, or 218 of 435 seats).

predict(seats.reg, newdata = tibble(HouseVoteShare = c(44, 46, 48, 50, 52, 54)))


# ---- 7. Which elections? ----------------------------------------------------

# NOTE: Democrats held the House from 1955 through 1994. Maybe that is weird?

recent = approvalvote |>
  filter(Year >= 1996)

seats.reg.recent = lm(HouseSeatShare ~ HouseVoteShare, data = recent)

seats.reg.recent

predict(seats.reg.recent, newdata = tibble(HouseVoteShare = c(44, 46, 48, 50, 52, 54)))


# ---- 8. Back to the poll: the generic ballot --------------------------------

# Does Weighting Matter
by_party |>
  summarize(
    Unadjusted = 1 - sum(PctDem * SampleShare),
    Adjusted = 1 - sum(PctDem * SampleShare * Weight)
  )

# Let's use that to predict!

# Everyone who answered, no adjustments. Republicans are the president's party.
predict(seats.reg, newdata = tibble(HouseVoteShare = 41))

# With Adjustment
predict(seats.reg, newdata = tibble(HouseVoteShare = 46.9))

# Has the president's party ever done this badly?
min(approvalvote$HouseVoteShare)
min(approvalvote$HouseSeatShare)



# ---- 9. Your turn! ----------------------------------------------------------

# You now have all the pieces. At each step you make a decision. There is no
# single right answer -- but you need to be able to defend what you did.

# ---- 10. Step 1: who is a likely voter? -------------------------------------

# Not everyone who answers a poll will vote, and there is no rule book for
# deciding who will.

# YOUR DECISION: Who is a likely voter? Look at registered, vote_intent, and
# vote2024.

# Create a variable called likely that is 1 if likely and 0 else


# ---- 11. Step 2: who will vote? ---------------------------------------------

# What is the partisan composition likely to be? How does that matter? We can
# change the numbers (make sure they add to 1!) and see how that matters!

by_party_new = poll |>
  filter(likely == 1) |>
  filter(!is.na(party3)) |>
  group_by(party3) |>
  summarize(
    N = n(),
    PctApprove = mean(approve),
    PctDem = mean(demvote)
  ) |>
  mutate(
    SampleShare = N / sum(N),
    ActualShare = case_when(
      party3 == "Democrat" ~ .46,  # replace
      party3 == "Republican" ~ .46, # replace
      party3 == "Independent" ~ .08 # replace
    ),
    Weight = ActualShare / SampleShare
  )

by_party_new


# Now predict!

# Use the approval model and also the seat share model. Do the two models agree?




# ---- 12. Step 3: compare ----------------------------------------------------

# Exercise: Put all of your predictions side by side, including Monday's. Do
# they agree? If not, is it the model or what you plugged in? Which decision
# mattered the most?




# ---- 13. Step 4: what must be true for Republicans to win? ------------------

# Flip it around: what would X have to be for Republicans to win 218 seats
# (50% of the seats)?

# Exercise: What share of the vote do Republicans need? Then: what would you
# have to assume about who votes to get our poll there? Does that electorate
# seem plausible?



# ---- 14. Make your prediction! ----------------------------------------------
#
# Republican seats:
# Which party wins the House:
# How sure are you (0-100%)?
# Which model and which value of X do you trust most, and why?
# What would make you wrong?




# ---- 15. Optional: fixing by 2024 vote --------------------------------------

# Party targets are a guess. The 2024 result is a fact. So why not fix the
# sample to how people voted in 2024?

# Who answered? About 89% of registered voters voted in 2024. Among those who
# voted: Trump 49.8, Harris 48.3, someone else 1.9.
poll |>
  count(vote2024) |>
  mutate(Pct = 100 * n / sum(n))

# Targets, as shares of registered voters: the actual 2024 result among the 89%
# who voted; 11% did not vote according to CPS.
turnout24 = .89

by_vote = poll |>
  filter(vote2024 != "Not eligible", !is.na(vote2024)) |>
  group_by(vote2024) |>
  summarize(
    N = n(),
    PctApprove = mean(approve),
    PctDem = mean(demvote)
  ) |>
  mutate(
    SampleShare = N / sum(N),
    ActualShare = case_when(
      vote2024 == "Harris" ~ .483 * turnout24,
      vote2024 == "Trump" ~ .498 * turnout24,
      vote2024 == "Someone else" ~ .019 * turnout24,
      vote2024 == "Did not vote" ~ 1 - turnout24
    ),
    Weight = ActualShare / SampleShare
  )

by_vote

by_vote |>
  summarize(
    Adjusted = sum(PctDem * SampleShare * Weight),
    ApproveAdjusted = 100 * sum(PctApprove * SampleShare * Weight)
  ) |>
  mutate(
    AdjustedMargin = 100 * (2 * Adjusted - 1),
    RepShare = 100 * (1 - Adjusted)
  )

# How does this compare to fixing by party? Now try it on your likely voters.
# The 2024 result is a fact -- but what share of likely voters did not vote in
# 2024? (Change turnout24.)
