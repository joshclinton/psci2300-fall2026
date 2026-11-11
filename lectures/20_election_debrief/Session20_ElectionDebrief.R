# PSC 2300: Data and Politics I
# Session 20: The Election Is Over: How Did We Do?
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Make sure the following are in the same folder and are named as follows:
#
# Session20_ElectionDebrief.R
# approvalvote2026.csv
# statepolls2026.csv
# midterm_pres2026.csv
# midterm2026_poll_clean.csv
# trump2024_bystate.csv
#
# Then choose Session > Set Working Directory > To Source File Location


# Load the tidyverse package.
library(tidyverse)


# ---- 2. What happened? ------------------------------------------------------

# MOTIVATION QUESTION: How well did our "simple" analyses do?

# The results (NBC News, as of Nov 6):
#   House seats (projected): Democrats 259, Republicans 176
#   Republicans won 220 seats in 2024, so they lost 44
#   Republican share of the two-party House vote: 43.7% (Democrats +12)

# Year             Election year (1946-2026)
# Election         "Midterm" or "Presidential"
# President        President in June of the election year
# Party            President's party ("D" or "R")
# JuneApproval     % approving of the president, last Gallup poll in June
# HouseVoteShare   President's party's share (%) of the two-party House vote
# HouseVoteChange  Change in HouseVoteShare from the previous election
# HouseSeatShare   President's party's share (%) of House seats
# HouseSeatChange  Change in the president's party's House seats
# PresVoteShare    President's party's share (%) of the two-party pres. vote
approvalvote = read.csv("approvalvote2026.csv")

glimpse(approvalvote)


# ---- 3. Forecast #1: approval to seats --------------------------------------

midterms = approvalvote |>
  filter(Election == "Midterm")

midterm.reg = lm(HouseSeatChange ~ JuneApproval, data = midterms)

# 220 = Republican seats won in 2024
# 35.6 = Trump approval in our NBC News poll, raw
# 41.0 = Trump approval in our NBC News poll, fixed by party (46/46/8)
220 + predict(midterm.reg, newdata = tibble(JuneApproval = c(35.6, 41.0)))

# Republicans won (projected) 176. How close were we?
midterms |>
  ggplot(aes(x = JuneApproval, y = HouseSeatChange)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -44, color = "red", linetype = "dashed") +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    x = "President's approval in June (%)",
    y = "Change in the president's party's House seats",
    title = "Red line: Predicted 2026 Republicans"
  ) +
  theme_bw()


# ---- 4. Forecast #2: the generic ballot to seats ----------------------------

before = approvalvote |>
  filter(Year < 2026)

seats.reg = lm(HouseSeatShare ~ HouseVoteShare, data = before)

recent = before |>
  filter(Year >= 1996)

seats.reg.recent = lm(HouseSeatShare ~ HouseVoteShare, data = recent)

# Republican share of the two-party House vote:
# 41.0 = our NBC News poll, raw (D+18)
# 46.9 = our NBC News poll, fixed by party (D+6)
# 43.7 = the actual result (D+12)
# 435 = seats in the House
435 * predict(seats.reg, newdata = tibble(HouseVoteShare = c(41, 46.9, 43.7))) / 100

435 * predict(seats.reg.recent, newdata = tibble(HouseVoteShare = c(41, 46.9, 43.7))) / 100

# Republicans won (projected) 176 seats = 40.5% of seats. Where does 2026 fall?
approvalvote |>
  ggplot(aes(x = HouseVoteShare, y = HouseSeatShare, color = Year == 2026)) +
  geom_hline(yintercept = 50) +
  geom_vline(xintercept = 50) +
  geom_point() +
  geom_smooth(data = before, method = "lm", color = "black") +
  scale_color_manual(values = c("grey50", "red"), labels = c("1946-2024", "2026")) +
  labs(
    x = "President's party's share of the House vote (%)",
    y = "President's party's share of House seats (%)",
    color = ""
  ) +
  theme_bw()


# ---- 5. How did the polls do? -----------------------------------------------

# MOTIVATION QUESTION: Our poll was off on the national vote. Was that just our
# poll, or were the polls off everywhere? And in which direction?

# office                        "Senate" or "Governor"
# state                         Two-letter state abbreviation
# dem_candidate, rep_candidate  Candidates (NE and SD Senate: the independent is
#                               in the dem columns)
# poll_dem, poll_rep            Final polling average (FiftyPlusOne)
# poll_margin                   poll_dem minus poll_rep
# result_dem, result_rep        Share (%) of all votes counted (NBC News)
# result_margin                 result_dem minus result_rep
# pct_counted                   % of the expected vote counted so far
# three_way                     TRUE if a strong third candidate also ran
# trump24                       Trump's share (%) of the two-party vote, 2024
# notes                         Notes on the race
races = read.csv("statepolls2026.csv")

glimpse(races)

races |>
  count(office)

# Are there races with no polling average? Why might a race not be polled?
races |>
  filter(is.na(poll_margin))

races = races |>
  filter(three_way == FALSE)

races = races |>
  mutate(
    dem_error = result_dem - poll_dem,
    rep_error = result_rep - poll_rep,
    margin_error = result_margin - poll_margin
  )

# On average, were the polls off -- and in which direction?
races |>
  group_by(office) |>
  summarize(
    N = sum(!is.na(margin_error)),
    AvgDemError = mean(dem_error, na.rm = TRUE),
    AvgRepError = mean(rep_error, na.rm = TRUE),
    AvgMarginError = mean(margin_error, na.rm = TRUE)
  )

# QUESTION: Why can both dem_error and rep_error be positive in the same race?

races |>
  ggplot(aes(x = margin_error)) +
  geom_histogram(binwidth = 2) +
  geom_vline(xintercept = 0, color = "red") +
  facet_wrap(~ office) +
  labs(
    x = "Polling error in the margin (result minus final average, D - R)",
    y = "Number of races"
  ) +
  theme_bw()

# Quick Exercise: Make the same histogram for dem_error and then rep_error.

# QUESTION: If polling errors were just random sampling error, what would these
# histograms look like? What do they look like?


# ---- 6. A graph from the day after ------------------------------------------

# MOTIVATION QUESTION: A graph posted on November 7 (see the handout) suggests
# that Republicans getting about 85% of Trump's 2024 share "in race after race"
# is suspicious -- too regular to be real. Is it?

# QUESTION: The states are ordered from reddest to bluest in 2024. What does
# that do to how the lines look? What do the lines connecting the states mean?

races = read.csv("statepolls2026.csv") |>
  filter(three_way == FALSE) |>
  mutate(
    rep26 = 100 * result_rep / (result_dem + result_rep),
    kept = rep26 / trump24
  )

races |>
  ggplot(aes(x = trump24, y = rep26)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_text(aes(label = state)) +
  geom_smooth(method = "lm") +
  facet_wrap(~ office) +
  labs(
    x = "Trump's share 2024 (%)",
    y = "Republican share 2026 (%)",
    title = "Two Party Vote Share: 2024 vs 2026"
  ) +
  theme_bw()

# Was it really 85% "in race after race"?
races |>
  group_by(office) |>
  summarize(
    Correlation = cor(trump24, rep26),
    AvgKept = mean(kept),
    MinKept = min(kept),
    MaxKept = max(kept)
  )


# ---- 7. Why would every state look the same? --------------------------------

# vote2024        2024 vote: "Trump", "Harris", "Someone else", "Did not vote",
#                 or "Not eligible"
# generic_ballot  2026 House vote: "Democrat" or "Republican"
# state           Two-letter state abbreviation
poll = read.csv("midterm2026_poll_clean.csv")

poll = poll |>
  mutate(vote24 = case_when(
    vote2024 == "Trump" ~ "Trump",
    vote2024 == "Harris" ~ "Harris",
    TRUE ~ "Neither"
  ))

poll |>
  group_by(vote24) |>
  summarize(
    N = n(),
    PctRep = 100 * mean(generic_ballot == "Republican")
  )

# Is that different from state to state?

# state    Two-letter state abbreviation
# trump24  Trump's share (%) of the two-party presidential vote, 2024
trump2024 = read.csv("trump2024_bystate.csv")

bystate = poll |>
  group_by(vote24, state) |>
  summarize(
    N = n(),
    PctRep = 100 * mean(generic_ballot == "Republican")
  ) |>
  filter(N >= 30) |>
  left_join(trump2024, by = "state")

bystate |>
  ggplot(aes(x = trump24, y = PctRep)) +
  geom_point(aes(size = N)) +
  geom_smooth(method = "lm") +
  facet_wrap(~ vote24) +
  labs(
    x = "Trump's share of the two-party vote in the state, 2024 (%)",
    y = "% voting Republican for the House, 2026",
    size = "Respondents"
  ) +
  theme_bw()


# ---- 8. What does 2026 tell us about 2028? ----------------------------------

# MOTIVATION QUESTION: Republicans lost 44 seats. Does a bad midterm for the
# president's party predict a bad presidential election two years later?

# Year             Year of the midterm
# President        President at the time of the midterm
# Party            President's party ("D" or "R")
# HouseSeatChange  Change in the president's party's House seats in the midterm
# HouseVoteChange  Change in its House vote share in the midterm
# PresVoteBefore   Its two-party presidential vote share (%) two years before
# PresVoteAfter    Its two-party presidential vote share (%) two years after
# PresVoteChange   PresVoteAfter minus PresVoteBefore
midterm_pres = read.csv("midterm_pres2026.csv")

midterm_pres

# Why does the first cor() give NA?
cor(midterm_pres$HouseSeatChange, midterm_pres$PresVoteChange)

cor(midterm_pres$HouseSeatChange, midterm_pres$PresVoteChange,
    use = "complete.obs")

# Do you see a relationship?
midterm_pres |>
  ggplot(aes(x = HouseSeatChange, y = PresVoteChange)) +
  geom_point() +
  labs(
    x = "Pres's party's House midterm seat change",
    y = "Pres's party pres. vote share change two years later"
  ) +
  theme_bw()

# How do we interpret the slope? What does losing 10 more seats in the midterm
# predict for the presidential vote two years later?
pres.reg = lm(PresVoteChange ~ HouseSeatChange, data = midterm_pres)

pres.reg

# What does the model predict for Republicans in 2028?
# -44 = Republican House seat change in 2026
# 50.7 = Republican share of the two-party presidential vote in 2024
predict(pres.reg, newdata = tibble(HouseSeatChange = -44))

50.7 + predict(pres.reg, newdata = tibble(HouseSeatChange = -44))


# ---- 9. Since 1996 ----------------------------------------------------------

# For votes to seats, the elections since 1996 did better. What happens here?
recent_pres = midterm_pres |>
  filter(Year >= 1996)

cor(recent_pres$HouseSeatChange, recent_pres$PresVoteChange,
    use = "complete.obs")

pres.reg.recent = lm(PresVoteChange ~ HouseSeatChange, data = recent_pres)

pres.reg.recent

50.7 + predict(pres.reg.recent, newdata = tibble(HouseSeatChange = -44))


# ---- 10. Careful: what did the election "mean"? -----------------------------

# QUESTION: After an election, someone says "voters rejected X" or "voters
# rewarded Y." What would you need to know to believe it?
