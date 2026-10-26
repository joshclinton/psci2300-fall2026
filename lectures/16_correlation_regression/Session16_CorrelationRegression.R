# PSC 2300: Data and Politics I
# Session 16: Correlations, Regressions, and Butterfly Ballots
# Prof. Josh Clinton
# Vanderbilt University


# ---- 1. Setup ---------------------------------------------------------------

# Make sure the following are in the same folder and are named as follows:
#
# Session16_CorrelationRegression.R
# florida.csv
# datasaurus.csv
# approvalvote.csv
#
# Then choose Session > Set Working Directory > To Source File Location


# Load the tidyverse package.
library(tidyverse)


# ---- 2. The butterfly ballot: read in the data ------------------------------

# Each row is one of Florida's 67 counties.
# Perot96: votes for Ross Perot in 1996. Buchanan00: votes for Patrick
# Buchanan in 2000. Buchanan received 3,407 votes in Palm Beach County.
florida = read.csv("florida.csv")

glimpse(florida)

# Save the scatterplot as flvotes so we can add to it later.
flvotes = florida |>
  ggplot(aes(x = Perot96, y = Buchanan00)) +
  geom_point() +
  labs(
    title = "Florida County Level Voting Data",
    x = "Perot Votes, 1996",
    y = "Buchanan Votes, 2000"
  ) +
  theme_bw()

flvotes


# ---- 3. Summarize with correlation ------------------------------------------

cor(florida$Perot96, florida$Buchanan00)

# Order does not matter.
cor(florida$Buchanan00, florida$Perot96)

# Changing units does not change the correlation.
cor(florida$Perot96 * 100,
    florida$Buchanan00 + 1000)

# Multiplying by a negative number reverses the sign.
cor(florida$Perot96,
    florida$Buchanan00 * -1)

# Correlation measures linear association: a non-linear change alters it.
cor(florida$Perot96,
    florida$Buchanan00^2)


# ---- 4. What happens if we set Palm Beach aside? ----------------------------

florida_nopb = florida |>
  filter(county != "PalmBeach")

cor(florida_nopb$Perot96,
    florida_nopb$Buchanan00)


# ---- 5. Predicting Palm Beach with regression -------------------------------

# Add the regression line to the saved plot.
flvotes +
  geom_smooth(method = "lm")

# Dependent variable on the left of ~, independent variable on the right.
florida.reg = lm(Buchanan00 ~ Perot96, data = florida)

florida.reg

# Predicted value for every county.
predict(florida.reg)

# Predicted value for just Palm Beach.
palm_beach = florida |>
  filter(county == "PalmBeach")

predict(florida.reg, newdata = palm_beach)

# Which regression? florida.reg used every county, including Palm Beach.
# Should Palm Beach help draw the line we compare it to?
florida.reg.nopb = lm(Buchanan00 ~ Perot96, data = florida_nopb)

florida.reg
florida.reg.nopb

predict(florida.reg, newdata = palm_beach)
predict(florida.reg.nopb, newdata = palm_beach)

# Observed minus predicted. Gore lost Florida by 537 votes.
palm_beach$Buchanan00 - predict(florida.reg, newdata = palm_beach)
palm_beach$Buchanan00 - predict(florida.reg.nopb, newdata = palm_beach)


# ---- 6. A complication: the Datasaurus Dozen --------------------------------

# 13 datasets, each with an x and a y.
datasaurus = read.csv("datasaurus.csv")

glimpse(datasaurus)

datasaurus |>
  group_by(dataset) |>
  summarize(
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x = sd(x),
    sd_y = sd(y),
    correlation = cor(x, y)
  )

# Always plot!
datasaurus |>
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~ dataset, ncol = 4) +
  labs(
    title = "Identical Summaries: Different Relationships"
  ) +
  theme_bw()


# ---- 7. Does presidential approval affect House elections? Read in the data -

# Which party will win the 2026 midterms? The election is one week away. Does
# an unpopular president hurt their party's candidates for the House?
#
# In Sections 7-12 you do the analysis yourself, using the same steps we used
# for Palm Beach in Sections 2-5: look at the data, plot the relationship,
# summarize it with cor(), fit a regression with lm(), and make a prediction
# with predict(). Copy, paste, and adjust the code from those sections.
# Write your answers as comments (lines that start with #).

# Each row is one national election from 1946 to 2024 (20 midterms and 20
# presidential elections). The House variables are all measured for the
# PRESIDENT'S PARTY.
#
# Year             Election year
# Election         Type of election: "Midterm" or "Presidential"
# President        President in June of the election year
#                  (1974 is Nixon, who resigned that August)
# Party            President's party: "D" or "R"
# JuneApproval     Percentage approving of the president in the last Gallup
#                  poll taken in June of the election year
# HouseVoteShare   President's party's share (%) of the two-party national
#                  House vote
# HouseVoteChange  HouseVoteShare minus the party's two-party House vote share
#                  two years earlier (percentage points)
# HouseSeatShare   President's party's share (%) of all House seats
# HouseSeatChange  Seats won by the president's party minus the seats it won
#                  two years earlier
# PresVoteShare    President's party's share (%) of the two-party presidential
#                  vote; NA in midterm years
#
# NOTE: Democrats held a House majority from 1955 through 1994!

approvalvote = read.csv("approvalvote.csv")

glimpse(approvalvote)

# Exercise: What does each row represent?



# Exercise: What are the possible dependent variables? What are you trying to
# predict/explain?



# Exercise: What are the possible independent variables? What are you using
# to predict/explain?



# Exercise: What would it mean for the president's party to "do well" in a
# House election? Which dependent variable matches the claim that an
# unpopular president hurts their party? Which one is reported on election
# night?




# ---- 8. Look at the relationship --------------------------------------------

# Exercise: If we are looking at the relationship between two continuous
# variables, what type of graph do we need?



# Exercise: Plot the relationship between JuneApproval and each possible
# dependent variable, one plot at a time. (Start from the flvotes code in
# Section 2 and change the data, the variables, and the labels.) For each:
# Does the relationship move up or down? How tightly do the observations
# cluster? Is there anything strange?




# Exercise: What is the correlation for each? (See Section 3.) Do the
# correlations match what you see in the scatterplots?




# Exercise: How do the graphs differ? Why might a CHANGE (in seats or votes)
# behave differently from a LEVEL (seat share or vote share)? (Recall the
# Democrats' long House majority.)




# ---- 9. Analyze the relationship with regression ----------------------------

# Exercise: Choose the dependent variable that you think is most defensible.
# We have not talked about how to do this statistically yet, so make an
# argument for which you think is most relevant.



# Exercise: Run the regression using lm() and call it house.reg. The
# dependent variable goes on the left of ~ and the independent variable on
# the right. (See Section 5.)

# house.reg = lm( ~ , data = approvalvote)



# Exercise: Interpret the intercept and the slope in words. What does one
# more point of approval mean for your dependent variable? Has any president
# had 0 percent approval?



# Gallup stopped polling presidential approval (why?), but we can still choose
# possible values. predict() gives the prediction for values of X that we
# choose.

predict(house.reg, newdata = tibble(JuneApproval = c(35, 40, 45)))

# Exercise: What does the regression predict? Does that fit what you know
# about recent midterms?




# ---- 10. Are midterms different? --------------------------------------------

# In presidential years, the president (or their party's nominee) is on the
# ballot. In midterm years, the president is not. Should the relationship be
# the same?

# Exercise: Use scatterplots to see if the relationship is different. You can
# add facet_wrap(~ Election) to split a scatterplot by type of election (as
# we did with the Datasaurus in Section 6), or you could filter -- there are
# lots of ways to do this. Adding geom_hline(yintercept = 0) helps if your
# dependent variable is a change.




# Exercise: Where do most midterm elections fall relative to zero? What does
# that tell us?



# Exercise: Compute the correlation separately for each kind of election.
# Use our old friends group_by() and summarize() (see the Datasaurus summary
# in Section 6). Where is the relationship stronger?




# NOTE: 1948 is very different: Truman's Democrats gained 75 seats with 40
# percent approval!

# Exercise: Recalculate the correlation for presidential elections without
# 1948. How much does it change? (Hint: filter(Year != 1948), as we did for
# Palm Beach in Section 4.) Is it OK to drop it?




# ---- 11. Predicting 2026 ----------------------------------------------------

# 2026 is a midterm. If the relationship differs in midterms, a line estimated
# from all 40 elections may predict midterms poorly.

# Exercise: Estimate the regression using only midterm elections. Create a
# new data object using filter and use it as the data in lm(). Call it
# midterm.reg.

# midterms = approvalvote |>
#   filter( )
#
# midterm.reg = lm( ~ , data = midterms)



# Exercise: Redo the prediction at 35, 40, and 45 percent approval using
# midterm.reg.



# Exercise: How do the slope, intercept, and predictions compare with
# house.reg, which used all 40 elections? Which would you use to forecast
# November? Why?




# ---- 12. Your forecast ------------------------------------------------------

# NOTE: In 2024 the president was Biden, so the 2024 row describes Democrats.
# In 2026 the president's party is the Republican Party. Republicans won 220
# of 435 House seats in 2024; a majority is 218.

# Exercise: What value of presidential approval will you use for 2026? Where
# does it come from? Is it measured the same way as Gallup's June approval?
# Is it inside the range of approval values in our data?



# Exercise: Using the regression you think is best, how many seats do you
# predict Republicans will gain or lose? Which party will win the House?




# Exercise: How confident are you? What would make your prediction wrong?



# Exercise: Write down your forecast and keep it -- the approval value you
# used, which regression, the predicted seat change, the number of Republican
# seats, and which party wins the House. On Wednesday we will make a second
# forecast from the votes in our class poll and compare the two.
#
# Approval value:
# Regression:
# Predicted seat change:
# Republican seats:
# Party that wins the House:




# ---- 13. Extra --------------------------------------------------------------

# Exercise: Repeat the comparison for the other dependent variables. For
# each, estimate the regression for all elections, for presidential
# elections, and for midterm elections. Do they give different answers? Think
# about what the slope and intercept mean given the different units being
# used!




# Exercise: Should we only look at more recent years? How would you do this?
# Does it change your forecast?



