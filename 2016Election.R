library(ggplot2)
library(jsonlite)
library(tidyverse)
library(tidytext)
library(stringr)
library(scales)
library(tidyjson)
library(lubridate)

# import and merge election and population/electoral college datasets
electionOnly <- read.csv('2016ElectionResultsByState.csv')
electoralCollege <- read.csv('2016ElectoralCollege.csv')
election <- merge(electionOnly,
                  electoralCollege,
                  by.x = 'state',
                  by.y = 'state',
                  all = TRUE
                  )
write.csv(election, '2016ElectionResultsWithElectoralCollege.csv')

# percentage of population voting per state
voterTurnout <- election$totalVotes / election$population2010

# electors per citizen and per voter
electorsPerCitizen <- election$electors2016 / election$population2010 * 1000000
electorsPerVoter <- election$electors2016 / election$totalVotes *1000000

# margin of victory
marginOfVictory <- abs(election$trumpVotes - election$clintonVotes)
marginOfVictoryPercent <- abs((election$trumpVotes - election$clintonVotes) / election$totalVotes)

# total third party votes
thirdParty <- election$johnsonVotes + election$steinVotes + election$mcmullinVotes + election$othersVotes

# new data frame with all analytical parameters
electionAnalysis <- cbind(election, electorsPerCitizen, electorsPerVoter, voterTurnout, marginOfVictory, marginOfVictoryPercent, thirdParty)
write.csv(electionAnalysis, '2016ElectionAnalysis.csv')

# list states where third party votes exceed margin of victory
electionAnalysis[electionAnalysis$thirdParty > electionAnalysis$marginOfVictory,]$state
electionAnalysis[electionAnalysis$steinVotes > electionAnalysis$marginOfVictory,]$state
electionAnalysis[electionAnalysis$johnsonVotes > electionAnalysis$marginOfVictory,]$state
electionAnalysis[electionAnalysis$mcmullinVotes > electionAnalysis$marginOfVictory,]$state
electionAnalysis[electionAnalysis$othersVotes > electionAnalysis$marginOfVictory,]$state

# third party totals
stein <- sum(electionAnalysis$steinVotes)
johnson <- sum(electionAnalysis$johnsonVotes)
mcmullin <- sum(electionAnalysis$mcmullinVotes)
other <- sum(electionAnalysis$othersVotes)
third <- sum(electionAnalysis$thirdParty)
popDiff <- sum(electionAnalysis$clintonVotes) - sum(electionAnalysis$trumpVotes)

# election results
electorMargin <- sum(electionAnalysis$trumpElectors) - sum(electionAnalysis$clintonElectors)

# could stein votes swing 3 red states and change the electoral college win to Clinton?
sum(electionAnalysis[election$state %in% 
                       c('Michigan', 'Pennsylvania', 'Wisconsin'),]$electors2016)*2 > electorMargin

# correlations
cor(electorsPerCitizen, electorsPerVoter)
cor(electorsPerCitizen, voterTurnout)
cor(electorsPerCitizen, marginOfVictoryPercent)
cor(voterTurnout, marginOfVictoryPercent)
cor(electionAnalysis$electorsPerCitizen, electionAnalysis$population2010)
cor(electionAnalysis$marginOfVictoryPercent, electionAnalysis$population2010)

# top states per metric

head(electionAnalysis[order(electionAnalysis$voterTurnout, decreasing = FALSE),])
head(electionAnalysis[order(electionAnalysis$voterTurnout, decreasing = TRUE),])
head(electionAnalysis[order(electionAnalysis$marginOfVictoryPercent, decreasing = TRUE),])
head(electionAnalysis[order(electionAnalysis$marginOfVictoryPercent, decreasing = FALSE),])
head(electionAnalysis[order(electionAnalysis$electorsPerCitizen, decreasing = TRUE),])
head(electionAnalysis[order(electionAnalysis$electorsPerVoter, decreasing = TRUE),])
head(electionAnalysis[order(electionAnalysis$electorsPerCitizen, decreasing = FALSE),])
head(electionAnalysis[order(electionAnalysis$electorsPerVoter, decreasing = FALSE),])

# linear model - needs normalizing to be valid, this is just a rough run

model <- lm(electionAnalysis$voterTurnout ~ 
              electionAnalysis$population2010 + 
              electionAnalysis$marginOfVictoryPercent +
              electionAnalysis$electorsPerCitizen)
summary(model)

model <- lm(electionAnalysis$electorsPerCitizen ~ 
              electionAnalysis$population2010 + 
              electionAnalysis$marginOfVictoryPercent)
summary(model)

# import expenditures data

spend <- read_csv('expenditures.csv')
spendVA <- spend[spend$recipient_st=='VA',]
spendTrump <- spend[spend$cand_nm == 'Trump, Donald J.',]
spendClinton <- spend[spend$cand_nm == 'Clinton, Hillary Rodham',]

candidate_spending <- spend %>%
  group_by(cand_nm, recipient_st, election_tp) %>%
  summarize(total = sum(disb_amt)) %>%
  filter(election_tp %in% (c('P2016', 'G2016')),
         cand_nm %in% c('Trump, Donald J.', 'Clinton, Hillary Rodham')) %>%
  spread(election_tp, total) %>%
  mutate(T2016 = sum(P2016, G2016, na.rm = TRUE)) 

primary_spend_by_state <- candidate_spending %>%
  select(cand_nm, recipient_st, P2016) %>%
  filter(recipient_st %in% electionAnalysis$postal) %>%
  spread(cand_nm, P2016) %>%
  mutate(clintonExpPrimary = `Clinton, Hillary Rodham`,
         trumpExpPrimary = `Trump, Donald J.`) %>%
  select(recipient_st, clintonExpPrimary, trumpExpPrimary)

general_spend_by_state <- candidate_spending %>%
  select(cand_nm, recipient_st, G2016) %>%
  filter(recipient_st %in% electionAnalysis$postal) %>%
  spread(cand_nm, G2016) %>%
  mutate(clintonExpGeneral = `Clinton, Hillary Rodham`,
         trumpExpGeneral = `Trump, Donald J.`) %>%
  select(recipient_st, clintonExpGeneral, trumpExpGeneral)

total_spend_by_state <- candidate_spending %>%
  select(cand_nm, recipient_st, T2016) %>%
  filter(recipient_st %in% electionAnalysis$postal) %>%
  spread(cand_nm, T2016) %>%
  mutate(clintonExpTotal = `Clinton, Hillary Rodham`,
         trumpExpTotal = `Trump, Donald J.`) %>%
  select(recipient_st, clintonExpTotal, trumpExpTotal)

spend_by_state <- primary_spend_by_state %>%
  full_join(general_spend_by_state) %>%
  full_join(total_spend_by_state) %>%
  mutate(postal = recipient_st)

# list of states
states <- sort(unique(spend$recipient_st))

stateTotal <- function(state, candidate, election = 'G2016') {
  if (is.na(election)) {
    electionSpend <- spend
  } else {
    electionSpend <- spend[spend$election_tp==election,]
  }
  candSpend <- electionSpend[electionSpend$cand_nm==candidate,]
  stateSpend <- candSpend[candSpend$recipient_st==state,]
  return(sum(stateSpend$disb_amt))
}

# trump by state - overall
total <- sapply(states, stateTotal, 'Trump, Donald J.', election_tp = NA)
trumpByState <- data.frame()
trumpByState <- cbind(as.character(states), total)
colnames(trumpByState) <- c('state', 'trumpExpTotal')

# trump by state - primary
total <- sapply(states, stateTotal, 'Trump, Donald J.', election_tp = 'P2016')
trumpByStatePrimary <- data.frame()
trumpByStatePrimary <- cbind(as.character(states), total)
colnames(trumpByStatePrimary) <- c('state', 'trumpExpPrimary')

# trump by state - general
total <- sapply(states, stateTotal, 'Trump, Donald J.', election_tp = 'G2016')
trumpByStateGeneral <- data.frame()
trumpByStateGeneral <- cbind(as.character(states), total)
colnames(trumpByStateGeneral) <- c('state', 'trumpExpGeneral')

# clinton by state - overall
total <- sapply(states, stateTotal, 'Clinton, Hillary Rodham', election_tp = NA)
clintonByState <- data.frame()
clintonByState <- cbind(as.character(states), total)
colnames(clintonByState) <- c('state', 'clintonExpTotal')

# clinton by state - primary
total <- sapply(states, stateTotal, 'Clinton, Hillary Rodham', election_tp = 'P2016')
clintonByStatePrimary <- data.frame()
clintonByStatePrimary <- cbind(as.character(states), total)
colnames(clintonByStatePrimary) <- c('state', 'clintonExpPrimary')

# clinton by state - general
total <- sapply(states, stateTotal, 'Clinton, Hillary Rodham', election_tp = 'G2016')
clintonByStateGeneral <- data.frame()
clintonByStateGeneral <- cbind(as.character(states), total)
colnames(clintonByStateGeneral) <- c('state', 'clintonExpGeneral')

# merge electionAnalysis with expenditures info
expendituresTrump <- merge(trumpByStateGeneral,
                           trumpByStatePrimary,
                           by.x = 'state',
                           by.y = 'state',
                           all = TRUE)

expendituresClinton <- merge(clintonByStateGeneral,
                             clintonByStatePrimary,
                             by.x = 'state',
                             by.y = 'state',
                             all = TRUE)

expendituresTotal <- merge(trumpByState,
                           clintonByState,
                           by.x = 'state',
                           by.y = 'state',
                           all = TRUE)

expendituresBoth <- merge(expendituresClinton,
                          expendituresTrump,
                          by.x = 'state',
                          by.y = 'state',
                          all = TRUE)

expenditures <- merge(expendituresBoth,
                      expendituresTotal,
                      by.x = 'state',
                      by.y = 'state',
                      all = TRUE)

# electionExpenditureAnalysis <- merge(electionAnalysis,
#                                      expenditures,
#                                      by.x = 'postal',
#                                      by.y = 'state',
#                                      all.x = TRUE,
#                                      all.y = FALSE)

electionExpenditureAnalysis <- electionAnalysis %>%
  full_join(spend_by_state)

write_csv(electionExpenditureAnalysis, '2016ElectionExpenditureAnalysis.csv')

# visualizations

# candidate spending by state
ggplot(data = electionExpenditureAnalysis) +
  geom_col(aes(x = state, 
               y = as.numeric(as.character(clintonExpTotal)), 
               fill = postal)) +
  coord_flip() +
  xlab('State') +
  ylab('Dollars spent during 2016 presidential election')


ggplot(data = electionExpenditureAnalysis) +
  geom_col(aes(x = state, 
               y = as.numeric(as.character(trumpExpTotal)), 
               fill = postal)) +
  coord_flip() +
  xlab('State') +
  ylab('Dollars spent during 2016 presidential election')

# state populations
ggplot(data = electionExpenditureAnalysis) +
  geom_col(aes(x = state, 
               y = population2010, 
               fill = postal)) +
  coord_flip()

# electors per citizen
electionExpenditureAnalysis %>%
  mutate(state = reorder(state, electorsPerCitizen)) %>%
  ggplot() +
    geom_col(aes(x = state, 
                 y = electorsPerCitizen, 
                 fill = state)) +
    xlab('State') +
    ylab(paste('Electors per million citizens')) +
    ggtitle('A citizen\'s share of the electoral college, by state') +
    theme(legend.position="none") +
    coord_flip()

# electors per voter in 2016
electionExpenditureAnalysis %>%
  mutate(state = reorder(state, electorsPerVoter)) %>%
  ggplot() +
    geom_col(aes(x = state, 
                 y = electorsPerVoter, 
                 fill = state)) +
    xlab('State') +
    ylab(paste('Electors per million votes')) +
    ggtitle('A vote\'s share of the electoral college, by state') +
    theme(legend.position="none") +
    coord_flip()

# voter turnout
# electors per citizen
electionExpenditureAnalysis %>%
  mutate(state = reorder(state, voterTurnout)) %>%
  ggplot() +
  geom_col(aes(x = state, 
               y = voterTurnout, 
               fill = state)) +
  xlab('State') +
  ylab(paste('Voter turnout (votes / citizens)')) +
  ggtitle('Voter turnout') +
  theme(legend.position="none") +
  coord_flip()

# margin of victory
electionExpenditureAnalysis %>%
  mutate(state = reorder(state, -marginOfVictoryPercent)) %>%
  ggplot() +
    geom_col(aes(x = state, 
                 y = marginOfVictoryPercent, 
                 fill = state)) +
    xlab('State') +
    ylab(paste('Margin of victory (percent of vote)')) +
    ggtitle('Margin of victory, by state') +
    theme(legend.position="none") +
    coord_flip()

electionExpenditureAnalysis %>%
  mutate(state = reorder(state, -marginOfVictory)) %>%
  ggplot() +
  geom_col(aes(x = state, 
               y = marginOfVictory, 
               fill = state)) +
  xlab('State') +
  ylab(paste('Margin of victory (raw vote count)')) +
  ggtitle('Margin of victory, by state') +
  theme(legend.position="none") +
  coord_flip()

electionExpenditureAnalysis %>%
  mutate(expTotal = as.numeric(clintonExpTotal) + as.numeric(trumpExpTotal),
    state = reorder(state, expTotal)) %>%
  ggplot() +
  geom_col(aes(x = state, 
               y = expTotal, 
               fill = state)) +
  xlab('State') +
  ylab('Total campaign expenditures through Dec 31, 2016') +
  ggtitle('Campaign expenditures, by state') +
  theme(legend.position="none") +
  coord_flip()

electionExpenditureAnalysis %>%
  mutate(expTotal = as.numeric(clintonExpGeneral) + as.numeric(trumpExpGeneral),
         state = reorder(state, expTotal)) %>%
  ggplot() +
  geom_col(aes(x = state, 
               y = expTotal, 
               fill = state)) +
  xlab('State') +
  ylab('General election campaign expenditures through Dec 31, 2016') +
  ggtitle('Campaign expenditures, by state') +
  theme(legend.position="none") +
  coord_flip()

# 2D: margin of victory * number of electors
electionExpenditureAnalysis %>%
  #filter(!postal %in% c('CA', 'NY', 'TX')) %>%
  ggplot(aes(x = electors2016, 
             y = marginOfVictory, 
             color = postal)) +
  geom_jitter() +
  geom_text(aes(label = postal), check_overlap = TRUE, vjust = 1.5) +
  xlab('Number of electors') +
  ylab(paste('Margin of victory (raw vote count)')) +
  ggtitle('Margin of victory (raw) vs. number of electors') +
  theme(legend.position="none") +
  coord_flip()

# 2D: margin of victory * turnout
electionExpenditureAnalysis %>%
  ggplot(aes(x = as.numeric(voterTurnout), 
             y = as.numeric(marginOfVictory), 
             color = postal)) +
  geom_jitter() +
  geom_text(aes(label = postal), check_overlap = TRUE, vjust = 1.5) +
  geom_smooth() +
  xlab('Voter turnout (votes / citizens)') +
  ylab(paste('Margin of victory (raw vote count)')) +
  ggtitle('Margin of victory (raw) vs. voter turnout') +
  theme(legend.position="none")



# total trump/clinton expenditures
ggplot(data = electionExpenditureAnalysis) +
  geom_col(aes(x = state, 
               y = as.numeric(as.character(clintonExpTotal)) + as.numeric(as.character(trumpExpTotal)), 
               fill = postal)) +
  xlab('State') +
  ylab('Total expenditures by Trump & Clinton in primary and general election') +
  coord_flip()

# correlations
cor(as.numeric(as.character(electionExpenditureAnalysis$clintonExpTotal)) + as.numeric(as.character(electionExpenditureAnalysis$trumpExpTotal)),
    electionExpenditureAnalysis$electors2016)
cor(as.numeric(as.character(electionExpenditureAnalysis$clintonExpTotal)) + as.numeric(as.character(electionExpenditureAnalysis$trumpExpTotal)),
    electionExpenditureAnalysis$electorsPerCitizen)
cor(as.numeric(as.character(electionExpenditureAnalysis$clintonExpTotal)) + as.numeric(as.character(electionExpenditureAnalysis$trumpExpTotal)),
    electionExpenditureAnalysis$electorsPerVoter)
cor(as.numeric(as.character(electionExpenditureAnalysis$clintonExpTotal)) + as.numeric(as.character(electionExpenditureAnalysis$trumpExpTotal)),
    electionExpenditureAnalysis$voterTurnout)

# various comparative visualizations

electionExpenditureAnalysis %>%
  filter(postal != 'DC') %>%
ggplot(aes(trumpExpTotal, clintonVotes)) +
  geom_point() +
  geom_smooth()

electionExpenditureAnalysis %>%
  #filter(!postal %in% c('VA', 'DC')) %>%
  mutate(totalExp = clintonExpTotal + trumpExpTotal) %>%
  mutate(state = reorder(state, totalExp)) %>%
  ggplot(aes(state, totalExp)) +
  geom_col() +
  coord_flip()

electionExpenditureAnalysis %>%
  filter(postal != 'DC') %>%
  mutate(totalExp = clintonExpTotal + trumpExpTotal) %>%
  ggplot(aes(totalExp, electors2016)) +
  geom_point() +
  geom_smooth() +
  xlab('Total expenditures (primary and general) by Trump and Clinton') +
  ylab('Electors per state')

electionExpenditureAnalysis %>%
  filter(postal != 'DC') %>%
  mutate(totalExp = clintonExpTotal + trumpExpTotal) %>%
  ggplot(aes(totalExp, electorsPerCitizen)) +
  geom_point() +
  geom_smooth() +
  xlab('Total expenditures (primary and general) by Trump and Clinton') +
  ylab('Electors per citizen per state')

electionExpenditureAnalysis %>%
  filter(postal != 'DC') %>%
  mutate(totalExp = clintonExpGeneral + trumpExpGeneral) %>%
  ggplot(aes(totalExp, electorsPerCitizen)) +
  geom_point() +
  geom_smooth() +
  xlab('Total expenditures (general election) by Trump and Clinton') +
  ylab('Electors per citizen per state')

electionExpenditureAnalysis %>%
  ggplot(aes(marginOfVictoryPercent, electorsPerCitizen)) +
  geom_point() +
  geom_smooth() +
  xlab('Margin of victory (percent)') +
  ylab('Electors per citizen') +
  ggtitle('How much is a vote worth? (2016 US presidential election)')

electionExpenditureAnalysis %>%
  filter(postal != 'DC') %>%
  mutate(totalExp = clintonExpTotal + trumpExpTotal) %>%
  ggplot(aes(totalExp, marginOfVictory)) +
  geom_point() +
  geom_smooth() +
  xlab('Total expenditures (primary and general) by Trump and Clinton') +
  ylab('Margin of victory (raw vote count)')

electionExpenditureAnalysis %>%
  filter(postal != 'DC') %>%
  mutate(totalExp = clintonExpTotal + trumpExpTotal) %>%
  ggplot(aes(totalExp, marginOfVictoryPercent)) +
  geom_point() +
  geom_smooth() +
  xlab('Total expenditures (primary and general) by Trump and Clinton') +
  ylab('Margin of victory (percent)')

