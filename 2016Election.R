library(ggplot2)

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

spend <- data.frame()
spend <- read.csv('expenditures.csv')
spendVA <- spend[spend$recipient_st=='VA',]
spendTrump <- spend[spend$cand_nm == 'Trump, Donald J.',]
spendClinton <- spend[spend$cand_nm == 'Clinton, Hillary Rodham',]

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
total <- sapply(states, stateTotal, 'Trump, Donald J.', election = NA)
trumpByState <- data.frame()
trumpByState <- cbind(as.character(states), total)
colnames(trumpByState) <- c('state', 'trumpExpTotal')

# trump by state - primary
total <- sapply(states, stateTotal, 'Trump, Donald J.', election = 'P2016')
trumpByStatePrimary <- data.frame()
trumpByStatePrimary <- cbind(as.character(states), total)
colnames(trumpByStatePrimary) <- c('state', 'trumpExpPrimary')

# trump by state - general
total <- sapply(states, stateTotal, 'Trump, Donald J.', election = 'G2016')
trumpByStateGeneral <- data.frame()
trumpByStateGeneral <- cbind(as.character(states), total)
colnames(trumpByStateGeneral) <- c('state', 'trumpExpGeneral')

# clinton by state - overall
total <- sapply(states, stateTotal, 'Clinton, Hillary Rodham', election = NA)
clintonByState <- data.frame()
clintonByState <- cbind(as.character(states), total)
colnames(clintonByState) <- c('state', 'clintonExpTotal')

# clinton by state - primary
total <- sapply(states, stateTotal, 'Clinton, Hillary Rodham', election = 'P2016')
clintonByStatePrimary <- data.frame()
clintonByStatePrimary <- cbind(as.character(states), total)
colnames(clintonByStatePrimary) <- c('state', 'clintonExpPrimary')

# clinton by state - general
total <- sapply(states, stateTotal, 'Clinton, Hillary Rodham', election = 'G2016')
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

electionExpenditureAnalysis <- merge(electionAnalysis,
                                     expenditures,
                                     by.x = 'postal',
                                     by.y = 'state',
                                     all.x = TRUE,
                                     all.y = FALSE)

write.csv(electionExpenditureAnalysis, '2016ElectionExpenditureAnalysis.csv', row.names = FALSE)

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
ggplot(data = electionExpenditureAnalysis) +
  geom_col(aes(x = state, 
               y = electorsPerCitizen, 
               fill = postal)) +
  xlab('State') +
  ylab('Electors per million citizens') +
  coord_flip()

# electors per voter in 2016
ggplot(data = electionExpenditureAnalysis) +
  geom_col(aes(x = state, 
               y = electorsPerVoter, 
               fill = postal)) +
  xlab('State') +
  ylab('Electors per million voters in 2016 presidential election') +
  coord_flip()

# voter turnout
# electors per citizen
ggplot(data = electionExpenditureAnalysis) +
  geom_col(aes(x = state, 
               y = voterTurnout, 
               fill = postal)) +
  xlab('State') +
  ylab('Voter turnout (voters / all citizens)') +
  coord_flip()

# margin of victory
ggplot(data = electionExpenditureAnalysis) +
  geom_col(aes(x = state, 
               y = marginOfVictoryPercent, 
               fill = postal)) +
  xlab('State') +
  ylab('Margin of victory') +
  coord_flip()

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

# linear model
fit <- lm(as.numeric(as.character(electionExpenditureAnalysis$clintonExpTotal)) + as.numeric(as.character(electionExpenditureAnalysis$trumpExpTotal)) ~
          electionExpenditureAnalysis$electors2016 + 
            electionExpenditureAnalysis$voterTurnout)
summary(fit)
