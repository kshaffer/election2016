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
electionAnalysis = cbind(election, electorsPerCitizen, electorsPerVoter, voterTurnout, marginOfVictory, marginOfVictoryPercent, thirdParty)
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
