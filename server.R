# 2016 election analysis - Shiny App server script

# Import ggplot and data table created by 2016Election.R
library('ggplot2')
election <- data.frame()
election <- read.csv('2016ElectionExpenditureAnalysis.csv')
  
# Server backend
subtable <- function(subset_parameter) {
  return(subset(election, select = c('state', subset_parameter)))
}

shinyServer(
  function(input, output) {
    
    # Plot of general state-by-state election data
    plot_parameter <- reactive({
      switch(input$parameter,
             'Population' = election$population2010, 
             'Electors' = election$electors2016,
             'Votes' = election$totalVotes, 
             'Voter turnout' = election$voterTurnout,
             'Electors per citizen' = election$electorsPerCitizen,
             'Electors per voter' = election$electorsPerVoter,
             'Margin of victory' = election$marginOfVictoryPercent,
             'Trump & Clinton Expenditures' = as.numeric(as.character(election$clintonExpTotal)) + as.numeric(as.character(election$trumpExpTotal)))
    })
    
    output$state_by_state_plot <- renderPlot({
      plot_par <- plot_parameter()
      ggplot() +
        geom_col(aes(x = election$state, 
                     y = plot_par,
                     fill = election$postal)) +
        xlab('State') +
        ylab(input$parameter) +
        coord_flip() +
        theme(legend.position="none")
    })
    
    # Table of new domain signups, by user-selected timeframe parameter
    output$state_by_state_table <- renderDataTable(
      {
        subset(election, select = c('population2010', 
                   'electors2016',
                   'totalVotes', 
                   'voterTurnout',
                   'electorsPerCitizen',
                   'electorsPerVoter',
                   'marginOfVictoryPercent',
                   'clintonExpTotal',
                   'trumpExpTotal'))
        },
      options = list(lengthMenu = c(12, 24, 36), pageLength = 12)
    )
  }
)