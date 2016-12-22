# 2016 election analysis - Shiny App UI script

library(shinydashboard)

# Title header
header <- dashboardHeader(
  title = '2016 Presidential Election',
  titleWidth = 300
)

# Sidebar with data sources
sidebar <- dashboardSidebar(
  width = 300,
  div(
    style='margin: 12px',
    p('State-by-state results from the 2016 US presidential election, with information about state populations, electors, and campaign expenditures.'),
    p('Find the source code and cleaned data on ', 
      a(href='https://github.com/kshaffer/election2016', 'GitHub.', style='text-decoration: underline')
    ),
    p('Data sources:'),
    tags$ul(
      tags$li(a(href='https://en.wikipedia.org/wiki/United_States_presidential_election,_2016', 
                'Election results', 
                style='text-decoration: underline',
                target='_blank')),
      tags$li(a(href='https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population', 
                'State populations', 
                style='text-decoration: underline',
                target='_blank'
                )),
      tags$li(a(href='https://en.wikipedia.org/wiki/Electoral_College_(United_States)', 
                'Electoral college', 
                style='text-decoration: underline',
                target='_blank'
                )),
      tags$li(a(href='http://www.fec.gov/disclosurep/pnational.do',
                'Expenditures',
                style='text-decoration: underline',
                target='_blank'
      ))
    )
  )
  )

# Main dashboard layout
body <- dashboardBody(
  # Title header
  fluidRow(
    h1(strong('2016 US presidential election'), align='center'),
    br()
  ),
  
  fluidRow(
    column(8,
      
      # New registrations over time - plot/table tabs selected by user
      plotOutput('state_by_state_plot')
    ),
    column(4,
       # Menus for user-selected time frame and status filters
       wellPanel(
         selectInput('parameter',
                     label = 'Data parameter',
                     choices = c('Population', 
                                 'Electors',
                                 'Votes', 
                                 'Voter turnout',
                                 'Electors per citizen',
                                 'Electors per voter',
                                 'Margin of victory',
                                 'Trump & Clinton Expenditures'),
                     selected = 'Population')
       )
    )
  )#,
  
  # fluidRow(
  #   dataTableOutput('state_by_state_table')
  # )
)


# Build the dashboard page
shinyUI(
  dashboardPage(header, sidebar, body)
)


