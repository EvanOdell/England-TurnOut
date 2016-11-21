#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Input on multiple tabs: http://stackoverflow.com/questions/21863898/r-shiny-multiple-use-in-ui-of-same-renderui-in-server
library(shiny)

# Define UI for application that draws a histogram

ui <- shinyUI(navbarPage(a(href="http://shiny.evanodell.com", "Return to Shiny Home"),
                         
                         ###Plan
                         #1. Allow for weighting of turnout relative to existing levels of turnout. Lower turnout = increased growth
                         #2. what other models are there?
                         tabPanel("Turnout Tool",
                                  
                                  fluidPage(tags$head(
                                    tags$style(HTML('#button{background-color:green;
                                                             font-weight:bold;}'))),   
                                    
                                    includeCSS("style.css"),
                                    
                                    h3("England Electoral Turnout"),
                                    
                                    p("A tool to estimate what changes in turnout do to electoral results. For details please see the methods tab."),
                                    
                                    fluidRow(
                                      column(4,
                                             
                                             sliderInput(inputId = "turnOut", label = "Voter Turnout", 
                                                         min=17.8, max=88.5, value=65.8, 
                                                         step = 0.1, ticks = TRUE, animate = FALSE,
                                                         width = NULL),
                                             actionButton("button", "Calculate Results"),
                                             
                                             selectInput(inputId = "distro", 
                                                         label = "Select a distribution: ",
                                                         choices = c("Uniform", "Turnout", "Marginality"),
                                                         selected = "Uniform"),
                                             h4("Results"),
                                             tableOutput("winner"),
                                             selectInput(inputId = "propType",
                                                         label = "Select a calculation of proportionality: ",
                                                         choices = c("Cox-Shugart", "D'Hont", "Farina", "Lijphart", "Grofman", "Sainte-Lague", "Rose", "Loosemore-Hanby", "Rae", "Gallagher"),
                                                         selected = "Gallagher"),
                                             h4("Proportionality Score"),
                                             textOutput("propOutput"),
                                             
                                             br(),
                                             h4("Select Voting Preference of Non-Voters:"),
                                             numericInput("toryNon", "% of Non-Voters Supporting the Conservatives",
                                                          20,min=0,max=100),
                                             numericInput("greenNon", "% of Non-Voters Supporting the Greens",
                                                          5,min=0,max=100),
                                             numericInput("labourNon", "% of Non-Voters Supporting Labour",
                                                          50,min=0,max=100),
                                             numericInput("libdemNon", "% of Non-Voters Supporting the Liberal Democrats",
                                                          10,min=0,max=100),
                                             numericInput("ukipNon", "% of Non-Voters Supporting Ukip",
                                                          15,min=0,max=100)   
                                      ),
                                      column(6,
                                             h3("Seats"),
                                             plotOutput("seatPlot", height = "425px"),
                                             h3("Votes"),
                                             plotOutput("votePlot", height = "425px")),
                                      br()
                                    ),
                                    
                                    fluidRow(
                                      column(6, offset=4,
                                             h4("Results and Changes"),
                                             HTML(paste(
                                               tags$table(
                                                 tags$tr(
                                                   tags$th(""),
                                                   tags$th("Seats"),
                                                   tags$th("Seat Share"),
                                                   tags$th("Votes"),
                                                   tags$th("Vote Share"),
                                                   tags$th("Seat Change"),
                                                   tags$th("Votes Change"),
                                                   tags$th("Vote Share Change")),
                                                 tags$tr(id="toryRow",
                                                         tags$th(h4("Conservatives")),
                                                         tags$td(textOutput("torySeats")),
                                                         tags$td(textOutput("torySeatShare")),
                                                         tags$td(textOutput("toryVotes")),
                                                         tags$td(textOutput("toryShare")),
                                                         tags$td(textOutput("torySeatChange")),
                                                         tags$td(textOutput("toryVoteChange")),
                                                         tags$td(textOutput("toryShareChange"))
                                                 ),
                                                 tags$tr(id="greenRow",
                                                         tags$th(h4("Green")),
                                                         tags$td(textOutput("greenSeats")),
                                                         tags$td(textOutput("greenSeatShare")),
                                                         tags$td(textOutput("greenVotes")),
                                                         tags$td(textOutput("greenShare")),
                                                         tags$td(textOutput("greenSeatChange")),
                                                         tags$td(textOutput("greenVoteChange")),
                                                         tags$td(textOutput("greenShareChange"))
                                                 ),
                                                 tags$tr(id="labourRow",
                                                         tags$th(h4("Labour")),
                                                         tags$td(textOutput("labourSeats")),
                                                         tags$td(textOutput("labourSeatShare")),
                                                         tags$td(textOutput("labourVotes")),
                                                         tags$td(textOutput("labourShare")),
                                                         tags$td(textOutput("labourSeatChange")),
                                                         tags$td(textOutput("labourVoteChange")),
                                                         tags$td(textOutput("labourShareChange"))
                                                 ),
                                                 tags$tr(id="libDemRow",
                                                         tags$th(h4("Liberal Democrats")),
                                                         tags$td(textOutput("libdemSeats")),
                                                         tags$td(textOutput("libdemSeatShare")),
                                                         tags$td(textOutput("libdemVotes")),
                                                         tags$td(textOutput("libdemShare")),
                                                         tags$td(textOutput("libdemSeatChange")),
                                                         tags$td(textOutput("libdemVoteChange")),
                                                         tags$td(textOutput("libdemShareChange"))
                                                 ),
                                                 tags$tr(id="ukipRow",
                                                         tags$th(h4("Ukip")),
                                                         tags$td(textOutput("ukipSeats")),
                                                         tags$td(textOutput("ukipSeatShare")),
                                                         tags$td(textOutput("ukipVotes")),
                                                         tags$td(textOutput("ukipShare")),
                                                         tags$td(textOutput("ukipSeatChange")),
                                                         tags$td(textOutput("ukipVoteChange")),
                                                         tags$td(textOutput("ukipShareChange"))
                                                 )
                                               )
                                             )
                                             )
                                      )),
                                    br()
                                    
                                  )
                         ),
                         
                         tabPanel("Methods",
                                  fluidPage(
                                    fluidRow(
                                      column(6,offset =3,
                                             includeMarkdown("methods.rmd")
                                      )
                                    ))
                         ),
                         tabPanel("Discussion",
                                  fluidPage(
                                    fluidRow(
                                      column(6,offset =3,
                                             includeMarkdown("discussion.rmd")
                                      )
                                    ))
                         )
))
