#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage(tags$a(href="http://evanodell.com", "Home"),
###Plan
  #1. Allow for weighting of turnout relative to existing levels of turnout. Lower turnout = increased growth
  #2. what other models are there?
                       tabPanel("Turnout Tool",
                                  
                                  fluidPage(
                                    
                                    h3("England Electoral Turnout"),
                                    
                                    p("A tool to estimate what changes in turnout do to electoral results. For details please see the methods tab."),
                                    
                                    sliderInput(inputId = "turnOut", label = "Voter Turnout", 
                                                min=17.2, max=88.5, value=65.8, 
                                                step = 0.1, ticks = TRUE, animate = FALSE,
                                                width = NULL),
                                    selectInput(inputId = "distro", 
                                                 label = "What distribution would you like?",
                                                 choices = c("Uniform", "Weighted"),
                                                 selected = "Uniform"),
                                    # plotOutput("map"),
                                    fluidRow(
                                      column(6,
                                             tableOutput("winner")
                                      )
                                      
                                      ),
                                    br(),
                                    h4("Total Votes"),
                                    fluidRow(
                                    column(2,
                                           tableOutput("votes")
                                          )
                                        ),
                                    fluidRow(
                                      column(2),
                                      column(2,
                                             h4("Conservative")
                                             #, textOutput("toryShare")
                                      ),
                                      column(2,
                                             h4("Green")
                                             #, textOutput("greenShare")
                                      ),
                                      column(2,
                                             h4("Labour")
                                             #, textOutput("labourShare")
                                      ),
                                      column(2,
                                             h4("Liberal Democrats")
                                             #, textOutput("libdemShare")
                                      ),
                                      column(2,
                                             h4("Ukip")
                                             #, textOutput("ukipShare")
                                      )
                                    ),
                                                                        
                                    fluidRow(
                                      column(2,
                                             h4("Vote Share")),
                                      column(2,
                                             textOutput("toryShare")
                                      ),
                                      column(2,
                                             textOutput("greenShare")
                                      ),
                                      column(2,
                                             textOutput("labourShare")
                                      ),
                                      column(2,
                                             textOutput("libdemShare")
                                      ),
                                      column(2,
                                             textOutput("ukipShare")
                                      )
                                    ),
                                    br(),
                                    fluidRow(
                                      column(2,
                                             h4("Total Votes")),
                                      column(2,
                                             #p("Conservative"), 
                                             tableOutput("tory")
                                      ),
                                      column(2,
                                             #p("Green"), 
                                             tableOutput("green")
                                      ),
                                      column(2,
                                             #p("Labour"), 
                                             tableOutput("labour")
                                      ),
                                      column(2,
                                             #p("Liberal Democrats"), 
                                             tableOutput("libdem")
                                      ),
                                      column(2,
                                             #p("Ukip"), 
                                             tableOutput("ukip")
                                      )
                                    )
                                  )
                              ),
                                        
                         tabPanel("Methods",
                                  fluidRow(
                                    column(10,
                                           includeMarkdown("methods.rmd")
                                    )
                                  )
                         ),
                         tabPanel("Discussion",
                                  fluidRow(
                                    column(10,
                                           includeMarkdown("discussion.rmd")
                                    )
                                  )
                         )
))
