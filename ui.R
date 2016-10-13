#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


ui <- shinyUI(navbarPage(a(href="http://shiny.evanodell.com", "Return to Shiny Home"),

                       tabPanel("Turnout Tool",
                                  
                                  fluidPage(
                                    
                                    h3("England Electoral Turnout"),
                                    
                                    p("A tool to estimate what changes in turnout do to electoral results. For details please see the methods tab."),
                                    fluidRow(
                                      column(6,
                                    sliderInput(inputId = "turnOut", label = "Voter Turnout", 
                                                min=17.2, max=88.5, value=65.8, 
                                                step = 0.1, ticks = TRUE, animate = FALSE,
                                                width = NULL),
                                    selectInput(inputId = "distro", 
                                                 label = "Select a distribution",
                                                 choices = c("Uniform", "Turnout", "Marginality"),
                                                 selected = "Uniform"),
                                    br(),
                                    h3("Seats in England"),
                                    htmlOutput("winner")     
                                    
                                    ),
                                      column(6,
                                        numericInput("toryNon", "Percentage of Non-Voters Supporting the Conservatives",
                                                     20,min=1,max=100),
                                        numericInput("greenNon", "Percentage of Non-Voters Supporting the Greens",
                                                     5,min=1,max=100),
                                        numericInput("labourNon", "Percentage of Non-Voters Supporting Labour",
                                                     50,min=1,max=100),
                                        numericInput("libdemNon", "Percentage of Non-Voters Supporting the Liberal Democrats",
                                                     10,min=1,max=100),
                                        numericInput("ukipNon", "Percentage of Non-Voters Supporting Ukip",
                                                     15,min=1,max=100)                                        
                                             )
                                    ),
                                    br(),
                                    h4("Total Votes"),
                                    fluidRow(
                                    column(2,
                                           tableOutput("votes")
                                          )
                                        ),
                                    fluidRow(#Total number of votes per party
                                      column(2),
                                      column(2,
                                             h4("Conservative")
                                      ),
                                      column(2,
                                             h4("Green")
                                      ),
                                      column(2,
                                             h4("Labour")
                                      ),
                                      column(2,
                                             h4("Liberal Democrats")
                                      ),
                                      column(2,
                                             h4("Ukip")
                                      )
                                    ),
                                                                        
                                    fluidRow(#vote share per party
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
                                    column(6,
                                           includeMarkdown("methods.rmd")
                                    )
                                  )
                         ),
                         tabPanel("Discussion",
                                  fluidRow(column(1)),
                                  fluidRow(
                                    column(6,
                                           includeMarkdown("discussion.rmd")
                                    )
                                  )
                         )
))
