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

ui <- shinyUI(navbarPage(a(href="http://shiny.evanodell.com", "Return to Shiny Home"),
                         
###Plan
  #1. Allow for weighting of turnout relative to existing levels of turnout. Lower turnout = increased growth
  #2. what other models are there?
                       tabPanel("Turnout Tool",
                                  
                                  fluidPage(
                                    
                                    includeCSS("style.css"),
                                    
                                    h3("England Electoral Turnout"),
                                    
                                    p("A tool to estimate what changes in turnout do to electoral results. For details please see the methods tab."),
                                    
                                    fluidRow(
                                    column(4,
                                    
                                    sliderInput(inputId = "turnOut", label = "Voter Turnout", 
                                                min=17.2, max=88.5, value=65.8, 
                                                step = 0.1, ticks = TRUE, animate = FALSE,
                                                width = NULL),
                                    selectInput(inputId = "distro", 
                                                label = "Select a distribution",
                                                choices = c("Uniform", "Turnout", "Marginality"),
                                                selected = "Uniform"),
                                    htmlOutput("table1")
                                    ),

                                    column(6,
                                             numericInput("toryNon", "% of Non-Voters Supporting the Conservatives",
                                                         20,min=1,max=100),
                                             numericInput("greenNon", "% of Non-Voters Supporting the Greens",
                                                          5,min=1,max=100),
                                             numericInput("labourNon", "% of Non-Voters Supporting Labour",
                                                          50,min=1,max=100),
                                             numericInput("libdemNon", "% of Non-Voters Supporting the Liberal Democrats",
                                                          10,min=1,max=100),
                                             numericInput("ukipNon", "% of Non-Voters Supporting Ukip",
                                                          15,min=1,max=100)                                        
                                           )
                                           ),
                                    fluidRow(

                                    )
                                  ),
                                br(),
                                br()
                              ),
                                        
                         tabPanel("Methods",
                                  fluidRow(
                                    column(8,
                                           includeMarkdown("methods.rmd")
                                    )
                                  )
                         ),
                         tabPanel("Discussion",
                                  fluidRow(
                                    column(8,
                                           includeMarkdown("discussion.rmd")
                                    )
                                  )
                         )
))
