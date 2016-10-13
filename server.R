#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(rmarkdown)


cont2 <- readRDS(file = "cont2.rds")

#Home
#setwd("~/Google Drive/Study Guides/Statistics/R/Shiny/Test1/England-TurnOut")

#Work
#setwd("C:/Users/eodell/Google Drive/Study Guides/Statistics/R/Shiny/Test1/England-TurnOut")

# Define server logic 
server <- function(input, output, session){
  
  values <- reactiveValues(df=NULL)
  values$df <- data.frame(
    constituency = cont2$constituency,
    constituencyID = cont2$constituencyID,
    labour = cont2$labour,
    tory = cont2$tory,
    green = cont2$green,
    libdem = cont2$libdem,
    ukip = cont2$ukip,
    labourwin = cont2$labourwin,
    torywin = cont2$torywin,
    greenwin = cont2$greenwin,
    libdemwin = cont2$libdemwin,
    ukipwin = cont2$ukipwin,
    winner = cont2$winner,
    spare = cont2$spare,
    electorate = cont2$electorate
  )
  
  observe({
    if(input$distro=="Uniform"){#Distribution of turnout increases or decreases are evenly spread across all constituencies
      #If turnout is equal to greater than 65.8
      newData <- reactive({
        values$df$labour <- cont2$labour+ ((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$labourNon/100))
        values$df$tory <- cont2$tory + ((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$toryNon/100))
        values$df$green <- cont2$green  + ((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$greenNon/100))
        values$df$libdem <- cont2$libdem +((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$libdemNon/100))
        values$df$ukip <- cont2$ukip + ((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$ukipNon/100))
        values$df$votes <- cont2$votes + (((input$turnOut - 65.8)/100)*cont2$electorate)
        values$df$labourwin <- as.factor(ifelse(values$df$labour>values$df$tory &
                                                  values$df$labour>values$df$green &
                                                  values$df$labour>values$df$libdem & 
                                                  values$df$labour>values$df$ukip, TRUE,FALSE))
        
        values$df$torywin <- as.factor(ifelse(values$df$tory>values$df$green &
                                                values$df$tory>values$df$labour &
                                                values$df$tory>values$df$libdem & 
                                                values$df$tory>values$df$ukip, TRUE,FALSE))
        
        values$df$greenwin <- as.factor(ifelse(values$df$green>values$df$tory &
                                                 values$df$green>values$df$labour &
                                                 values$df$green>values$df$libdem & 
                                                 values$df$green>values$df$ukip, TRUE,FALSE))
        
        values$df$libdemwin <- as.factor(ifelse(values$df$libdem>values$df$tory &
                                                  values$df$libdem>values$df$green &
                                                  values$df$libdem>values$df$labour & 
                                                  values$df$libdem>values$df$ukip, TRUE,FALSE))
        
        values$df$ukipwin <- as.factor(ifelse(values$df$ukip>values$df$tory &
                                                values$df$ukip>values$df$green &
                                                values$df$ukip>values$df$labour & 
                                                values$df$ukip>values$df$libdem, TRUE,FALSE))
        
        values$df$winner <- as.factor(ifelse(values$df$torywin==TRUE,"Conservatives",
                                             ifelse(values$df$greenwin==TRUE,"Green",
                                                    ifelse(values$df$labourwin==TRUE,"Labour",
                                                           ifelse(values$df$libdemwin==TRUE,"Liberal Democrats",
                                                                  ifelse(values$df$ukipwin==TRUE,"Ukip", NA))))))
      })
      #   output$winner <- renderTable({
      #    outcome <- as.data.frame(newData())
      #   summary(outcome)},
      #  striped=TRUE, rownames = FALSE, colnames=FALSE)
      
      
    }else if(input$distro=="Turnout"){
      #Distribution of turnout increases or decreases are weighted towards constituencies with low turnout 
      newData <- reactive({
        values$df$labour <- cont2$labour + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$labourNon/100))*cont2$turnStand)
        values$df$tory <- cont2$tory + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$toryNon/100))*cont2$turnStand)
        values$df$green <- cont2$green  + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$greenNon/100))*cont2$turnStand)
        values$df$libdem <- cont2$libdem + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$libdemNon/100))*cont2$turnStand)
        values$df$ukip <- cont2$ukip + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$ukipNon/100))*cont2$turnStand)
        values$df$votes <- cont2$votes + ((((input$turnOut - 65.8)/100)*cont2$electorate)*cont2$turnStand)
        values$df$labourwin <- as.factor(ifelse(values$df$labour>values$df$tory &
                                                  values$df$labour>values$df$green &
                                                  values$df$labour>values$df$libdem & 
                                                  values$df$labour>values$df$ukip, TRUE,FALSE))
        
        values$df$torywin <- as.factor(ifelse(values$df$tory>values$df$green &
                                                values$df$tory>values$df$labour &
                                                values$df$tory>values$df$libdem & 
                                                values$df$tory>values$df$ukip, TRUE,FALSE))
        
        values$df$greenwin <- as.factor(ifelse(values$df$green>values$df$tory &
                                                 values$df$green>values$df$labour &
                                                 values$df$green>values$df$libdem & 
                                                 values$df$green>values$df$ukip, TRUE,FALSE))
        
        values$df$libdemwin <- as.factor(ifelse(values$df$libdem>values$df$tory &
                                                  values$df$libdem>values$df$green &
                                                  values$df$libdem>values$df$labour & 
                                                  values$df$libdem>values$df$ukip, TRUE,FALSE))
        
        values$df$ukipwin <- as.factor(ifelse(values$df$ukip>values$df$tory &
                                                values$df$ukip>values$df$green &
                                                values$df$ukip>values$df$labour & 
                                                values$df$ukip>values$df$libdem, TRUE,FALSE))
        
        values$df$winner <- as.factor(ifelse(values$df$torywin==TRUE,"Conservatives",
                                             ifelse(values$df$greenwin==TRUE,"Green",
                                                    ifelse(values$df$labourwin==TRUE,"Labour",
                                                           ifelse(values$df$libdemwin==TRUE,"Liberal Democrats",
                                                                  ifelse(values$df$ukipwin==TRUE,"Ukip", NA))))))      
      })
      
    }else if(input$distro=="Marginality"){
      
      newData <- reactive({
        values$df$labour <- cont2$labour + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$labourNon/100))*cont2$marginStand)
        values$df$tory <- cont2$tory + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$toryNon/100))*cont2$marginStand)
        values$df$green <- cont2$green  + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$greenNon/100))*cont2$marginStand)
        values$df$libdem <- cont2$libdem + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$libdemNon/100))*cont2$marginStand)
        values$df$ukip <- cont2$ukip + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$ukipNon/100))*cont2$marginStand)
        values$df$votes <- cont2$votes + ((((input$turnOut - 65.8)/100)*cont2$electorate)*cont2$marginStand)
        values$df$labourwin <- as.factor(ifelse(values$df$labour>values$df$tory &
                                                  values$df$labour>values$df$green &
                                                  values$df$labour>values$df$libdem & 
                                                  values$df$labour>values$df$ukip, TRUE,FALSE))
        
        values$df$torywin <- as.factor(ifelse(values$df$tory>values$df$green &
                                                values$df$tory>values$df$labour &
                                                values$df$tory>values$df$libdem & 
                                                values$df$tory>values$df$ukip, TRUE,FALSE))
        
        values$df$greenwin <- as.factor(ifelse(values$df$green>values$df$tory &
                                                 values$df$green>values$df$labour &
                                                 values$df$green>values$df$libdem & 
                                                 values$df$green>values$df$ukip, TRUE,FALSE))
        
        values$df$libdemwin <- as.factor(ifelse(values$df$libdem>values$df$tory &
                                                  values$df$libdem>values$df$green &
                                                  values$df$libdem>values$df$labour & 
                                                  values$df$libdem>values$df$ukip, TRUE,FALSE))
        
        values$df$ukipwin <- as.factor(ifelse(values$df$ukip>values$df$tory &
                                                values$df$ukip>values$df$green &
                                                values$df$ukip>values$df$labour & 
                                                values$df$ukip>values$df$libdem, TRUE,FALSE))
        
        values$df$winner <- as.factor(ifelse(values$df$torywin==TRUE,"Conservatives",
                                             ifelse(values$df$greenwin==TRUE,"Green",
                                                    ifelse(values$df$labourwin==TRUE,"Labour",
                                                           ifelse(values$df$libdemwin==TRUE,"Liberal Democrats",
                                                                  ifelse(values$df$ukipwin==TRUE,"Ukip", NA))))))      
      })
    }
    
    output$winner <- renderTable({validate(
      need(input$toryNon+
             input$greenNon+
             input$labourNon+
             input$libdemNon+
             input$ukipNon == 100, 'Inputs must sum to 100%')
      
    )
      outcome <- newData()
      summary(outcome)},bordered=TRUE,
    striped=TRUE, rownames = TRUE, colnames=FALSE)
    
    output$tory <- renderText(
      formatC(round(sum(values$df$tory),digits=0), format="d", big.mark=',')
    )
    output$green <- renderText(
      formatC(round(sum(values$df$green),digits=0), format="d", big.mark=',')
    )
    output$labour <- renderText(
      formatC(round(sum(values$df$labour),digits=0), format="d", big.mark=',')
    )
    output$libdem <- renderText(
      formatC(round(sum(values$df$libdem),digits=0), format="d", big.mark=',')
    )
    output$ukip <- renderText(
      formatC(round(sum(values$df$ukip),digits=0), format="d", big.mark=',')
    )
    output$electorate <- renderText(
      formatC(round(sum(values$df$electorate),digits=0), format="d", big.mark=',')
    )
    
    output$toryShare <- renderText({paste(round(
      (sum(values$df$tory)/sum(values$df$electorate))*100,digits=2),"%",sep="")
    })
    
    output$greenShare <- renderText({paste(round(
      (sum(values$df$green)/sum(values$df$electorate))*100,digits=2),"%",sep="")
    })
    
    output$labourShare <- renderText({paste(round(
      (sum(values$df$labour)/sum(values$df$electorate))*100,digits=2),"%",sep="")
    })
    
    output$libdemShare <- renderText({paste(round(
      (sum(values$df$libdem)/sum(values$df$electorate))*100,digits=2),"%",sep="")
    })
    
    output$ukipShare <- renderText({paste(round(
      (sum(values$df$ukip)/sum(values$df$electorate))*100,digits=2),"%",sep="")
    })
  })
}


