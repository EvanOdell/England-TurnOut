

library(shiny)
library(rmarkdown)
library(ggplot2)
library(dplyr)
library(scales)
library(SciencesPo)
library(readr)

cont2 <- read_rds("cont2.rds")

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
    other = cont2$other,
    labourwin = cont2$labourwin,
    torywin = cont2$torywin,
    greenwin = cont2$greenwin,
    libdemwin = cont2$libdemwin,
    ukipwin = cont2$ukipwin,
    otherwin = cont2$otherwin,
    winner = cont2$winner,
    spare = cont2$spare,
    electorate = cont2$electorate,
    votes = cont2$votes
  )
  
  
  observeEvent(input$button, {
    
    if(input$distro=="Uniform"){#Distribution of turnout increases or decreases are evenly spread across all constituencies
      #If turnout is equal to greater than 65.8
      newData <- eventReactive(input$button,{
        validate(
          need(input$toryNon+
                 input$greenNon+
                 input$labourNon+
                 input$libdemNon+
                 input$ukipNon == 100, 'Inputs must sum to 100%'))
        values$df$labour <- cont2$labour + ((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$labourNon/100))
        values$df$labour <- ifelse(values$df$labour < 0, 0, values$df$labour)
        values$df$tory <- cont2$tory + ((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$toryNon/100))
        values$df$tory <- ifelse(values$df$tory < 0, 0, values$df$tory)
        values$df$green <- cont2$green  + ((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$greenNon/100))
        values$df$green <- ifelse(values$df$green < 0, 0, values$df$green)
        values$df$libdem <- cont2$libdem +((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$libdemNon/100))
        values$df$libdem <- ifelse(values$df$libdem < 0, 0, values$df$libdem)
        values$df$ukip <- cont2$ukip + ((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$ukipNon/100))
        values$df$ukip <- ifelse(values$df$ukip < 0, 0, values$df$ukip)
        values$df$votes <- values$df$tory + values$df$labour + values$df$green + values$df$libdem + values$df$ukip + values$df$other
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
        
        values$df$toryGain <- as.factor(ifelse((values$df$torywin==TRUE) & (cont2$torywin==FALSE),TRUE,FALSE))
        
        values$df$greenGain <- as.factor(ifelse((values$df$greenwin==TRUE) & (cont2$greenwin==FALSE),TRUE,FALSE))
        
        values$df$labourGain <- as.factor(ifelse((values$df$labourwin==TRUE) & (cont2$labourwin==FALSE),TRUE,FALSE))
        
        values$df$libdemGain <- as.factor(ifelse((values$df$libdemwin==TRUE) & (cont2$libdemwin==FALSE),TRUE,FALSE))
        
        values$df$ukipGain <- as.factor(ifelse((values$df$ukipwin==TRUE) & (cont2$ukipwin==FALSE),TRUE,FALSE))
        
        values$df$winner <- as.factor(ifelse(values$df$torywin==TRUE,"Conservatives",
                                             ifelse(values$df$greenwin==TRUE,"Green",
                                                    ifelse(values$df$labourwin==TRUE,"Labour",
                                                           ifelse(values$df$libdemwin==TRUE,"Liberal Democrats",
                                                                  ifelse(values$df$ukipwin==TRUE,"Ukip", NA))))))
        
      })
      
    }else if(input$distro=="Turnout"){
      #Distribution of turnout increases or decreases are weighted towards constituencies with low turnout 
      newData <- eventReactive(input$button,{
        validate(
          need(input$toryNon+
                 input$greenNon+
                 input$labourNon+
                 input$libdemNon+
                 input$ukipNon == 100, 'Inputs must sum to 100%'))
        values$df$labour <- cont2$labour + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$labourNon/100))*cont2$turnStand)
        values$df$labour <- ifelse(values$df$labour < 0, 0, values$df$labour)
        values$df$tory <- cont2$tory + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$toryNon/100))*cont2$turnStand)
        values$df$tory <- ifelse(values$df$tory < 0, 0, values$df$tory)
        values$df$green <- cont2$green  + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$greenNon/100))*cont2$turnStand)
        values$df$green <- ifelse(values$df$green < 0, 0, values$df$green)
        values$df$libdem <- cont2$libdem + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$libdemNon/100))*cont2$turnStand)
        values$df$libdem <- ifelse(values$df$libdem < 0, 0, values$df$libdem)
        values$df$ukip <- cont2$ukip + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$ukipNon/100))*cont2$turnStand)
        values$df$ukip <- ifelse(values$df$ukip < 0, 0, values$df$ukip)
        values$df$votes <- values$df$tory + values$df$labour + values$df$green + values$df$libdem + values$df$ukip + values$df$other
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
        
        values$df$toryGain <- as.factor(ifelse((values$df$torywin==TRUE) & (cont2$torywin==FALSE),TRUE,FALSE))
        
        values$df$greenGain <- as.factor(ifelse((values$df$greenwin==TRUE) & (cont2$greenwin==FALSE),TRUE,FALSE))
        
        values$df$labourGain <- as.factor(ifelse((values$df$labourwin==TRUE) & (cont2$labourwin==FALSE),TRUE,FALSE))
        
        values$df$libdemGain <- as.factor(ifelse((values$df$libdemwin==TRUE) & (cont2$libdemwin==FALSE),TRUE,FALSE))
        
        values$df$ukipGain <- as.factor(ifelse((values$df$ukipwin==TRUE) & (cont2$ukipwin==FALSE),TRUE,FALSE))
        
        values$df$winner <- as.factor(ifelse(values$df$torywin==TRUE,"Conservatives",
                                             ifelse(values$df$greenwin==TRUE,"Green",
                                                    ifelse(values$df$labourwin==TRUE,"Labour",
                                                           ifelse(values$df$libdemwin==TRUE,"Liberal Democrats",
                                                                  ifelse(values$df$ukipwin==TRUE,"Ukip", NA))))))      
        
      })
      
    }else if(input$distro=="Marginality"){
      newData <- eventReactive(input$button,{
        validate(
          need(input$toryNon+
                 input$greenNon+
                 input$labourNon+
                 input$libdemNon+
                 input$ukipNon == 100, 'Inputs must sum to 100%'))
        values$df$labour <- cont2$labour + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$labourNon/100))*cont2$marginStand)
        values$df$labour <- ifelse(values$df$labour < 0, 0, values$df$labour)
        values$df$tory <- cont2$tory + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$toryNon/100))*cont2$marginStand)
        values$df$tory <- ifelse(values$df$tory < 0, 0, values$df$tory)
        values$df$green <- cont2$green  + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$greenNon/100))*cont2$marginStand)
        values$df$green <- ifelse(values$df$green < 0, 0, values$df$green)
        values$df$libdem <- cont2$libdem + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$libdemNon/100))*cont2$marginStand)
        values$df$libdem <- ifelse(values$df$libdem < 0, 0, values$df$libdem)
        values$df$ukip <- cont2$ukip + (((((input$turnOut - 65.8)/100)*cont2$electorate)*(input$ukipNon/100))*cont2$marginStand)
        values$df$ukip <- ifelse(values$df$ukip < 0, 0, values$df$ukip)
        values$df$votes <- values$df$tory + values$df$labour + values$df$green + values$df$libdem + values$df$ukip + values$df$other
        
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
        
        values$df$toryGain <- as.factor(ifelse((values$df$torywin==TRUE) & (cont2$torywin==FALSE),TRUE,FALSE))
        
        values$df$greenGain <- as.factor(ifelse((values$df$greenwin==TRUE) & (cont2$greenwin==FALSE),TRUE,FALSE))
        
        values$df$labourGain <- as.factor(ifelse((values$df$labourwin==TRUE) & (cont2$labourwin==FALSE),TRUE,FALSE))
        
        values$df$libdemGain <- as.factor(ifelse((values$df$libdemwin==TRUE) & (cont2$libdemwin==FALSE),TRUE,FALSE))
        
        values$df$ukipGain <- as.factor(ifelse((values$df$ukipwin==TRUE) & (cont2$ukipwin==FALSE),TRUE,FALSE))
        
        values$df$winner <- as.factor(ifelse(values$df$torywin==TRUE,"Conservatives",
                                             ifelse(values$df$greenwin==TRUE,"Green",
                                                    ifelse(values$df$labourwin==TRUE,"Labour",
                                                           ifelse(values$df$libdemwin==TRUE,"Liberal Democrats",
                                                                  ifelse(values$df$ukipwin==TRUE,"Ukip", NA))))))
      })
      
    } else {"Please Select an Input"}
    
    output$winner <- renderTable({
      validate(
        need(input$toryNon+
               input$greenNon+
               input$labourNon+
               input$libdemNon+
               input$ukipNon == 100, 'Inputs must sum to 100%')
      )
      outcome <- newData()
      summary(outcome)},bordered=TRUE,
      striped=TRUE, rownames = TRUE, colnames=FALSE)
    
    output$seatPlot <- renderPlot({
      validate(
        need(input$toryNon+
               input$greenNon+
               input$labourNon+
               input$libdemNon+
               input$ukipNon == 100, 'Inputs must sum to 100%'))
      data4 <- data.frame(winner=c("Conservatives",
                                   "Green", "Labour",
                                   "Liberal Democrats",
                                   "Ukip"),seats=c(sum(values$df$torywin==TRUE),
                                                   sum(values$df$greenwin==TRUE),
                                                   sum(values$df$labourwin==TRUE),
                                                   sum(values$df$libdemwin==TRUE),
                                                   sum(values$df$ukipwin==TRUE)))
      
      parties <- c("Conservatives", "Green", "Labour", "Liberal Democrats", "Ukip")
      
      barcolour3 <- c("Conservatives" = "#0087dc",
                      "Green" = "#6AB023",
                      "Labour" = "#DC241f",
                      "Liberal Democrats" = "#FDBB30",
                      "Ukip" = "#70147A"
      )
      
      ggSeats <- ggplot(data4, aes(x = winner, y = seats, fill=factor(parties), label = seats)) + geom_bar(stat = "identity") + scale_fill_manual(values = barcolour3, breaks = parties) + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) + scale_x_discrete(limits = c("Conservatives", "Green", "Labour", "Liberal Democrats", "Ukip"), drop=FALSE)  + xlab("Party") + ylab("Seats in England") + geom_text(aes(y = seats + 0.1), position = position_dodge(0.9), vjust = -0.2, size = 6)+ guides(fill=FALSE)
      
      print(ggSeats)
    })
    
    output$votePlot <- renderPlot({
      validate(
        need(input$toryNon+
               input$greenNon+
               input$labourNon+
               input$libdemNon+
               input$ukipNon == 100, 'Inputs must sum to 100%'))
      
      data6 <- data.frame(winner=c("Conservatives",
                                   "Green", "Labour",
                                   "Liberal Democrats",
                                   "Ukip"),votes=c(sum(values$df$tory),
                                                   sum(values$df$green),
                                                   sum(values$df$labour),
                                                   sum(values$df$libdem),
                                                   sum(values$df$ukip)))
      
      parties <- c("Conservatives", "Green", "Labour", "Liberal Democrats", "Ukip")
      
      barcolour3 <- c("Conservatives" = "#0087dc",
                      "Green" = "#6AB023",
                      "Labour" = "#DC241f",
                      "Liberal Democrats" = "#FDBB30",
                      "Ukip" = "#70147A"
      )
      
      ggVote <- ggplot(data6, aes(x = winner, y = votes, fill=factor(parties), label = votes)) + geom_bar(stat = "identity") + scale_fill_manual(values = barcolour3, breaks = parties) + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) + scale_x_discrete(limits = c("Conservatives", "Green", "Labour", "Liberal Democrats", "Ukip"), drop=FALSE)  + xlab("Party") + ylab("Votes in England") + geom_text(aes(label= paste0(formatC(round(votes),format="d", big.mark=','))), position = position_dodge(0.9), vjust = -0.2, size = 5)+ guides(fill=FALSE)
      
      print(ggVote)
      
    })
    
    #Conservatives
    output$torySeats <- renderText({
      sum(values$df$torywin==TRUE)})
    
    output$torySeatShare <- renderText({paste(
      round((sum(values$df$torywin==TRUE)/532)*100, digits=2),"%",sep="")})
    
    output$toryVotes <- renderText({
      formatC(round(sum(values$df$tory),digits=0), format="d", big.mark=',')})
    
    output$toryShare <- renderText({paste(
      round((sum(values$df$tory)/sum(values$df$votes))*100,digits=2),"%",sep="")})
    
    output$torySeatChange <- renderText({
      sum(values$df$torywin==TRUE)-sum(cont2$torywin==TRUE)})
    
    output$toryVoteChange <- renderText({
      formatC(round(sum(values$df$tory)-sum(cont2$tory),digits=0), format="d", big.mark=',')})
    
    output$toryShareChange  <- renderText({paste(
      round(((sum(values$df$tory)/sum(values$df$votes))*100)-((sum(cont2$tory)/sum(cont2$votes)*100)),digits=2),"%",sep="")})
    #Greens
    output$greenSeats <- renderText({
      sum(values$df$greenwin==TRUE)})
    
    output$greenSeatShare <- renderText({paste(
      round((sum(values$df$greenwin==TRUE)/532)*100, digits=2),"%",sep="")})
    
    output$greenVotes <- renderText({
      formatC(round(sum(values$df$green),digits=0), format="d", big.mark=',')})
    
    output$greenShare <- renderText({paste(
      round((sum(values$df$green)/sum(values$df$votes))*100,digits=2),"%",sep="")})
    
    output$greenSeatChange <- renderText({
      sum(values$df$greenwin==TRUE)-sum(cont2$greenwin==TRUE)})
    
    output$greenVoteChange <- renderText({
      formatC(round(sum(values$df$green)-sum(cont2$green),digits=0), format="d", big.mark=',')})
    
    output$greenShareChange  <- renderText({paste(
      round(((sum(values$df$green)/sum(values$df$votes))*100)-((sum(cont2$green)/sum(cont2$votes)*100)),digits=2),"%",sep="")})
    #Labour
    output$labourSeats <- renderText({
      sum(values$df$labourwin==TRUE)})
    
    output$labourSeatShare <- renderText({paste(
      round((sum(values$df$labourwin==TRUE)/532)*100, digits=2),"%",sep="")})
    
    output$labourVotes <- renderText({
      formatC(round(sum(values$df$labour),digits=0), format="d", big.mark=',')})
    
    output$labourShare <- renderText({paste(
      round((sum(values$df$labour)/sum(values$df$votes))*100,digits=2),"%",sep="")})
    
    output$labourSeatChange <- renderText({
      sum(values$df$labourwin==TRUE)-sum(cont2$labourwin==TRUE)})
    
    output$labourVoteChange <- renderText({
      formatC(round(sum(values$df$labour)-sum(cont2$labour),digits=0), format="d", big.mark=',')})
    
    output$labourShareChange  <- renderText({paste(
      round(((sum(values$df$labour)/sum(values$df$votes))*100)-((sum(cont2$labour)/sum(cont2$votes)*100)),digits=2),"%",sep="")})
    #LibDems
    output$libdemSeats <- renderText({
      sum(values$df$libdemwin==TRUE)})
    
    output$libdemSeatShare <- renderText({paste(
      round((sum(values$df$libdemwin==TRUE)/532)*100, digits=2),"%",sep="")})
    
    output$libdemVotes <- renderText({
      formatC(round(sum(values$df$libdem),digits=0), format="d", big.mark=',')})
    
    output$libdemShare <- renderText({paste(
      round((sum(values$df$libdem)/sum(values$df$votes))*100,digits=2),"%",sep="")})
    
    output$libdemSeatChange <- renderText({
      sum(values$df$libdemwin==TRUE)-sum(cont2$libdemwin==TRUE)})
    
    output$libdemVoteChange <- renderText({
      formatC(round(sum(values$df$libdem)-sum(cont2$libdem),digits=0), format="d", big.mark=',')})
    
    output$libdemShareChange  <- renderText({paste(
      round(((sum(values$df$libdem)/sum(values$df$votes))*100)-((sum(cont2$libdem)/sum(cont2$votes)*100)),digits=2),"%",sep="")})
    #Ukip
    output$ukipSeats <- renderText({
      sum(values$df$ukipwin==TRUE)})
    
    output$ukipSeatShare <- renderText({paste(
      round((sum(values$df$ukipwin==TRUE)/532)*100, digits=2),"%",sep="")})
    
    output$ukipVotes <- renderText({
      formatC(round(sum(values$df$ukip),digits=0), format="d", big.mark=',')})
    
    output$ukipShare <- renderText({paste(
      round((sum(values$df$ukip)/sum(values$df$votes))*100,digits=2),"%",sep="")})
    
    output$ukipSeatChange <- renderText({
      sum(values$df$ukipwin==TRUE)-sum(cont2$ukipwin==TRUE)})
    
    output$ukipVoteChange <- renderText({
      formatC(round(sum(values$df$ukip)-sum(cont2$ukip),digits=0), format="d", big.mark=',')})
    
    output$ukipShareChange  <- renderText({paste(
      round(((sum(values$df$ukip)/sum(values$df$votes))*100)-((sum(cont2$ukip)/sum(cont2$votes)*100)),digits=2),"%",sep="")})
    
    #GAINS
    
    output$toryGainsTable <- renderTable({
      
      toryGainDF <- subset(values$df, toryGain=="TRUE")
      
      toryGainDF$from <- cont2$winner[match(toryGainDF$constituency,cont2$constituency)]
      
      toryGainDFsummary <- subset(toryGainDF, select=c("constituency", "from"))
      
      if(nrow(toryGainDFsummary)==0){
        print("No Conservative Gains")
      } else{
        
        toryGainDFsummary
      }
      
    },bordered=TRUE,striped=TRUE, rownames = FALSE, colnames=TRUE)
    
    output$greenGainsTable <- renderTable({
      
      greenGainDF <- subset(values$df, greenGain=="TRUE")
      
      greenGainDF$from <- cont2$winner[match(greenGainDF$constituency,cont2$constituency)]
      
      greemGainDFsummary <- subset(greenGainDF, select=c("constituency", "from"))
      
      if(nrow(greemGainDFsummary)==0){
        print("No Green Gains")
      } else{
        
        greemGainDFsummary
      }
      
    },bordered=TRUE,striped=TRUE, rownames = FALSE, colnames=TRUE)
    
    output$labourGainsTable <- renderTable({
      
      labourGainDF <- subset(values$df, labourGain=="TRUE")
      
      labourGainDF$from <- cont2$winner[match(labourGainDF$constituency,cont2$constituency)]
      
      labourGainDFsummary <- subset(labourGainDF, select=c("constituency", "from"))
      
      if(nrow(labourGainDFsummary)==0){
        print("No Labour Gains")
      } else{
        
        labourGainDFsummary
        
      }
      
    },bordered=TRUE,striped=TRUE, rownames = FALSE, colnames=TRUE)
    
    output$libdemGainsTable <- renderTable({
      
      libdemGainDF <- subset(values$df, libdemGain=="TRUE")
      
      libdemGainDF$from <- cont2$winner[match(libdemGainDF$constituency,cont2$constituency)]
      
      libdemGainDFsummary <- subset(libdemGainDF, select=c("constituency", "from"))
      
      if(nrow(libdemGainDFsummary)==0){
        print("No Liberal Democrat Gains")
      } else{
        
        libdemGainDFsummary
      }
      
    },bordered=TRUE,striped=TRUE, rownames = FALSE, colnames = TRUE)
    
    output$ukipGainsTable <- renderTable({
      
      ukipGainDF <- subset(values$df, ukipGain=="TRUE")
      
      ukipGainDF$from <- cont2$winner[match(ukipGainDF$constituency,cont2$constituency)]
      
      ukipGainDFsummary <- subset(ukipGainDF, select=c("constituency", "from"))
      
      if(nrow(ukipGainDFsummary)==0){
        print("No Ukip Gains")
      } else{
        
        ukipGainDFsummary
      }
      
    },bordered=TRUE,striped=TRUE, rownames = FALSE, colnames = TRUE)
    
    
    
    ### PROPORTONALITY
    observeEvent(input$button, {
      validate(
        need(input$toryNon+
               input$greenNon+
               input$labourNon+
               input$libdemNon+
               input$ukipNon == 100, 'Inputs must sum to 100%'))
      
      propData <- data.frame(party=c("Conservatives",
                                     "Green", "Labour",
                                     "Liberal Democrats",
                                     "Ukip"),
                             votes=c(sum(values$df$tory),
                                     sum(values$df$green),
                                     sum(values$df$labour),
                                     sum(values$df$libdem),
                                     sum(values$df$ukip)),
                             pvotes=c((sum(values$df$tory)/sum(values$df$votes))*100,
                                      (sum(values$df$green)/sum(values$df$votes))*100,
                                      (sum(values$df$labour)/sum(values$df$votes))*100,
                                      (sum(values$df$libdem)/sum(values$df$votes))*100,
                                      (sum(values$df$ukip)/sum(values$df$votes))*100),
                             seats=c(sum(values$df$torywin==TRUE),
                                     sum(values$df$greenwin==TRUE),
                                     sum(values$df$labourwin==TRUE),
                                     sum(values$df$libdemwin==TRUE),
                                     sum(values$df$ukipwin==TRUE)),
                             pseats=c((sum(values$df$torywin==TRUE)/532)*100,
                                      (sum(values$df$greenwin==TRUE)/532)*100,
                                      (sum(values$df$labourwin==TRUE)/532)*100,
                                      (sum(values$df$libdemwin==TRUE)/532)*100,
                                      (sum(values$df$ukipwin==TRUE)/532)*100))
      
      
      switch(input$propType,
             
             "Gallagher" = output$propOutput <-renderText({#1
               prop1 <-  with(propData, Proportionality(pvotes, pseats, index = "Gallagher"))
             }),
             
             "Rae" = output$propOutput <-renderText({#2
               prop2 <-  with(propData, Proportionality(pvotes, pseats, index = "Rae"))
             }),
             
             "Loosemore-Hanby" = output$propOutput <-renderText({#3
               prop3 <-  with(propData, Proportionality(pvotes, pseats, index = "Loosemore-Hanby"))
             }),
             
             "Rose" = output$propOutput <-renderText({#4
               prop4 <-  with(propData, Proportionality(pvotes, pseats, index = "Rose"))
             }),
             
             "Sainte-Lague" = output$propOutput <-renderText({#5
               prop5 <-  with(propData, Proportionality(pvotes, pseats, index = "Sainte-Lague"))
             }),
             
             "Grofman" = output$propOutput <-renderText({#6
               prop6 <-  with(propData, Proportionality(pvotes, pseats, index = "Grofman"))
             }),
             
             "Lijphart" = output$propOutput <-renderText({#7
               prop7 <-  with(propData, Proportionality(pvotes, pseats, index = "Lijphart"))
             }),
             
             "Farina" = output$propOutput <-renderText({#8
               prop8 <-  with(propData, Proportionality(pvotes, pseats, index = "Farina"))
             }),
             
             "Cox-Shugart" = output$propOutput <-renderText({#9
               prop9 <-  with(propData, Proportionality(pvotes, pseats, index = "Cox-Shugart"))
             }),
             
             "D'Hondt" = output$propOutput <-renderText({#10
               prop10 <-  with(propData, Proportionality(pvotes, pseats, index = "DHondt"))
             })
      )
      
    })
    
  })
  
}


