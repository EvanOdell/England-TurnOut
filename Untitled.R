

output$propOutput 

switch(input$propType,
       
       if(input$propType == "Gallagher"){
         output$propOutput <-renderText({#1
           prop1 <-  with(propData, Proportionality(pvotes, pseats, index = "Gallagher"))}) 
       } else if (input$propType == "Rae"){
         output$propOutput <-renderText({#2
           prop2 <-  with(propData, Proportionality(pvotes, pseats, index = "Rae"))
         })
       } else if (input$propType == "Loosemore-Hanby"){
         output$LoosemoreHanby <-renderText({#3
           prop3 <-  with(propData, Proportionality(pvotes, pseats, index = "Loosemore-Hanby"))
         })
       } else if (input$propType == "Rose"){
         output$propOutput <-renderText({#6
           prop6 <-  with(propData, Proportionality(pvotes, pseats, index = "Rose"))
         })
       } else if (input$propType == "Sainte-Lague"){
         output$propOutput <-renderText({#7
           prop7 <-  with(propData, Proportionality(pvotes, pseats, index = "Sainte-Lague"))
         })
       } else if (input$propType == "Grofman"){
         output$propOutput <-renderText({#8
           prop8 <-  with(propData, Proportionality(pvotes, pseats, index = "Grofman"))
         })
       } else if (input$propType == "Lijphart"){
         output$propOutput <-renderText({#9
           prop9 <-  with(propData, Proportionality(pvotes, pseats, index = "Lijphart"))
         })
       } else if (input$propType == "Farina"){
         output$propOutput <-renderText({#10
           prop10 <-  with(propData, Proportionality(pvotes, pseats, index = "Farina"))
         })
       } else if (input$propType == "Cox-Shugart"){
         output$propOutput <-renderText({#11
           prop11 <-  with(propData, Proportionality(pvotes, pseats, index = "Cox-Shugart"))
         })
         
       } else if (input$propType == "D'Hondt" ){
         output$propOutput <-renderText({#12
           prop12 <-  with(propData, Proportionality(pvotes, pseats, index = "DHondt"))
         })
       }
       
       
       





if(is.null(input$proportion_calc)){
  return()
} else{
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
  
  output$gallagher <-renderText({#1
    prop1 <-  with(propData, Proportionality(pvotes, pseats, index = "Gallagher"))
  })
  
  output$raes <-renderText({#2
    prop2 <-  with(propData, Proportionality(pvotes, pseats, index = "Rae"))
  })
  
  output$LoosemoreHanby <-renderText({#3
    prop3 <-  with(propData, Proportionality(pvotes, pseats, index = "Loosemore-Hanby"))
  })
  
  output$Rose <-renderText({#6
    prop6 <-  with(propData, Proportionality(pvotes, pseats, index = "Rose"))
  })
  
  output$SainteLague <-renderText({#7
    prop7 <-  with(propData, Proportionality(pvotes, pseats, index = "Sainte-Lague"))
  })
  
  output$Grofman <-renderText({#8
    prop8 <-  with(propData, Proportionality(pvotes, pseats, index = "Grofman"))
  })
  
  output$Lijphart <-renderText({#9
    prop9 <-  with(propData, Proportionality(pvotes, pseats, index = "Lijphart"))
  })
  
  output$Farina <-renderText({#10
    prop10 <-  with(propData, Proportionality(pvotes, pseats, index = "Farina"))
  })
  
  output$CoxShugart <-renderText({#11
    prop11 <-  with(propData, Proportionality(pvotes, pseats, index = "Cox-Shugart"))
  })
  
  output$DHondt <-renderText({#12
    prop12 <-  with(propData, Proportionality(pvotes, pseats, index = "DHondt"))
  })
  
  
  
  output$ui <- renderUI({
    tabPanel("Proportionality",
             fluidPage(
               actionButton("doProp", "Calculate Results"),
               
               HTML(paste(
                 tags$table(
                   tags$tr(
                     tags$th("Method"),
                     tags$th("Score")),
                   tags$tr(tags$th(h4("Gallagher")),
                           tags$td(textOutput("gallagher"))
                   ),
                   tags$tr(tags$th(h4("Raes")),
                           tags$td(textOutput("raes"))
                   ),
                   tags$tr(tags$th(h4("Loosemore-Hanby")),
                           tags$td(textOutput("LoosemoreHanby"))
                   ),
                   tags$tr(tags$th(h4("Inverted Gallagher")),
                           tags$td(textOutput("inv.Gallagher"))
                   ),
                   tags$tr(tags$th(h4("Rose")),
                           tags$td(textOutput("rose"))
                   ),
                   tags$tr(tags$th(h4("Sainte-Lague")),
                           tags$td(textOutput("SainteLague"))
                   ),
                   tags$tr(tags$th(h4("gallagher")),
                           tags$td(textOutput("gallagher"))
                   ),
                   tags$tr(tags$th(h4("Grofman")),
                           tags$td(textOutput("Grofman"))
                   ),
                   tags$tr(tags$th(h4("Lijphart")),
                           tags$td(textOutput("Lijphart"))
                   ),
                   tags$tr(tags$th(h4("Farina")),
                           tags$td(textOutput("Farina"))
                   ),
                   tags$tr(tags$th(h4("CoxShugart")),
                           tags$td(textOutput("CoxShugart"))
                   ),
                   tags$tr(tags$th(h4("DHondt")),
                           tags$td(textOutput("DHondt"))
                   )
                 )
               )
               )#End of table
             )       
    )
  })
  
}

