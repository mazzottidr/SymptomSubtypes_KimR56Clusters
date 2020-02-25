library(rsconnect) 
library(shiny)
library(plotly)
library(shinydashboard)
library(shinyjs)
library(V8)

#source("functions.R")

##
#
# Server Call
#
##
server <- function(input, output, session) {
  
  ##
  # ESS Error
  ##
  render_ess_error<-eventReactive(input$do, {
    
    if(is.na(8*mean(c(as.numeric(input$ess1), 
                      as.numeric(input$ess2), 
                      as.numeric(input$ess3), 
                      as.numeric(input$ess4), 
                      as.numeric(input$ess5), 
                      as.numeric(input$ess6), 
                      as.numeric(input$ess7), 
                      as.numeric(input$ess8)), 
                    na.rm=TRUE ))) {
      HTML(paste("<div style='text-align:center'><font color='red'><h4> ERROR: Please complete at least one question in the Epworth Sleepiness Scale. </h4></font></div>"))
    }
  })
  output$ess_error<-renderText({render_ess_error()})
  
  ##
  # Calculate Subtype
  ##
  CalcSubtype <- eventReactive(input$do, {
    
    doClusterProbs( ctyp = input$type,
                    e1 = input$ess1,
                    e2 = input$ess2,
                    e3 = input$ess3,
                    e4 = input$ess4,
                    e5 = input$ess5,
                    e6 = input$ess6,
                    e7 = input$ess7,
                    e8 = input$ess8,
                    s1 = input$rested,
                    s2 = input$sleepyday,
                    s3 = input$phystired,
                    s4 = input$sleepinvol,
                    s5 = input$napping,
                    s6 = input$dozedrive,
                    s7 = input$dis, 		 
                    s8 = input$dms,		 
                    s9 = input$ema, 		 
                    s10 = input$restless, 
                    s11 = input$headache, 	  
                    s12 = input$nocsweat, 	  
                    s13 = input$cantbreathe, 
                    s14 = input$stopbreathe, 
                    s15_1 = input$snore,   	  
                    s15_2 = input$snoredist, 
                    c1_1 = input$rls_urge, 		    
                    c1_2 = input$rls_discom, 		    
                    c1_3 = input$rls_relief, 		    
                    c1_4 = input$rls_times, 		    
                    c2 = input$htn, 		    
                    c3 = input$dm ,		      
                    c4_1 = input$cad,
                    c4_2 = input$hf,
                    c4_3 = input$stroke
    )
    
  })
  
 
  
  ##
  # Text
  ##
  output$SubtypeText <- renderText({
    HTML(paste("<div style='text-align:center'>","<h3>","Patient has the <b>", CalcSubtype()$Subtype,"</b> OSA Subtype (Probability = ", CalcSubtype()$Probability,"%)", "</h3></div>"))
  })
  
  ##
  # Pie
  ##
  render_SubtypePie <- eventReactive(input$do, {
     
      plot_ly(data.frame(x=c(round(CalcSubtype()$PrC1,3),
                             round(CalcSubtype()$PrC2,3),
                             round(CalcSubtype()$PrC3,3)), 
                         n=c("Disturbed Sleep", 
                             "Minimally Symptomatic", 
                             "Excessively Sleepy")), 
              labels=~n, 
              values=~x, 
              type='pie', 
              marker=list(colors=c("blue", "green", "red")), 
              textposition='inside',
              textinfo = 'label+percent',
              showlegend=FALSE) %>%
        layout(paper_bgcolor='#00000000')
    
  })
  
  output$SubtypePie <- renderPlotly({render_SubtypePie()})
  
  ##
  # Cluster Table
  ##
  render_SubtypeTable <-  eventReactive(input$do, {
    
      data.frame(Probability=c(round(CalcSubtype()$PrC1,4),
                               round(CalcSubtype()$PrC2,4),
                               round(CalcSubtype()$PrC3,4)), 
                 Subtype=c("Disturbed Sleep", 
                           "Minimally Symptomatic", 
                           "Excessively Sleepy"))

  })
  
  output$SubtypeTable <- renderTable({render_SubtypeTable()},bordered=TRUE,digits=4,striped=TRUE,align='c')
  
  ##
  # Data Table
  ##
  render_SubtypeData <-  eventReactive(input$do, {
 
      dt<-data.frame(ESS            = round(CalcSubtype()$ESS,0),
                     SleepyTV        = input$ess2>1,                                    ## Sleepy Watching TV
                     Rested          = input$rested<3,                                  ## I feel rested upon waking
                     SleepyDay       = input$sleepyday>2,                               ## I feel sleepy during the day                   
                     PhysTired       = input$phystired>2,                               ## Physically Tired / Fatigued
                     SleepInvol      = input$sleepinvol>2,                              ## Fall asleep Involuntarily
                     Napping         = input$napping>2,                                 ## Napping
                     DozeDrive       = input$dozedrive>2,                               ## Drowsy Driving
                     DifInitSleep    = input$dis>2,                                     ## Difficulty Initiating Sleep
                     DifStayAsleep   = input$dms>2,                                     ## Difficulty Maintaining Sleep
                     EarlyAwake      = input$ema>2,                                     ## Early Morning Awakening
                     RestlessSleep   = input$restless>2,                                ## I'm Restless in my Sleep
                     Headache        = input$headache>2,                                ## Headache
                     NightSweat      = input$nocsweat>2,                                ## Nocturnal Sweating
                     CantBreathe     = input$cantbreathe>2,                             ## Wake up and can't breathe
                     WitnessApnea    = input$stopbreathe>2,                             ## Witnessed Apneas
                     Snorer          = input$snore==1,                                  ## Snoring (Yes/No)
                     SnoreDisturb    = input$snoredist==1,                              ## Snoring (Disturb Partner)
                     RLS             = (input$rls_urge>3 
                                        & input$rls_discom==1 
                                        & input$rls_relief==1            
                                        & input$rls_times!=1 
                                        & input$rls_times!=2),                          ## RLS 
                     HTN             = input$htn==1,                                    ## HTN
                     Diabetes        = input$dm==1,                                     ## Diabetes
                     CVD             = (input$cad==1 
                                        | input$hf==1 
                                        | input$stroke==1)   ## CVD
      ) 
      dtt<-t(dt)
      dtt<-cbind(colnames(dt),dtt)
      colnames(dtt)<-c("Symptom", "Result")
      dtt
      
   })
  
  output$SubtypeData <- renderTable({render_SubtypeData()},bordered=TRUE,striped=TRUE,align='c')
 
  ##
  #
  # Prev and Next Buttons
  #
  ##
  observeEvent(input$tabs, {
    js$gotoTop()
  })
  
  observeEvent(input$t1next, {
    js$gotoTop()
    newtab <- switch(input$tabs,"About" = "Epworth")
    updateTabItems(session, "tabs", newtab)
  })
  
  observeEvent(input$t2prev, {
    js$gotoTop()
    newtab <- switch(input$tabs,"Epworth" = "About")
    updateTabItems(session, "tabs", newtab)
  })
  
  observeEvent(input$t2next, {
    js$gotoTop()
    newtab <- switch(input$tabs,"Epworth" = "Symptoms")
    updateTabItems(session, "tabs", newtab)
  })
  
  observeEvent(input$t3prev, {
    js$gotoTop()
    newtab <- switch(input$tabs,"Symptoms" = "Epworth")
    updateTabItems(session, "tabs", newtab)
  })

  observeEvent(input$t3next, {
    js$gotoTop()
    newtab <- switch(input$tabs,"Symptoms" = "Comorb")
    updateTabItems(session, "tabs", newtab)
  })
  
  observeEvent(input$t4prev, {
    js$gotoTop()
    newtab <- switch(input$tabs,"Comorb" = "Symptoms")
    updateTabItems(session, "tabs", newtab)
  })
  
  observeEvent(input$t4next, {
    js$gotoTop()
    newtab <- switch(input$tabs,"Comorb" = "Calc")
    updateTabItems(session, "tabs", newtab)
  })
  
  observeEvent(input$t5prev, {
    js$gotoTop()
    newtab <- switch(input$tabs,"Calc" = "Comorb")
    updateTabItems(session, "tabs", newtab)
  })
  
  observeEvent(input$reset_forms, {
    shinyjs::reset("forms")
  })
  
} ## server end