library(rsconnect) 
library(shiny)
library(plotly)
library(shinydashboard)
library(shinyjs)

##
# Symptom Cluster Shiny Application - UI
##
ui <- dashboardPage(title="OSA Subtype Calculator", skin="red",
              
                    dashboardHeader(title="OSA Subtype Calculator (Kim R56)", titleWidth=400), 
                    
                    dashboardSidebar(
                          sidebarMenu(id="tabs",
                          menuItem("About", tabName = "About", icon = icon("info")),
                          menuItem("Epworth Sleepiness Scale", tabName = "Epworth", icon = icon("bed")),
                          menuItem("Symptoms", tabName = "Symptoms", icon = icon("notes-medical")),
                          menuItem("Comorbidities", tabName = "Comorb", icon = icon("stethoscope")),
                          menuItem("Calculate Subtype", tabName = "Calc", icon = icon("cogs"))
                      ),
                      tags$p(""),
                      useShinyjs(), 
                      actionButton("reset_forms", "Reset All Forms", icon = icon("trash-alt"))
                    ),
                    
                    dashboardBody(
                      id="forms",
                      position = "fixed-top",
                      tags$script(HTML("$('body').addClass('fixed');")),
                      useShinyjs(), 
                      extendShinyjs(text = "shinyjs.gotoTop = function() {window.scrollTo(0, 0);}"),
                      tabItems(
                          tabItem("About",
                                tags$h4(
                                  tags$p("This Shiny Application can be used to determine the OSA symptom subtype of an individual patient with Apnea-Hypopnea Index ", 
                                         HTML("&ge;"), 
                                         "15 events/hour, based on those originally identified in the Iceland Sleep Apnea Cohort (ISAC).",
                                         tags$sup(1),
                                         tags$strong("This specific App was written to support recruitment in the funded R56 led by Dr. Jinyoung Kim at UNLV."),
                                         "These subtypes have been replicated and generalized to patients from international sleep centers participating in the Sleep Apnea Global Interdisciplinary Consortium (SAGIC)",
                                         tags$sup(2),
                                         ", as well as in the Korean Genome and Epidemiology Study",
                                         tags$sup(3),
                                         ". Recently, the", 
                                         tags$em("Excessively Sleepy"), 
                                         "subtype has been shown to be at increased risk for Cardiovascular Disease when compared to other subytpes and controls in the Sleep Heart Health Study (SHHS)",
                                         tags$sup(4), 
                                         ". Specific questions can be directed towards the corresponding authors of these manuscripts, by contacting the developer of this Application (keenanbr@pennmedicine.upenn.edu), or the PI of the funded R56 (jinyoung.kim@unlv.edu)."
                                  ),
                                  tags$p(""),
                                  tags$ol(tags$li("Ye L, Pien GW, Ratcliffe SJ, Bjornsdottir E, Arnardottir ES, Pack AI, et al. The different clinical faces of obstructive sleep apnoea: a cluster analysis. Eur Respir J. 2014;44(6):1600-7."),
                                          tags$li("Keenan BT, Kim J, Singh B, Bittencourt L, Chen NH, Cistulli PA, et al. Recognizable clinical subtypes of obstructive sleep apnea across international sleep centers: a cluster analysis. Sleep. 2018;41(3)."),
                                          tags$li("Kim J, Keenan BT, Lim DC, Lee SK, Pack AI, Shin C. Symptom-based subgroups of Koreans with obstructive sleep apnea. J Clin Sleep Med. 2018;14(3):437-43."),
                                          tags$li("Mazzotti DR, Keenan BT, Lim DC, Gottlieb DJ, Kim J, Pack AI. Symptom subtypes of obstructive sleep apnea predict incidence of cardiovascular outcomes. Am J Respir Crit Care Med. 2019;200(4):493-506.")
                                  ),
                                  tags$p(" "),
                                  tags$p(" "),
                                  tags$h3(tags$strong("For accurate subtype classification, please complete all questions to the best of your ability."))
                                ),
                                fluidRow(column(2, actionButton('t1next', "Next", icon("arrow-circle-right"))))
                        ),
                        
                        tabItem("Epworth",
                                h4("How likely are you to doze off or fall asleep in the following situation(s), in contrast to feeling just tired? This refers to your usual way of life in recent times. Even if you haven't done some of these things recently, try to work out how they would have affected you."),
                                h4("It is important to answer each question as best you can."),
                                
                                radioButtons("ess1",label= "a. Sitting and reading:", width=1500, 
                                             choices=list("Would never dose" = 0, "Slight change of dozing" = 1, "Moderate chance of dozing" = 2, "High chance of dozing" = 3, "None selected"=""),
                                             selected="",),
                                radioButtons("ess2",label= "b. Watching TV:", width=1500, 
                                             choices=list("Would never dose" = 0, "Slight change of dozing" = 1, "Moderate chance of dozing" = 2, "High chance of dozing" = 3, "None selected"=""),
                                             selected="",),
                                radioButtons("ess3",label= "c. Sitting, inactive in a public place (e.g., theatre or meeting):", width=1500, 
                                             choices=list("Would never dose" = 0, "Slight change of dozing" = 1, "Moderate chance of dozing" = 2, "High chance of dozing" = 3, "None selected"=""),
                                             selected="",),
                                radioButtons("ess4",label= "d. As a passenger in a car for an hour without a break:", width=1500, 
                                             choices=list("Would never dose" = 0, "Slight change of dozing" = 1, "Moderate chance of dozing" = 2, "High chance of dozing" = 3, "None selected"=""),
                                             selected="",),
                                radioButtons("ess5",label= "e. Lying down in the afternoon when circumstances permit:", width=1500, 
                                             choices=list("Would never dose" = 0, "Slight change of dozing" = 1, "Moderate chance of dozing" = 2, "High chance of dozing" = 3, "None selected"=""),
                                             selected="",),
                                radioButtons("ess6",label= "f. Sitting down and talking to someone:", width=1500, 
                                             choices=list("Would never dose" = 0, "Slight change of dozing" = 1, "Moderate chance of dozing" = 2, "High chance of dozing" = 3, "None selected"=""),
                                             selected="",),
                                radioButtons("ess7",label= "g. Sitting quietly after a lunch without alcohol:", width=1500, 
                                             choices=list("Would never dose" = 0, "Slight change of dozing" = 1, "Moderate chance of dozing" = 2, "High chance of dozing" = 3, "None selected"=""),
                                             selected="",),
                                radioButtons("ess8",label= "h. In a car, while stopped for a few minutes in traffic:", width=1500, 
                                             choices=list("Would never dose" = 0, "Slight change of dozing" = 1, "Moderate chance of dozing" = 2, "High chance of dozing" = 3, "None selected"=""),
                                             selected="",),
                                fluidRow(
                                  column(2, actionButton('t2prev', "Previous", icon("arrow-circle-left"))),
                                  column(2, offset=1, actionButton('t2next', "Next", icon("arrow-circle-right")))
                                )
                                
                        ),
                        
                        tabItem("Symptoms",
                                
                                h4("Please answer the following based on your experience in the past month:"),
                                
                                ##
                                # During your waking time, do you feel tired, fatigued or not up to par?
                                ##
                                radioButtons("rested",label= "During your waking time, do you feel tired, fatigued or not up to par?", selected=0, width=1500, 
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # I feel very sleepy during the day?
                                ##
                                radioButtons("sleepyday",label= "I feel very sleepy during the day?", selected=0, width=1500,
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # How often do you feel tired or fatigued after you sleep? 
                                ##
                                radioButtons("phystired",label= "How often do you feel tired or fatigued after you sleep?", selected=0, width=1500, 
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # I fall asleep involuntarily during the day (e.g., when I take a break from my work)?
                                ##
                                radioButtons("sleepinvol",label= "I fall asleep involuntarily during the day (e.g., when I take a break from my work)?", selected=0, width=1500,
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # Naps
                                ##
                                radioButtons("napping",label= "I take naps or doze off", selected=0, width=1500,
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # Drowsy Driving Questions
                                ##
                                radioButtons("dozedrive", label = "I doze off at the steering wheel when driving?", selected=0, width=1500,
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # Difficulty Falling Asleep
                                ##
                                radioButtons("dis", label = "I have difficulty falling asleep at night", selected=0, width=1500,
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # Difficulty Maintaining Sleep
                                ##
                                radioButtons("dms", label = "I wake up often during the night / have difficulty staying asleep", selected=0, width=1500,
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # Early Morning Awakening
                                ##
                                radioButtons("ema", label = "I Wake up too early in the morning and am unable to fall back asleep?", selected=0, width=1500,
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # Restlessness during sleep
                                ##
                                radioButtons("restless", label = "I toss, turn and thrash excessively during the night?", selected=0, width=1500,
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # Headache
                                ##
                                radioButtons("headache", label = "I wake up with a headache?", selected=0, width=1500,
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # Nocturnal Sweating
                                ##
                                radioButtons("nocsweat", label = "I sweat/perspire excessively during the night?", selected=0, width=1500,
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # I wake up suddenly and feel as if I can't breathe?
                                ##
                                radioButtons("cantbreathe", label = "I wake up suddenly and feel as if I can't breathe?", selected=0, width=1500,
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # Has anyone noticed that you quit breathing during the night?
                                ##
                                radioButtons("stopbreathe", label = "Has anyone noticed that you quit breathing during the night?", selected=0, width=1500,
                                             choices=list("Never" = 1, 
                                                          "Rarely (less than once a week)" = 2, 
                                                          "Sometimes (1-2 times a week)" = 3, 
                                                          "Frequently (3-4 times a week)" = 4,
                                                          "Always (5-7 times a week)" = 5,
                                                          "Don't know" = 0)),
                                
                                ##
                                # Do you snore?
                                ##
                                radioButtons("snore", label = "Do you snore?", selected=0, width=1500,
                                             choices=list("Yes" = 1, "No" = 2, "Don't know"=0)),
                                
                                ##
                                # Has your snoring ever bothered other people
                                ##
                                radioButtons("snoredist", label = "Has your snoring ever bothered other people?", selected=0, width=1500,
                                             choices=list("Yes" = 1, "No" = 2, "Don't know"=0)),
                                
                                ##
                                # Buttons
                                ##
                                fluidRow(
                                  column(2, actionButton('t3prev', "Previous", icon("arrow-circle-left"))),
                                  column(2, offset=1, actionButton('t3next', "Next", icon("arrow-circle-right")))
                                )
                                
                        ),
                        
                        tabItem("Comorb",
                                
                                h4("Please answer the following regarding your medical history:"),
                                
                                
                                ## 
                                # Hypertension
                                ##
                                radioButtons("htn", label="Have you been diagnosed with high blood pressure?", selected=0, width=1500,
                                             choices=list("Yes"=1, "No"=2, "Don't know"=0)),
                                
                                radioButtons("htn_meds", label="Do you currently take medications to lower you blood pressure", selected=0, width=1500,
                                             choices=list("Yes"=1, "No"=2, "Don't know"=0)),
                                
                                ##
                                # Diabetes
                                ##
                                radioButtons("dm", label="Have you been diagnosed with diabetes?", selected=0, width=1500,
                                             choices=list("Yes"=1, "No"=2, "Don't know"=0)),
                                
                                ##
                                # CVD
                                ##
                                # CAD
                                radioButtons("cad", label="Have you ever been diagnosed with a coronary artery disease (angina, myocardial infarction, or heart attack)?", selected=0, width=1500,
                                             choices=list("Yes"=1, "No"=2, "Don't know"=0)),
                                
                                # HF
                                radioButtons("hf", label="Have you ever been diagnosed with heart failure?", selected=0, width=1500,
                                             choices=list("Yes"=1, "No"=2, "Don't know"=0)),
                                
                                # Stroke
                                radioButtons("stroke", label="Have you ever had a stroke?", selected=0, width=1500,
                                             choices=list("Yes"=1, "No"=2, "Don't know"=0)),
                                
                                ## 
                                # RLS
                                ##
                                # When sitting or lying down, doyou have a strong urge to move your legs?
                                radioButtons("rls_urge", label = "When sitting or lying down, do you have a strong urge to move your legs?", selected=0, width=1500,
                                             choices=list("Never"=1, "Rarely (once a month or less)"=2, "Sometimes (2-4 times a month)"=3, "Often (5-15 times a month)"=4, "Very often (More than 15 times a month)"=5,"Don't know"=0)),
                                
                                # Is the urge to move your legs accompained by a discomfort (unpleasant sensation) in your legs, for example a creepy-crawly or tingly feeling?
                                radioButtons("rls_discom", label = "Is the urge to move your legs accompained by a discomfort (unpleasant sensation) in your legs, for example a creepy-crawly or tingly feeling?", selected=0, width=1500,
                                             choices=list("Yes" = 1, "No" = 2, "Don't know"=0)),
                                
                                # Is the discomfort in your legs relieved in any way, even for a short time, by walking or moving your legs?
                                radioButtons("rls_relief", label = "Is the discomfort in your legs relieved in any way, even for a short time, by walking or moving your legs?", selected=0, width=1500,
                                             choices=list("Yes" = 1, "No" = 2, "Don't know"=0)),
                                
                                # At what times in is the discomfort in your legs and/or urge to move most bothersome?
                                radioButtons("rls_times", label="At what times in is the discomfort in your legs and/or urge to move most bothersome?", selected=0, width=1500,
                                             choices=list("In the mornings"=1, "In the afternoons"=2, "In the evenings"=3, "At bedtime"=4, "No difference by time of day"=5, "Don't know"=0)),
                                
                                #radioButtons("rls", label="Have you been diagnosed with restless legs syndrome (RLS)?", selected=0, width=1500,
                                #             choices=list("Yes"=1, "No"=2, "Don't know"=0)),
                                ##
                                # Buttons
                                ##
                                fluidRow(
                                  column(2, actionButton('t4prev', "Previous", icon("arrow-circle-left"))),
                                  column(2, offset=1, actionButton('t4next', "Next", icon("arrow-circle-right")))
                                )
                                
                        ),
                        
                        tabItem("Calc",
                                actionButton("do", "Click to Determine Subtype", icon = icon("cogs")),
                                tags$p(""),
                                htmlOutput("ess_error"),
                                tableOutput("SubtypeTable"),
                                htmlOutput("SubtypeText"),
                                plotlyOutput("SubtypePie"),
                                tableOutput("SubtypeData"),
                                
                                ##
                                # Buttons
                                ##
                                fluidRow(
                                  column(2, actionButton('t5prev', "Previous", icon("arrow-circle-left")))
                                )
                        ) 
                      ) 
                      
                    )
                    
)  ## Webpage Details