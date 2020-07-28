library(shinydashboard)
library(DT)
library(reshape)
library(stopwords)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(shinyWidgets)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape)
library(tidytext)
library(RPostgreSQL)
library(plotlyGeoAssets)
library(stringr)
library(wordcloud2)
library(stopwords)
library(tidyverse)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(igraph)
library(radarchart)
library(gridExtra)
library(circlize)
library(formattable)



ui <- dashboardPage (title = "Ingxoxo Student Engagement Analysis ", skin = "red",
                     dashboardHeader(title = strong("Ingxoxo Student Engagement Analysis ")),
                     
                     # SIDE BAR
                     dashboardSidebar (
                       
                       sidebarMenu(
                         # Select Campaign to be plotted
                         
                         id = "sbm",
                        
                         
                         menuItem("Data Analysis Report", icon = icon("line-chart"),
                                  menuSubItem("Raw Data", icon = icon("gears"),tabName = "post"),
                                  menuSubItem("Analysis", icon = icon("area-chart"), tabName = "participat"),
                                  menuSubItem("By Topic Analysis", icon = icon("line-chart"), tabName = "Topic_Post")  ,
                                  menuSubItem("Logins Analysis ", icon = icon("line-chart"), tabName = "login" ) ),
                         menuItem("Upload Data", tabName="intro_page", icon = icon("info"),
                                  fileInput('target_upload', 'Choose file to upload',
                                            accept = c(
                                              'text/csv',
                                              'text/comma-separated-values',
                                              '.csv'
                                            )),
                                  radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=";",inline=TRUE)
                         
                       ) ),
                       hr(),
                       box(
                         status = "primary",
                         solidHeader = FALSE,
                         width = 12,
                         background = "navy",
                         uiOutput("topic")
                         #  selectInput(inputId='Action_tracker',label = 'Select Action Tracker',choices="", selected = "",multiple = TRUE),
                       #  uiOutput("action_tracker_picker"),
                        # uiOutput("channel_picker"),
                        
                         
                         # selectInput(inputId='channel',label = 'Select Media Channel',choices="", selected = "",multiple = TRUE),
                         # selectInput(inputId='Publisher_id',label = 'Select Publisher_Id',choices="", selected = "",multiple = TRUE)
                         
                         
                         
                       )
                       
                       
                       
                       
                       ),
                     
                     
                     
                     dashboardBody(
                       
                       # Model Input
                       
                       tabItems (
                         
                         tabItem(tabName = "post",
                                 fluidPage (
                                   title = "Raw Data ",
                                   
                                   fluidRow(
                                     
                                     column(width = 12,
                                            box(
                                              title = "Data",
                                              status = "primary",
                                              width = 12,
                                              height = 600,
                                              solidHeader = FALSE,
                                              collapsible = TRUE,
                                              DT::dataTableOutput("all_data_analysis_2"),
                                              hr(), 
                                              DT::dataTableOutput("all_data_upload")
                                             
                                              
                                              
                                              
                                            ))
                                   )
                                   
                                   
                                   
                                   
                                   
                                   
                                 )  # end fluidpage
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                         ) ,
                         
                         # Model Diagnostic
                         
                         
                         tabItem (tabName = "participat",
                                  fluidPage (
                                    title = "Topic",
                                    
                                    fluidRow(
                                      column(width = 6,
                                             box(
                                               title = "Participants",
                                               width = 12,
                                               height = 500,
                                               background = "orange",
                                               solidHeader = FALSE,
                                               collapsible = FALSE,
                                               collapsed = FALSE,
                                               plotOutput("plot_participation")
                                              
                                          
                                             ) # end of box
                                      ),
                                      
                                      
                                      
                                      
                                      
                                      
                                      column(width = 6,
                                             box(
                                               title = "Topic Contribution by User Group",
                                               status = "primary",
                                               width = 12,
                                               height = 500,
                                               solidHeader = FALSE,
                                               collapsible = TRUE,
                                                plotOutput("plot_topic_1")
                                                
                                               
                                               
                                               
                                             ))
                                    ), # end fluid row
                                    hr() ,
                                    
                                    
                                    
                                    
                                    fluidRow (
                                      column(width = 12,
                                             box(
                                               title = "Participation Analysis 6",
                                               status = "primary",
                                               width = 12,
                                               height = 700,
                                               solidHeader = FALSE,
                                               collapsible = TRUE,
                                               DT::dataTableOutput("topic_creat",width = "100%", height = 750)
                                               
                                             ) #End of Box
                                      ) ),
                                    hr(),
                                    
                                    
                                    
                                    # Comparisons analysis
                                    fluidRow (
                                      column(width = 12,
                                             box(
                                               title = "Participation Analysis 3",
                                               status = "primary",
                                               width = 12,
                                               height = 700,
                                               solidHeader = FALSE,
                                               collapsible = TRUE,
                                               plotOutput("plot_no_stop_word",width = "100%", height = 600)
                                               
                                             ) #End of Box
                                      )
                                    ) ,
                                    
                                    fluidRow (
                                      column(width = 12,
                                             box(
                                               title = "Participation Analysis 6",
                                               status = "primary",
                                               width = 12,
                                               height = 700,
                                               solidHeader = FALSE,
                                               collapsible = TRUE,
                                               DT::dataTableOutput("text_title",width = "100%", height = 650)
                                               
                                             ) #End of Box
                                      ) ) ,
                                    
                                    fluidRow (
                                      column(width = 6,
                                             box(
                                               title = "Word Count No Stopping Words",
                                               status = "primary",
                                               width = 12,
                                               height = 800,
                                               solidHeader = FALSE,
                                               collapsible = TRUE,
                                               DT::dataTableOutput("text_1",width = "100%", height = 650)
                                              
                                               
                                             ) #End of Box
                                      ),
                                      
                                      column(width = 6,
                                             box(
                                               title = "Word Count No Stopping Words",
                                               status = "primary",
                                               width = 12,
                                               height = 800,
                                               solidHeader = FALSE,
                                               collapsible = TRUE,
                                               plotOutput("word_cloud",width = "100%", height = 600)
                                               
                                               
                                             ) #End of Box
                                      )
                                      
                                      
                                      
                                      ),
                                    
                                    fluidRow (
                                      column(width = 12,
                                             box(
                                               title = "Sentiment Topic",
                                               status = "primary",
                                               width = 12,
                                               height = 800,
                                               solidHeader = FALSE,
                                               collapsible = TRUE,
                                               DT::dataTableOutput("text_sentimentsummary",width = "100%", height = 600)
                                               
                                             ) #End of Box
                                      ) )
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                  )  # end fluidpage
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                         ),
                         
                         
                         tabItem (tabName = "Topic_Post",
                                  fluidPage (
                                    title = "Topic_Post Analysis",
                                    
                                    fluidRow(
                                      column(width = 4,
                                             box(
                                               title = "Awards/Likes Analysis",
                                               width = 12,
                                               height = 400,
                                               background = "orange",
                                               solidHeader = FALSE,
                                               collapsible = FALSE,
                                               collapsed = FALSE,
                                               h3("Awards/Likes Analysis"),
                                               p(
                                                 paste("Add text:")),
                                               tags$ul(
                                                 tags$li("Objective 1", span( style = "color:white")),
                                                 tags$li("Objective 2", span( style = "color:white")),
                                                 tags$li("Objective 3"
                                                         , span( style = "color:white")),
                                                 tags$li("Objective 4", span( style = "color:white"))
                                                 
                                               )
                                             ) # end of box
                                      ),
                                      
                                      column(width = 8,
                                             box(
                                               title = " Awards/Likes Analysis Plot 1 ",
                                               status = "primary",
                                               width = 12,
                                               height = 700,
                                               solidHeader = FALSE,
                                               collapsible = TRUE,
                                                DT::dataTableOutput("topic_question",width = "100%", height = 600)
                                               
                                               
                                             ))
                                    ), # end fluid row
                                    
                                    fluidRow(
                                      column(width = 12,
                                             box(
                                               title = "Replies by Topics",
                                               width = 12,
                                               height = 700,
                                               solidHeader = FALSE,
                                               collapsible = FALSE,
                                               collapsed = FALSE,
                                              plotOutput("plot_topic_question_views",width = "100%", height = 600)
                                               
                                               
                                             ) # end of box
                                      )
                                    )
                                    
                                    
                                    
                                    
                                    
                                    ,
                                    fluidRow (
                                      column(width = 12,
                                             box(
                                               title = "Target Post Summary",
                                               status = "primary",
                                               width = 12,
                                               height = 700,
                                               solidHeader = FALSE,
                                               collapsible = TRUE,
                                              DT::dataTableOutput("text_summary_target_post",width = "100%", height = 600)
                                              
                                              
                                             ) #End of Box
                                      )
                                    ),
                                    
                                    
                                    # Comparisons analysis
                                    fluidRow (
                                      column(width = 12,
                                             box(
                                               title = " Post Question Word Analysis
                                               ",
                                               status = "primary",
                                               width = 12,
                                               height = 900,
                                               solidHeader = FALSE,
                                               collapsible = TRUE, 
                                               plotOutput("plot_question_posts_text",width = "100%", height = 800)
                                               
                                             ) #End of Box
                                      )
                                    ) ,
                                    
                                    fluidRow (
                                      column(width = 12,
                                             box(
                                               title = "Additional Analysis",
                                               status = "primary",
                                               width = 12,
                                               height = 800,
                                               solidHeader = FALSE,
                                               collapsible = TRUE,
                                              plotOutput("word_cloud_question_topic",width = "100%", height = 750)
                                               
                                             ) #End of Box
                                      ) ) ,
                                    
                                    fluidRow (
                                      column(width = 12,
                                             box(
                                               title = "All Replies Data",
                                               status = "primary",
                                               width = 12,
                                               height = 800,
                                               solidHeader = FALSE,
                                               collapsible = TRUE,
                                              DT::dataTableOutput("data_question_response_table",width = "100%", height = 700)
                                               
                                             ) #End of Box
                                      ) ),
                                    
                                    fluidRow (
                                      column(width = 12,
                                             box(
                                               title = "All Word Cloud",
                                               status = "primary",
                                               width = 12,
                                               height = 800,
                                               solidHeader = FALSE,
                                               collapsible = TRUE,
                                               plotOutput("word_cloud_response_topic",width = "100%", height = 700)
                                               
                                             ) #End of Box
                                      ) ),
                                    
                                    fluidRow (
                                      column(width = 12,
                                             box(
                                               title = "Word Cloud Responses by User Type",
                                               status = "primary",
                                               width = 12,
                                               height = 800,
                                               solidHeader = FALSE,
                                               collapsible = TRUE,
                                               plotOutput("word_cloud_response_topic_by_topic",width = "100%", height = 700)
                                               
                                             ) #End of Box
                                      ) )  
                                    
                                    
                                    
                                    
                                    
                                  )  # end fluidpage
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                         )
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                       )
                       
                       
                       
                       
                       
                       
                       # Model Diagnostic
                       
                       
                       
                       
                       
                     ))
