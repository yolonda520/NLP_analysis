#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
#install.packages('font-awesome')
shinyUI( 
    dashboardPage(title = "Alcatraz Tours & Travels", skin = "yellow", #the name of our app and color of pagebar
        dashboardHeader(title = "Alcatraz Tours & Travels",
            dropdownMenu(type = "message",
                         messageItem(from = "Finance update" , message = "we are on threshold"),#where the message coem from 
                         messageItem(from = "sales update",message = "sales are at 55%",icon = icon("bar-chart"),time ="22:00"),
                         messageItem(from = "sales update",message = "sales meeting at 6 PM on MOnday",icon=icon("handshake-o"),time = "March-25 - 2018")
                         ) 
                        ),#min code 
        dashboardSidebar(#min code, side bar menu
            sidebarMenu(
                sidebarSearchForm("searchText","buttonSearch","Search"),
                menuItem("Company",tabName = "aboutus",icon = icon("users")), #?icon to looking for more icon in R
                                    #or go to fontawesome.io/icons
                 menuItem("Dashboard ",tabName = "dashboard",icon=icon("dashboard")),
                 #menuSubItem("Dashboard Sales",tabName = "sales"),
            menuItem("Prediction",tabName = "prediction", badgeLabel = "Updated",badgeColor = "green",icon = icon("bar-chart")), #create new label in the sidebar
            menuItem("Recommendation",tabName = "details",icon = icon("thumbs-up"))
        )),
        dashboardBody(#min code , the area to explain all graps
            tabItems(
                tabItem(tabName = "aboutus",
                    box("",tags$img(src = "1.png",
                                    width = 500,
                                    height =700)),
                    
                    box("",tags$img(src = "2.png",
                                    width = 500,
                                    height = 700))),
                    
                 tabItem(tabName = "dashboard",
                        
                         
                     fluidRow(
                         column(width = 12, #width of the kpi box 
                         infoBox("% of people interested in Alcatraz",paste0('76%'),icon = icon("thumbs-up"),color = "green"),
                         infoBox("% of people interested at night",paste0('70%'), icon =icon("moon"),color = "navy"),
                         infoBox("Average price",('$60-$75'), icon =icon("dollar-sign"),color = "light-blue"))),
                     
                     box(title = "Wordcloud",br(),tags$img(src = "cloud.jpeg",width = 500,height =500)),
                     
                      fluidRow(
                          tabBox(title = "Frequency word histogram",side = "right", height = "630px",width = 6,
                                 tabPanel("Question 5",br(),br(),tags$img(src = "fre_q5.png",width = 600,height = 400,icon = icon("users"))),
                                 tabPanel("Question 4",br(),br(),tags$img(src = "fre_q4.png",width = 600,height = 400,icon = icon("users"))),
                                 tabPanel("Question 3",br(),br(),tags$img(src = "fre_q3.png",width = 600,height = 400)),
                                 tabPanel("Question 2",br(),br(),tags$img(src = "fre_q2.png",width = 600,height = 400)),
                                 tabPanel("Question 1",br(),br(),tags$img(src = "fre_q1.png",width = 600,height = 400,downloadButton("downloadPlot","Download Plot")))), 
                              
                          
                     tabBox(title = "The sentiment analysis wordcloud",side = "right", height = "630px",width = 6,
                            tabPanel("Question 5",br(),tags$img(src = "bing_question5.png",width = 600,height = 400,icon = icon("users"))),
                            tabPanel("Question 4",br(),tags$img(src = "bing_question4.png",width = 600,height = 400,icon = icon("users"))),
                            tabPanel("Question 3",br(),tags$img(src = "bing_question3.png",width = 600,height = 400)),
                            tabPanel("Question 2",br(),tags$img(src = "bing_question2.png",width = 600,height = 400)),
                            tabPanel("Question 1",br(),tags$img(src = "bing_question1.png",width = 600,height = 400,downloadButton("downloadPlot","Download Plot")))),
                     
                     
                         box(title = "The bigram analysis by tf-idf",br(),tags$img(src = "bigramtfidf.png",
                                                                                  width = 650,
                                                                                height =550)),
                     
                
                     ),
                    
        
                    # tabBox(title = "The sentiment analysis wordcloud",side = "right", height = "450px",width = 12,
                    #     tabPanel("Question 5",tags$img(src = "sentiment 1.jpeg",width = 600,height = 400),icon = icon("users")),
                    #     tabPanel("Question 4",tags$img(src = "1.png",width = 300,height = 400)),
                    #     tabPanel("Question 3",tags$img(src = "1.png",width = 300,height = 400)),
                    #     tabPanel("Question 2",tags$img(src = "1.png",width = 300,height = 400)),
                    #     tabPanel("Question 1",tags$img(src = "sentiment 1.jpeg",width = 600,height = 400,downloadButton("downloadPlot","Download Plot")))),
 ################## 
                     # fluidRow(  
                     #    plotOutput("plot4tab4"),
                     #    textOutput("text4")),   
                     
                    fluidRow(
                        box(status = "primary",
                            sliderInput("orders", "Orders", min = 1, max = 100, value = 50),
                            selectInput("percent", "Progress",
                                    choices = c("0%" = 0, "20%" = 20, "40%" = 40, "60%" = 60, "80%" = 80,"100%" = 100)
                                )
                            ),
                        box(title = "Histogram box title",
                            status = "warning", solidHeader = TRUE, collapsible = TRUE,background = "yellow",
                            width = 6,height = 30,
                            plotOutput("plot", height = 250)
                        ),)
 ,
                
                    ),
                
                    tabItem(tabName = "prediction",
                            
                            box("Naïve Bayes prediction model",tags$img(src = "Slide6.jpg",
                                            width = 1200,
                                            height = 800))),
 
                            
                     tabItem(tabName = "details",
                             
                        
                        
                        
                        tabBox(title = "Sentiment in NRC",side = "right", height = "450px",width = 7,
                               tabPanel("Question 5",tags$img(src = "sentiment 1.jpeg",width = 600,height = 400),icon = icon("users")),
                               tabPanel("Question 4",tags$img(src = "1.png",width = 300,height = 400)),
                               tabPanel("Question 3",tags$img(src = "1.png",width = 300,height = 400)),
                               tabPanel("Question 2",tags$img(src = "1.png",width = 300,height = 400)),
                               tabPanel("Question 1",tags$img(src = "sentiment 1.jpeg",width = 600,height = 400,downloadButton("downloadPlot","Download Plot")))),
                       
                         box(title = "Histogram box title",
                            status = "warning", solidHeader = TRUE, collapsible = TRUE,background = "yellow",
                            width = 6,height = 30,tags$img())
                        
            )
                
                        #,tags$video(scr = "dash.mp4,width = "5000px",height = "3000px")),
                        # tabPanel(title = "Data",solidHeader = T, tableOutput("nlp"),downloadButton("downloadData","Download Data")),
                        # tabPanel(title = "Summary",solidHeader =T , verbatimTextOutput("summ")),
                        # tabPanel(title = "Plot",solidHeader = T , plotOutput("plot"),downloadButton("downloadPlot","Download Plot"))
            
            )
        )))

# fulidPage(
#     headerPanel(title = "Shiny Tabset Example"),
#     sidebarLayout(
#     sidebarPanel(
#         selectInput("ngear","Select the gear number",c("Cylinders" = "cyl","Trasmission" = "am","Gears" = "gear"))
#     ),




