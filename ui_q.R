#install.packages("tableHTML")

library(shiny)
library(shinydashboard)
library(plotly)
library(tableHTML)

shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Dating App'),
    
    dashboardSidebar(
      menuItem("Problem Statement", tabName = "tab1"),
      menuItem("Insight 1", tabName = "tab2"),
      menuItem("Insight 2", tabName = "tab3"),
      menuItem("Insight 3", tabName = "tab4"),
      menuItem("Recommendations", tabName = "tab5")
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "tab1",
                fluidRow(
                  valueBox(
                    value = "$6.4B",
                    subtitle = "Dating app market value",
                    color = "maroon",
                    icon = icon("money")
                  ),
                  valueBox(
                    value = "$9.2B",
                    subtitle = "By 2025",
                    color = "maroon",
                    icon = icon("money")
                  ),
                  valueBox(
                    value = "25.1M",
                    subtitle = "Active users ",
                    color = "maroon",
                    icon = icon("users")
                  ),
                  infoBox(
                    title = "Prolem Statement",
                    subtitle = "XXXX Dating app is currently loosing customers and 
                    wants us to come with Business Insights to ensure that the companies 
                    grows and maximizes its profit. ",
                    width = 12
                  )
                  )),
        tabItem(tabName="tab2",
                fluidRow(
                  infoBox("Insights",
                          color = "teal", icon = icon("star"), 
                          width=12,
                          "•	love movies
                          •	Love Sports
                          •	Love Travelling
                          •	Love Reading
                          •	Love hanging out with loved ones"),
                  plotOutput("plot4tab2"))
                ),
        tabItem(tabName="tab3",
                fluidRow(
                  plotOutput("plot4tab3"),
                  textOutput("text3")
                  )),
        tabItem(tabName = "tab4",
                fluidRow(
                  plotOutput("plot4tab4"),
                  textOutput("text4")
                  )),
        tabItem(tabName = "tab5",
                fluidRow(
                  textOutput("text5")
                ))
    )
  )
)
)









# h2("tf_idf"),
# 
# fluidRow(
#      gradientBox(
#             title = "the name of the grapho",icon = icon("thumbs-up"), gradientColor = "black",
#             boxToolSize = "md", 
#             footer = sliderInput("num1","Slide here....", min = 5 , max =15 ,
#                                                 value = 10),"select the number...."
#     )
#     
# ),
# plotOutput("tf_idf")





