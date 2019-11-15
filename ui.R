#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# below is the list of libraries that are used for displaying the visualization
library(shiny)
library(shinydashboard)
library(shiny)
library(plotly)
library(leaflet)
library(ggmap)
library(ggplot2)
library(magrittr)
library(tidyr)
library(tmap)
library(ggmap)
library(maps)
library(plotly)
library(dplyr)
library(corrplot)
# reading the data into R shiny
df_states <- read.csv("States_all.csv")
# renaming the columns as the column names are too long to show correct correlation plot. They usually merge into each other
names(df_states)[names(df_states) == "TOTAL_EXPENDITURE"] <- "total_exp"
names(df_states)[names(df_states) == "INSTRUCTION_EXPENDITURE"] <- "ins_exp"
names(df_states)[names(df_states) == "SUPPORT_SERVICES_EXPENDITURE"] <- "SS_exp"
names(df_states)[names(df_states) == "OTHER_EXPENDITURE"] <- "other_exp"
names(df_states)[names(df_states) == "CAPITAL_OUTLAY_EXPENDITURE"] <- "CO_exp"
names(df_states)[names(df_states) == "AVG_MATH_4_SCORE"] <- "avg_M_4"
names(df_states)[names(df_states) == "AVG_MATH_8_SCORE"] <- "avg_M_8"
names(df_states)[names(df_states) == "AVG_READING_4_SCORE"] <- "avg_R_4"
names(df_states)[names(df_states) == "AVG_READING_8_SCORE"] <- "avg_R_8"
names(df_states)[names(df_states) == "GRADES_PK_G"] <- "gr_pk_g"
names(df_states)[names(df_states) == "GRADES_KG_G"] <- "gr_kg_g"
names(df_states)[names(df_states) == "GRADES_4_G"] <- "gr_4_g"
names(df_states)[names(df_states) == "GRADES_8_G"] <- "gr_8_g"
names(df_states)[names(df_states) == "GRADES_12_G"] <- "gr_12_g"
names(df_states)[names(df_states) == "GRADES_1_8_G"] <- "gr_1_8_g"
names(df_states)[names(df_states) == "GRADES_9_12_G"] <- "gr_9_12"
names(df_states)[names(df_states) == "GRADES_ALL_G"] <- "gr_all_g"
names(df_states)[names(df_states) == "FEDERAL_REVENUE"] <- "fed_rev"
names(df_states)[names(df_states) == "TOTAL_REVENUE"] <- "total_rev"
names(df_states)[names(df_states) == "STATE_REVENUE"] <- "sta_rev"
names(df_states)[names(df_states) == "LOCAL_REVENUE"] <- "loc_rev"
x_corr <- df_states %>% select(3:24) # visualizing correlation between all variables except state and year
df_country <- read.csv("States_all.csv") # reading the same file again such that it does not have the column names modified
# Define UI for application dashboard that draws all the plots
shinyUI(dashboardPage(
  dashboardHeader(title = "Analysis of Revenue", # main purpose of the analysis
                  dropdownMenu(type = "message",
                               # defining the names of the stakeholders as a message in the app
                               messageItem(from = "State Govenment", message("Stakeholder")), 
                               messageItem(from = "Local Government", message("Stakeholder")),
                               messageItem(from = "Internal Revenue Service", message("Stakeholder")),
                               messageItem(from = "Intergovernmental administration", message("Stakeholder")),
                               messageItem(from = "FIT5147 Tutor", message("Stakeholder")),
                               messageItem(from = "FIT5147 Lecturer", message("Stakeholder")),
                               messageItem(from = "Economic Analyst ", message("Stakeholder")))),
  
  
  
  dashboardSidebar(
    # creating a side menu which wil have all the contents of the app
    sidebarMenu(
    menuItem("Dashboard",tabName = "dashboard", icon = icon("dashboard")) # defining dashboard with a dashboard icon
      # menuSubItem("Dashboard Total revenue", tabName = "total Revenue"), 
      # menuSubItem("Dashboard section analysis", tabName = "Section analysis"),
    # menuItem("Detailed Analysis"),
    # menuItem("Raw data")
  )),
  dashboardBody(
    # created three tab items for analyzing three sections of my questions
    tabItems(
      # this section contains the correlation plot information
      tabItem(tabName = "dashboard", 
              fluidRow(
                box(title = "Heatmap of correlation plot",status = "primary",solidHeader = T,plotlyOutput("Correlation"), h3("Total revenue is highly correlated with total expenditure and Enrolment. It has very less correlation with Average score. So, our analysis of total revenue will be based on how total revenue is varying across different states for different years and its variation with Expenditure and Enrolment."))
                
              )
     )) ,
    # this section contains the total revenue variation section
      tabItem(tabName = "total Revenue", h1("Total Revenue variation for different states and for different years"),
              # map displaying total revenue varitation for different states
              fluidRow(
                box(leafletOutput("mymap"),title = "Map displaying total revenue for different states",status = "primary",solidHeader = T,h3("It is clear from the map that California generates highest total revenue across all states. The possible reason for this trend may be the IT companies that have many employees working in each company. The largest source of revenue for the federal government comes from the income of its residents. When people work for a company, group, or for themselves, they are compensated for the services that they render. These working group of people are middle-aged who send their children to study. Hence, lot of revenue is generated from the education department. ")),
                box(
                  title = "Controls"
                  ,selectInput("variable", label = "Select State",choices = c(levels(df_country$STATE), "All" = "All"),
                              multiple = TRUE,
                              selected = "All")
                )
                
              ),
              # line chart displaying total revenue for different states and for different years
              fluidRow(
                box(plotlyOutput("plot"),title = "Total Revenue variation for different years",status = "primary",solidHeader = T, h3("Similar to map, the line chart displays individual trend of the states for different years. For all the years california again seems to have one of the highest revenue across all the years. The reason is that IT industries were growing at a very fast pace and it was inviting more and employees to work for them. Hence, more and more revenue was generated in the education department"))
              )),
      # this is a stepwise section where step-wise analysis of the total revenue takes place        
      tabItem(tabName = "Section analysis", h2("Section wise analysis of revenue data"),
             # Bar chart and area chart displaying different sections of FR, SR and LR 
               fluidRow(
                box(plotlyOutput("view"),title = "Bar chart and Area chart displaying Federal Revenue, State Revenue and Local Revenue distribution",status = "primary",solidHeader = T,h3("Stacked bar chart and stacked area chart explains the portion of total revenue variation for Federal Revenue, State Revenue and Local Revenue. It is obvious that Federal Revenue is greater than State Revenue and Local Revenue as these two departments come under Federal Revenue.")),
                box(title = "Controls", selectInput("chart",label = "Select type of chart",choices = c("Bar","Stepped Area"), selected = "Bar"))
                )
              ,
             # Bubble chart displaying stepwise analysis of TR
              fluidRow(
                box(plotlyOutput("motion"),title = "Bubble chart displaying Revenue variation with enrolment and expenditure for each state",status = "primary",solidHeader = T,h3("Motion chart explains the variation of selected x var and y var. A very interesting observation about the data can be made by looking at the trend of bubble chart. The country that has high revenue also has high enrolment and it also has high expenditure.") ),
                box(title = "Control x var",selectInput("xvar", "Select x variable", c("ENROLL", "TOTAL EXPENDITURE"),selected = "ENROLL")),
                box(title = "Control y var", selectInput("yvar", "Select y variable", 
                                                         c("TOTAL REVENUE", "FEDERAL REVENUE", "STATE REVENUE" ,"LOCAL REVENUE"), 
                                                         selected = "TOTAL REVENUE"))
    
              ))
         
  
)
))
  
  
