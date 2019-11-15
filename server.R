#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# libraries used for implemetation
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

# Define server logic required to draw all the plots in the dashboard one by one
shinyServer(function(input, output) {
  ####### Section to create correlation plot ##### 
  output$Correlation<- renderPlotly({
    x_corr <- df_states %>% select(3:24) # selecting columns that do not have state and year
      rt <- cor(x_corr) # finding correlation between two plots
      p <- plot_ly(x = colnames(x_corr), y = colnames(x_corr), z =rt , type = "heatmap", colors = colorRamp(c("red", "yellow")))
      p
    })
  ###### Section to create line plot #####
  output$plot <-  renderPlotly({
  df_country <- read.csv("States_all.csv")
    if (input$variable == "All"){
      new_df <- df_country
    }
    else{
      new_df <- df_country[df_country$STATE == input$variable,]  # relating input variables with states
    }
    # using ggplot to create line chart 
    TR_chart <- ggplot(data = new_df, aes(x = YEAR, y =TOTAL_REVENUE, text = paste0("<b>Year: </b>", YEAR,"<br>","<b>Total Revenue: </b> $", TOTAL_REVENUE," million<br>","<b>States: </b>", new_df$STATE ,"<br>", "<b>Enrolment: </b>", ENROLL ),group = 1)) +
      geom_point(aes(color = new_df$STATE)) + geom_line(aes(color = new_df$STATE)) 
    # adding interactivity to the line chart
    TR_interactive <- ggplotly(TR_chart, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(hoverlabel = list(bgcolor = "white",
                               font = list(family = "Georgia")))
    
    print(TR_interactive)
    
    
  })
  ###### Section to create map #####
  output$mymap <- renderLeaflet({
    # reading the file for creating map leaflet
    df_country <- read.csv("States_all.csv")
    if (input$variable == "All"){
      new_df <- df_country
    }
    else{
      new_df <- df_country[df_country$STATE == input$variable,]  
    }
    # taking average total revenue for each state
    ag <- aggregate(new_df$TOTAL_REVENUE ~ new_df$STATE, FUN = mean, data = new_df)
    ag$`new_df$STATE` <- gsub("\\_", " ",ag$`new_df$STATE`)
    ag$`new_df$STATE` <- tolower(ag$`new_df$STATE`)
    # map used for creating map states
    mapStates <- map("state", fill = TRUE, plot = FALSE)
    # split the string with : as seperator
    spliteNames <- strsplit(mapStates$names, ":")
    # get first part of the origin string;
    # e.g. get washington from washington:san juan island
    firstPartNames <- lapply(spliteNames, function(x) x[1])
    rates <- ag$`new_df$TOTAL_REVENUE`[match(firstPartNames, ag$`new_df$STATE`)]
    pal <- colorBin("YlOrRd", domain = rates) # prepare the color mapping
    m <- leaflet(mapStates) # create a blank canvas
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Average total revenue",
      firstPartNames, rates
    ) %>% lapply(htmltools::HTML)
    m <- m %>% addPolygons( # draw polygons on top of the base map (tile)
      fillColor = ~pal(rates), # fill colour
      weight = 2,
      opacity = 1,
      color = "black",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions( # highlighting the boundaries of map
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 1,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions( # label will display total revenue and state
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
      
    )
    # adding legend to the graph
    m %>% addLegend(pal = pal, values = rates, opacity = 0.7, title = "Average total revenue",
                    position = "bottomright")
    
  })
  
  
  ###### Section to create stacked bar chart and area chart #####
  # creating a stacked bar chart and area chart
output$view <- renderPlotly({
  # if the variable is bar then ggplot will create bar chart
  if(input$chart == "Bar"){
    df_bar <- read.csv("Agg_final_area.csv", header = T)
    df_bar$YEAR <- as.factor(df_bar$YEAR)
    gx <- ggplot(data=df_bar, aes(x=YEAR, y=Values, fill=df_bar$Revenue)) +
      geom_bar(stat="identity")
    ggplotly(gx)
    
  }
  else{
    # create an area chart otherwise
    df_bar <- read.csv("Agg_final_area.csv", header = T)
    p2 <- ggplot() + geom_area(aes(y = Values, x = YEAR, fill = Revenue), data = df_bar,
                               stat="identity")
    ggplotly(p2)
  }
})

######Section to create motion chart#####
output$motion <- renderPlotly({
  # creating a bubble chart for motion chart
  df_country <- read.csv("States_all.csv")
  agg_motion <- df_country %>% select(1:24) 
  agg_motion <- agg_motion %>% # only taking mean values for of the states(as this has many years and states)
    group_by(STATE) %>% 
    summarise_each(funs(mean))
  agg_motion_1 <- agg_motion[,-2] # deleting the year row so that it does not disturn the column
  colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951') # color palette
  # using conditions for choosing x and y variable
  # Enrol variable for x axis and TR on y axis
  # creating bubble plot using plotly
  if( (input$xvar == "ENROLL")&(input$yvar == "TOTAL REVENUE") ){
    plot_ly(agg_motion_1, x = ~ENROLL, y = ~TOTAL_REVENUE, color = ~STATE, size = ~TOTAL_REVENUE, colors = colors,
            type = 'scatter', mode = 'markers', 
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF')),
            text = ~paste('State:', STATE, '<br>Total Revenue:', TOTAL_REVENUE, '<br>ENROLL:', ENROLL
            )) 
    
  }
  # choosing input x and y var again using plotly
  else if((input$xvar == "ENROLL") & (input$yvar == "FEDERAL REVENUE")){
    plot_ly(agg_motion_1, x = ~ENROLL, y = ~FEDERAL_REVENUE, color = ~STATE, size = ~FEDERAL_REVENUE, colors = colors,
            type = 'scatter', mode = 'markers',
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF')),
            text = ~paste('State:', STATE, '<br>Federal Revenue:', FEDERAL_REVENUE, '<br>ENROLL:', ENROLL
            ))
    
  }
  # choosing input x and y var again using plotly
  else if((input$xvar == "ENROLL") & (input$yvar == "STATE REVENUE")){
    plot_ly(agg_motion_1, x = ~ENROLL, y = ~STATE_REVENUE, color = ~STATE, size = ~STATE_REVENUE, colors = colors,
            type = 'scatter', mode = 'markers',
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF')),
            text = ~paste('State:', STATE, '<br>State Revenue:', STATE_REVENUE, '<br>ENROLL:', ENROLL
            ))
    
  }
  # choosing input x and y var again using plotly
  else if((input$xvar == "TOTAL EXPENDITURE") & (input$yvar == "TOTAL REVENUE")){
    plot_ly(agg_motion_1, x = ~TOTAL_EXPENDITURE, y = ~TOTAL_REVENUE, color = ~STATE, size = ~TOTAL_REVENUE, colors = colors,
            type = 'scatter', mode = 'markers',
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF')),
            text = ~paste('State:', STATE, '<br>Total Revenue:', TOTAL_REVENUE, '<br>Total Expenditure:', TOTAL_EXPENDITURE
            ))
    
  }
  # choosing input x and y var again using plotly
  else if(input$xvar == "TOTAL EXPENDITURE" & input$yvar == "FEDERAL REVENUE"){
    plot_ly(agg_motion_1, x = ~TOTAL_EXPENDITURE, y = ~FEDERAL_REVENUE, color = ~STATE, size = ~FEDERAL_REVENUE, colors = colors,
            type = 'scatter', mode = 'markers',
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF')),
            text = ~paste('State:', STATE, '<br>Federal Revenue:', FEDERAL_REVENUE, '<br>Total Expenditure:', TOTAL_EXPENDITURE
            ))
    
  }
  # choosing input x and y var again using plotly
  else if((input$xvar == "TOTAL EXPENDITURE") & (input$yvar == "STATE REVENUE")){
    plot_ly(agg_motion_1, x = ~TOTAL_EXPENDITURE, y = ~STATE_REVENUE, color = ~STATE, size = ~STATE_REVENUE, colors = colors,
            type = 'scatter', mode = 'markers',
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF')),
            text = ~paste('State:', STATE, '<br>State Revenue:', STATE_REVENUE, '<br>Total Expenditure:', TOTAL_EXPENDITURE
            ))
    
  }
  # choosing input x and y var again using plotly
  else if((input$xvar == "TOTAL EXPENDITURE") & (input$yvar == "LOCAL REVENUE")){
    plot_ly(agg_motion_1, x = ~TOTAL_EXPENDITURE, y = ~LOCAL_REVENUE, color = ~STATE, size = ~LOCAL_REVENUE, colors = colors,
            type = 'scatter', mode = 'markers',
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF')),
            text = ~paste('State:', STATE, '<br>Local Revenue:', LOCAL_REVENUE, '<br>Total Expenditure:', TOTAL_EXPENDITURE
            ))
    
  }
  # choosing input x and y var again using plotly
  else if((input$xvar == "ENROLL") & (input$yvar == "LOCAL REVENUE")){
    plot_ly(agg_motion_1, x = ~ENROLL, y = ~LOCAL_REVENUE, color = ~STATE, size = ~LOCAL_REVENUE, colors = colors,
            type = 'scatter', mode = 'markers',
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF')),
            text = ~paste('State:', STATE, '<br>Local Revenue:', LOCAL_REVENUE, '<br>ENROLL:', ENROLL
            ))
    
  }
})
  
})
