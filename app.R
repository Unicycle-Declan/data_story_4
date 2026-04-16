# Sewanee app
#load packeges ----
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(DT)
library(lubridate)
library(plotly)

# load datasets ----
load('sewanee_weather.rds')

# primary dataset modifications ----
sewanee_rain <- 
  sewanee_rain %>%
  mutate(time = make_date(year = sewanee_rain$year,
                          month = 1,
                          day = 1))
sewanee_temp <- 
  sewanee_temp %>%
  mutate(time = paste(month, year),
         temp = as.numeric(temp)) %>%
  mutate(time = my(time)) %>%
  filter(time != is.na(time))

# UI ----
ui <- fluidPage(
  titlePanel("Weather Data for Sewanee, TN"),
  tabsetPanel(
    ## temp tab ----
    tabPanel(h5("Tempature"),
             fluidRow(
               ### date ranges ----
               column(4, sliderInput(inputId = "temp_year",
                                     label = "timeframe",
                                     min = min(sewanee_temp$time),
                                     max = max(sewanee_temp$time),
                                     value = range(sewanee_temp$time)
                                        )
                      ),
               ### month selector ----
               column(4, selectInput(inputId = "month",
                                        label = "month",
                                        multiple = FALSE,
                                        choices = c(unique(sewanee_temp$month),
                                                    ""),
                                        selected = "")
                      ),
               column(1),
               ### stat checkboxes ----
               column(1, checkboxInput(inputId = "avg",
                                       label = "average",
                                       value = TRUE
                                       )
                      ),
               column(1, checkboxInput(inputId = "max",
                                       label = "maximum",
                                       value = TRUE
                                       )
                      ),
               column(1, checkboxInput(inputId = "min",
                                       label = "minimum",
                                       value = TRUE
                                       )
                      )
               ),
             ### plot ----
             fluidRow(
               column(1),
               column(10, plotlyOutput("Temperature_plot")),
               column(1)
             ),
             fluidRow()
             ),
    ## rainfall tab ----
    tabPanel(h5("Rainfall"),
             fluidRow(
               column(6, sliderInput(inputId = "year",
                                     label = "timeframe",
                                     min = min(sewanee_rain$time),
                                     max = max(sewanee_rain$time),
                                     value = range(sewanee_rain$time)
                                     )
                      ),
               column(6, selectInput(inputId = "scale",
                                     label = "scale",
                                     multiple = FALSE,
                                     choices = c("year","season","month"),
                                     selected = "year")
                      )
               ),
             br(),
             br(),
             fluidRow(
               column(1),
               column(10,plotlyOutput("Rainfall_plot")),
               column(1)
             ),
             fluidRow()
      
    )
  )

)

# Server ----
server <- function(input, output) {
  # temp plot ----
  
  output$Temperature_plot <-
    renderPlotly({
      ## functions ----
      temp_month_plot <- 
        function(m, data){
          data %>%
            filter(month == m) %>%
            ggplot(aes(x = time,
                       y = temp,
                       color = stat)) +
            geom_path() +
            labs(x = "year",
                 y = "temperature if °F") +
            xlim(input$temp_year)
        }
      ## stat checkboxes ----
      filtered_temp <- sewanee_temp
      
      if (input$avg == FALSE){
        filtered_temp <- 
          filtered_temp %>%
          filter(stat != "avg")
      }
      if (input$min == FALSE){
        filtered_temp <- 
          filtered_temp %>%
          filter(stat != "min")
      }
      if (input$max == FALSE){
        filtered_temp <- 
          filtered_temp %>%
          filter(stat != "max")
      }
      
      ## year round ----
      if (input$month == "" ){
        ggplot(filtered_temp,
               aes(x = time,
                   y = temp,
                   color = stat)) +
          geom_path() +
          labs(x = "year",
               y = "temperature if °F") +
          xlim(input$temp_year)
        ## for months ----
      } else if(input$month == "January"){
        temp_month_plot(m = "January", data = filtered_temp)
      } else if(input$month == "February"){
        temp_month_plot(m = "February", data = filtered_temp)
      } else if(input$month == "March"){
        temp_month_plot(m = "March", data = filtered_temp)
      } else if(input$month == "April"){
        temp_month_plot(m = "April", data = filtered_temp)
      } else if(input$month == "May"){
        temp_month_plot(m = "May", data = filtered_temp)
      } else if(input$month == "June"){
        temp_month_plot(m = "June", data = filtered_temp)
      } else if(input$month == "July"){
        temp_month_plot(m = "July", data = filtered_temp)
      } else if(input$month == "August"){
        temp_month_plot(m = "August", data = filtered_temp)
      } else if(input$month == "September"){
        temp_month_plot(m = "September", data = filtered_temp)
      } else if(input$month == "October"){
        temp_month_plot(m = "October", data = filtered_temp)
      } else if(input$month == "November"){
        temp_month_plot(m = "November", data = filtered_temp)
      } else if(input$month == "December"){
        temp_month_plot(m = "December", data = filtered_temp)
      }
      
      })
  

  # rainfall plot ----
  output$Rainfall_plot <- 
    renderPlotly({
      
      ## for year ----
      if(input$scale == "year"){
        ### manipulate data ----
        sewanee_rain_year <-
          sewanee_rain %>% 
          group_by(time) %>%
          summarise(total_inches = sum(inches, na.rm = TRUE)) 
        
        ### plot ----
        ggplot(sewanee_rain_year,
               aes(x = time,
                   y = total_inches)) +
          geom_area(fill = 'blue') +
          xlim(input$year) +
          labs(y = "total rainfall (in)")
      }
      ## for season ----
      else if(input$scale == "season"){
        ### define seasons ----
        spring <- c("March","April","May")
        summer <- c("June","July","August")
        autumn <- c("September","October","November")
        winter <- c("December","January","February")
        
        ### manipulate data ----
        sewanee_rain_season <- 
          sewanee_rain %>%
          mutate(season = month)
        
        for (i in spring){
          sewanee_rain_season <-
            sewanee_rain_season %>%
            mutate(season = gsub(i, "spring", sewanee_rain_season$season))
        }
        
        for (i in summer){
          sewanee_rain_season <-
            sewanee_rain_season %>%
            mutate(season = gsub(i, "summer", sewanee_rain_season$season))
        }
        
        for (i in autumn){
          sewanee_rain_season <-
            sewanee_rain_season %>%
            mutate(season = gsub(i, "autumn", sewanee_rain_season$season))
        }
        
        for (i in winter){
          sewanee_rain_season <-
            sewanee_rain_season %>%
            mutate(season = gsub(i, "winter", sewanee_rain_season$season))
        }
        
        sewanee_rain_season <-
          sewanee_rain_season %>%
          group_by(year, season) %>%
          summarise(total_inches = sum(inches, na.rm = TRUE)) %>%
          mutate(ID = row_number()) 
        
        sewanee_rain_season$ID <- gsub("1", "09", sewanee_rain_season$ID)
        sewanee_rain_season$ID <- gsub("2", "03", sewanee_rain_season$ID)
        sewanee_rain_season$ID <- gsub("1", "06", sewanee_rain_season$ID)
        sewanee_rain_season$ID <- gsub("1", "11", sewanee_rain_season$ID)
        
        sewanee_rain_season <- 
          sewanee_rain_season %>% 
          mutate(time = paste(ID, year)) %>%
          mutate(time = my(time))
        
        ### plot ----
        ggplot(sewanee_rain_season,
               aes(x = time,
                   y = total_inches)) +
          geom_area(fill = "blue") +
          xlim(input$year) +
          labs(y = "total rainfall (in)")
      }
      ## for month ----
      else if(input$scale == "month"){
        ### manipulate data ----
        sewanee_rain_month <- 
          sewanee_rain %>% 
          mutate(time = paste(month, year)) %>%
          mutate(time = my(time))
        
        ### plot ----
        ggplot(sewanee_rain_month,
               aes(x = time,
                   y = inches))+
          geom_area(fill = "blue") +
          xlim(input$year) +
          labs(y = "total rainfall (in)")
      }
    })

}

# Run the application ----
shinyApp(ui = ui, server = server)
