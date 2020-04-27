#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(maps)
library(mapdata)
library(wesanderson)
library(ggplot2)
library(dplyr)

### Preparing the times series data

US_time_series_confirmed_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")) %>%
    select(-c(UID, iso2, iso3, code3, FIPS)) %>% 
    rename(Long = "Long_") %>%
    pivot_longer(-c(Admin2, Province_State, Country_Region, Lat, Long, Combined_Key),
                 names_to = "Date", values_to = "Confirmed") 
# Let's get the times series data for deaths
US_time_series_deaths_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")) %>%
    select(-c(UID, iso2, iso3, code3, FIPS)) %>% 
    rename(Long = "Long_") %>%
    pivot_longer(-c(Admin2, Province_State, Country_Region, Lat, Long, Combined_Key),
                 names_to = "Date", values_to = "Deaths")
# Create Keys 
US_time_series_confirmed_long <- US_time_series_confirmed_long %>% 
    unite(Key, Combined_Key, Date, sep = ".", remove = FALSE)

US_time_series_deaths_long <- US_time_series_deaths_long %>% 
    unite(Key, Combined_Key, Date, sep = ".") %>% 
    select(Key, Deaths)

# Join tables
US_time_series_long_joined <- full_join(US_time_series_confirmed_long,
                                        US_time_series_deaths_long, by = c("Key")) %>% 
    select(-Key) 

US_time_series_long_joined$Date <- mdy(US_time_series_long_joined$Date)

US_time_series <- US_time_series_long_joined 


# Get first and last date for graph ***There are NA in the date field to consider
first_date = min(US_time_series$Date, na.rm = TRUE)
last_date = max(US_time_series$Date, na.rm = TRUE)

# Defining reporting types
Report_Type = c("Confirmed", "Deaths")

# Define UI for application 
ui <- fluidPage(
    
    # Application title
    titlePanel("Representative Graphs Made Using JHU COVID-19 data"),
    p("Data for this application are from the Johns Hopkins Center for Systems Science and Engineering",
      tags$a("GitHub Repository", href="https://github.com/CSSEGISandData")
    ),
    tags$br(),
    tags$hr(),  # Adds line to page
    
    sidebarLayout(
        sidebarPanel(
            # Select Reporting type
            selectInput("select_type", 
                        label = "Report Type", 
                        choices = Report_Type, selected = "Confirmed"),
            # Select Date 
            sliderInput("slider_date", label = "Report Date", min = first_date, 
                        max = last_date, value = first_date, step = 7)
        ), 
        
        # Show a plots
        mainPanel(
            plotOutput("Plot1")
        )
    )
)


# Define server logic required to make the plot
server <- function(input, output) {
    
    output$Plot1 <- renderPlot({
        # develop data set to graph
        pick_date <- US_time_series %>%
            filter(Province_State == "Massachusetts") %>% 
            mutate(Admin2 = tolower(Admin2)) %>% 
            filter(Date == input$slider_date) %>% 
            group_by(Admin2) %>% 
            summarise_at(c("Confirmed", "Deaths"), sum)
        
        # load the world map data
        us <- map_data("state")
        ma_us <- subset(us, region == "massachusetts")
        counties <- map_data("county")
        ma_county <- subset(counties, region == "massachusetts")
        
        # We need to join the us map data with our daily report to make one data frame/tibble
        state_join <- left_join(ma_county, pick_date, by = c("subregion" = "Admin2"))
        
        # plot world map
        ggplot(data = ma_county, mapping = aes(x = long, y = lat, group = group)) + 
            coord_fixed(1.3) + 
            # Add data layer
            geom_polygon(data = state_join, aes_string(fill = input$select_type), color = "white") +
            scale_fill_gradientn(colors = 
                                     wes_palette("Zissou1", 100, type = "continuous"),
                                 trans = "log10") +
            ggtitle("JHU COVID-19 data in Massachusetts by County:", input$select_type)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)