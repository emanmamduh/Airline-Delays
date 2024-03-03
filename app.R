library(dplyr)
library(shiny)
library(DT)
library(shinydashboard)
library(ggplot2)
library(highcharter)
library(plotly)
library(shinyjs)
library(magrittr)
library(prompter)
#-------------------------------------------------------------------------
# Read dataset
#file_path <- "airline_delay.csv"
#data <- read.csv(file_path)
file_path <- "airline_delay.csv"
data <- read.csv(file_path)
data <- data %>%
mutate(date = as.Date(paste(year, month, "01", sep = "-")))
#--------------------------------------------------------------------------
# Preprocessing: Convert numeric columns to integer columns
numeric_columns <- c("carrier_ct", "weather_ct", "nas_ct", "security_ct", "late_aircraft_ct")
data[, numeric_columns] <- lapply(data[, numeric_columns], as.integer)
#--------------------------------------------------------------------------
# Remove NA values from data
your_data_no_na <- na.omit(data)
#---------------------------------------------------------------------------
# Calculate the total number of delays, cancellations, and diverted flights for each year
result <- your_data_no_na %>%
  group_by(year) %>%
  summarise(
    total_delays = sum(arr_del15, na.rm = TRUE),
    total_cancellations = sum(arr_cancelled, na.rm = TRUE),
    total_diverted = sum(arr_diverted, na.rm = TRUE)
  )

# View the resulting summary
print(result)
#----------------------------------------------------------------------------------------
#Calculate the total number of delays, cancellations, and diverted flights for each carrier name.
result <- your_data_no_na %>%
  group_by(carrier_name) %>%
  summarise(
    total_delays = sum(arr_del15, na.rm = TRUE),
    total_cancellations = sum(arr_cancelled, na.rm = TRUE),
    total_diverted = sum(arr_diverted, na.rm = TRUE)
  )
print(result)
#--------------------------------------------------------------------------------------------
#Calculate the average delay time for each carrier.
result <- your_data_no_na %>%
  group_by(carrier_name) %>%
  summarise(
    avg_delay_time = mean(arr_delay, na.rm = TRUE)
  )
print(result)
#--------------------------------------------------------------------------------------------
#Calculate the average delay time for each year.
data %>%
  group_by(year) %>%
  summarise(
    avg_delay_time = mean(arr_delay, na.rm = TRUE)
  )
#--------------------------------------------------------------------------------------------
# Calculate the average number of cancelled flights for each year
data %>%
  group_by(year) %>%
  summarise(
    avg_cancelled_flights  = mean(arr_cancelled, na.rm = TRUE)
  )
#--------------------------------------------------------------------------------------------
## Calculate the average number of diverted flights for each year
data %>%
  group_by(year) %>%
  summarise(
    avg_diverted_flights = mean(arr_diverted, na.rm = TRUE)
  )
#-------------------------------------------------------------------------------------------
#Calculate the proportion of delayed flights (more than 15 minutes) for each carrier.
data %>%
  group_by(carrier_name) %>%
  summarise(
    #نسبه تاخير الرحلات ف كل شركه
    proportion_delayed= sum(arr_del15 > 0, na.rm = TRUE) / n()
  )
#---------------------------------------------------------------------------------------------
#Compare the average delay times and cancellation rates between different airlines.
data %>%
  group_by(carrier_name) %>%
  summarise(
    avg_delay_time = mean(arr_del15, na.rm = TRUE),
    cancellation_rate = mean(arr_cancelled > 0, na.rm = TRUE)
    
  )
#------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# Convert 'date' column to Date type (if not already)
#data$date <- as.Date(data$date)
#-----------------------------------------------------------------------------------------------
default_Choice <- unique(data$carrier)[1:2] 
# “blue”, “black”, “purple”, “green”, “red”, “yellow”
# Define UI for application that draws a histogram
ui<-dashboardPage(skin = "black",
                  dashboardHeader(
                    #title = "My Shiny Dashboard"),
                    title = tags$span(
                      style = "font-family: Arial, sans-serif; font-size: 19px; font-weight: bold; text-transform: capitalize ; titleWidth = 400;align = left",
                      div(icon("plane-departure", class = "fa fa-plane fa-lg"), " Flights-Delay"))
                    #title = div(icon("plane-departure", class = "fa fa-plane fa-lg"), " Flights-Delay")
                  ),
                  dashboardSidebar(
                    # Sidebar content (input controls)
                    sidebarMenu(
                      actionButton("toggleButton", "Dark Mode", class = "btn btn-default" , icon = icon("moon")), #toggle modes
                      menuItem("Home", tabName = "tab1", icon = icon("home")),
                      menuItem("Statistics Charts", tabName = "tab2", icon = icon("chart-simple"))
                      # Add more menu items as needed
                      
                    )
                  ),
                  #---------------------------------------------------------------------------------------------------------------------
                  dashboardBody(
                    use_prompt(),
                    
                    #dark/light mode (CSS)
                    useShinyjs(),
                    tags$style(
                      HTML("
           body.light-background {
             background-color: #fff;
             color: #000;
           }
           .box.light-background {
             background-color: #fff;
             color: #000;
           }
           body.dark-background {
             background-color: #222;
             color: #fff;
           }
           .dark-background {
      background-color: #222d32; /* Change this to your desired dark color */
      color: #fff;
    }
           .selected-tab {
             background-color:#222;
             color: #fff;
           }
           .skin-blue .main-header {
          background-color: #222d32;
        }
        
        .skin-blue .main-sidebar {
          background-color: #222d32;
        }
        
        .skin-blue .content-wrapper {
          background-color: #000103;
          }
           
    
          ")
                    ),
                    #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                    # Body content (main panel)
                    tabItems(
                      # Tab 1 content
                      tabItem(
                        tabName = "tab1",
                        fluidRow(

                          box(
                            width = 12,
                            title = "Choose Airport",
                            status = "primary",
                            solidHeader = TRUE,
                            background = "navy",
                            borderRadius = "25px",
                            dateRangeInput("dateRange", "Select Date Range",
                                           start = min(data$date), end = max(data$date)),
                            selectInput("airport", "Select Airport", choices = unique(data$airport)),
                            plotlyOutput("flightsPlot")
                          ),
                          
                          box(
                            width = 12,
                            title = "Select Airport Carriers",
                            status = "primary",
                            solidHeader = TRUE,
                            background = "navy",
                            borderRadius = "25px",
                            checkboxGroupInput("selected_carriers", "Carriers",
                                               choices = unique(data$carrier),
                                               selected = default_Choice , inline = TRUE),
                            plotOutput("barChart")
                          ),
                          
                          box(
                            width = 6,
                            title = "Filter Data",
                            status = "primary",
                            solidHeader = TRUE,
                            background = "navy",
                            borderRadius = "25px",
                            selectInput("unit", "Unit:", c("Absolute Values", "Percentage Change")),
                            selectInput("carrier1", "Carrier:", unique(data$carrier)),
                            selectInput("month", "Month:", sort(unique(data$month), decreasing = FALSE)),
                            plotOutput("scatterPlot")
                          ),
                          
                          box(
                            width = 6,
                            title = "Select Data Type",
                            status = "primary",
                            solidHeader = TRUE,
                            background = "navy",
                            borderRadius = "25px",
                            radioButtons("data_type", "Data Type:",
                                         choices = c("Daily", "Cumulative"),
                                         selected = "Daily"),
                            plotOutput("scatterPlot1")
                          )
                        )#end of fluidRow
                      ),#end of tab1
                      
                      
                      #----------------------------------------------------------
                      #), # Close tabItem for "tab1"
                      
                      # Tab 2 content for the bar plot
                      tabItem(
                        tabName = "tab2",
                        # Display the DataTable based on the selected year range
                        
                        fluidRow(
                          
                          
                          box(
                            add_prompt(
                              checkboxInput("show_chart1", "Chart1", value = TRUE),
                              position = "bottom", message = "Hide This Chart"
                            ),
                            plotlyOutput("chart1"),
                            actionButton("DetailsButton1", "Details")
                          ),
                          
                          box(
                            #"Chart 2: Monthly Distribution of Delayed Flights by Carrier",
                            add_prompt(
                              checkboxInput("show_chart2", "Chart2", value = TRUE),
                              position = "bottom", message = "Hide This Chart"
                            ),
                            plotlyOutput("chart2"),
                            actionButton("DetailsButton2", "Details")
                          ),
                          
                          box(
                            #"Chart 3: Proportion of Cancellations by Carrier",
                            add_prompt(
                              checkboxInput("show_chart3", "Chart3", value = TRUE),
                              position = "bottom", message = "Hide This Chart"
                            ),
                            plotlyOutput("chart3"),
                            actionButton("DetailsButton3", "Details")
                          ),
                          
                          box(
                            #"Chart 4: Year Distribution of Cancelled Flights",
                            add_prompt(
                              checkboxInput("show_chart4", "Chart4", value = TRUE),
                              position = "bottom", message = "Hide This Chart"
                            ),
                            plotlyOutput("chart4"),
                            actionButton("DetailsButton4", "Details")
                          ),
                          box(
                            #"Chart 5: Year Distribution of Diverted Flights",
                            add_prompt(
                              checkboxInput("show_chart5", "Chart5", value = TRUE),
                              position = "bottom", message = "Hide This Chart"
                            ),
                            plotlyOutput("chart5"),
                            actionButton("DetailsButton5", "Details")
                          ),
                          box(
                            #"Chart 6: Year Distribution of Delayed Flights due to Weather",
                            add_prompt(
                              checkboxInput("show_chart6", "Chart6", value = TRUE),
                              position = "bottom", message = "Hide This Chart"
                            ),
                            plotlyOutput("chart6"),
                            actionButton("DetailsButton6", "Details")
                          ),
                          box(
                            #"Chart 7: Heatmap of Delay Times Over years and Carriers",
                            add_prompt(
                              checkboxInput("show_chart7", "Chart7", value = TRUE),
                              position = "bottom", message = "Hide This Chart"
                            ),    
                            plotlyOutput("chart7"),
                            actionButton("DetailsButton7", "Details")
                          ),
                          #--------------------------------------------
                          box(
                            #"Chart 8: Relationship Between Carrier and Cancellations",
                            add_prompt(
                              checkboxInput("show_chart8", "Chart8", value = TRUE),
                              position = "bottom", message = "Hide This Chart"
                            ),
                            plotlyOutput("chart8"),
                            actionButton("DetailsButton8", "Details")
                          ),
                          box(
                            #"Chart 9: Delay Types Over Years by Airport",
                            add_prompt(
                              checkboxInput("show_chart9", "Chart9", value = TRUE),
                              position = "bottom", message = "Hide This Chart"
                            ),
                            plotlyOutput("chart9"),
                            actionButton("DetailsButton9", "Details")
                          ),
                          box(
                            #"Chart 10: Histogram of Arrival Delays",
                            add_prompt(
                              checkboxInput("show_chart10", "Chart10", value = TRUE),
                              position = "bottom", message = "Hide This Chart"
                            ),
                            plotlyOutput("chart10"),
                            actionButton("DetailsButton10", "Details")
                          ),
                          box(
                            #"Chart 11: Diverted Flights vs. Cancellations by Carrier",
                            add_prompt(
                              checkboxInput("show_chart11", "Chart11", value = TRUE),
                              position = "bottom", message = "Hide This Chart"
                            ),
                            plotlyOutput("chart11"),
                            actionButton("DetailsButton11", "Details")
                          )
                          
                        ) # Close fluei3 tabItem for "tab3"
                        #------------------------------------------------------------------------------------------------
                        #------------------------------------------------------------------------------------------------
                      ) # Close tab2
                    ) # Close tabitems dashboardBody
                  ) # Close dashboardBody
) #close dashboardpage


# Define server
server <- function(input, output, session) {
  
  flights <- reactive({
    data %>% 
      filter(date >= input$dateRange[1], 
             date <= input$dateRange[2],
             airport == input$airport)
  })
  
  output$flightsPlot <- renderPlotly({
    plot_ly(data = flights(), x = ~month) %>%
      add_bars(y = ~nrow(flights()), name = "Flights Count",hoverinfo = "none")%>%
      layout(yaxis = list(title = "Flights"))
  })
  
  #Checkboxes in tab1
  selected_carriers <- reactive({
    carriers_selected <- input$selected_carriers
    carriers_selected
  })
  output$barChart <- renderPlot({
    ggplot(filter(data, carrier %in% selected_carriers()), aes(x = carrier_name)) +
      geom_bar(fill = "skyblue") +
      labs(title = "Delayed Flights by Carrier",
           x = "Carrier Name",
           y = "Flights")
  })
  
  
  #Details buttons
  observeEvent(input$DetailsButton1, {
    showModal(modalDialog(
      title = "Details",
      "This line chart depicts the total number of flights over different carriers,Carrier X has the highest total flights, followed by carriers Y and Z",
      footer = actionButton("okButton", "OK")
    ))
  })
  
  observeEvent(input$okButton, {
    removeModal()
  })
  observeEvent(input$DetailsButton2, {
    showModal(modalDialog(
      title = "Details",
      "This Stacked bar chart illustrates the monthly distribution of delayed flights by carrier.
       Carriers may experience varying degrees of delays during different months.
       Identify months with a higher frequency of delays for specific carriers.",
      footer = actionButton("okButton", "OK")
    ))
  })
  
  observeEvent(input$okButton, {
    removeModal()
  })
  observeEvent(input$DetailsButton3, {
    showModal(modalDialog(
      title = "Details",
      "This pie chart shows the proportion of flight cancellations for each carrier.
      Carrier A has the highest proportion of cancellations, while others contribute less.
      Useful for understanding the distribution of cancellations among carriers.",
      footer = actionButton("okButton", "OK")
    ))
  })
  
  observeEvent(input$okButton, {
    removeModal()
  })
  observeEvent(input$DetailsButton4, {
    showModal(modalDialog(
      title = "Details",
      "This bar chart presents the yearly distribution of cancelled flights.
       Check for any significant increase or decrease in cancellations over the years.
       Identify years with a notable impact on cancelled flights.",
      footer = actionButton("okButton", "OK")
    ))
  })
  
  observeEvent(input$okButton, {
    removeModal()
  })
  observeEvent(input$DetailsButton5, {
    showModal(modalDialog(
      title = "Details",
      "this chart focuses on the distribution of diverted flights.
       Observe trends or patterns in flight diversions over different years.",
      footer = actionButton("okButton", "OK")
    ))
  })
  
  observeEvent(input$okButton, {
    removeModal()
  })
  observeEvent(input$DetailsButton6, {
    showModal(modalDialog(
      title = "Details",
      "Depicts the yearly distribution of delayed flights specifically due to weather.
       Identify years with a higher impact of weather-related delays.
       Explore potential correlations with specific weather events.",
      footer = actionButton("okButton", "OK")
    ))
  })
  
  observeEvent(input$okButton, {
    removeModal()
  })
  observeEvent(input$DetailsButton7, {
    showModal(modalDialog(
      title = "Details",
      "The heatmap displays delay times over the years for different carriers.
      Quickly identifies carriers and years with the highest delay times.
      Enables spotting patterns or clusters of delay occurrences.",
      footer = actionButton("okButton", "OK")
    ))
  })
  
  observeEvent(input$okButton, {
    removeModal()
  })
  observeEvent(input$DetailsButton8, {
    showModal(modalDialog(
      title = "Details",
      "Scatter plot showing the relationship between carriers and cancellations.
       Identify carriers with a higher rate of cancellations.
       Assess whether carriers with more flights also experience more cancellations.",
      footer = actionButton("okButton", "OK")
    ))
  })
  
  observeEvent(input$okButton, {
    removeModal()
  })
  observeEvent(input$DetailsButton9, {
    showModal(modalDialog(
      title = "Details",
      "Stacked bar chart showcasing different types of delays over the years.
       Identify which type of delay contributes most to the overall delays.
      Explore trends in the occurrence of specific delay types.",
      footer = actionButton("okButton", "OK")
    ))
  })
  
  observeEvent(input$okButton, {
    removeModal()
  })
  observeEvent(input$DetailsButton10, {
    showModal(modalDialog(
      title = "Details",
      "Histogram depicting the distribution of arrival delays.
      Check for the central tendency and spread of arrival delays.
      Identify common delay durations and their frequencies.This chart displays the total number of flights over different carriers.",
      footer = actionButton("okButton", "OK")
    ))
  })
  
  observeEvent(input$okButton, {
    removeModal()
  })
  observeEvent(input$DetailsButton11, {
    showModal(modalDialog(
      title = "Details",
      "Bubble chart comparing the total number of diverted flights to cancellations by carrier.
Identify carriers with a higher proportion of cancellations or diversions.
Explore potential relationships between these two factors.",
      footer = actionButton("okButton", "OK")
    ))
  })
  #End Of Details Button
  
  
  
  #for dark/light button
  observeEvent(input$toggleButton, {
    shinyjs::toggleClass(selector = "body", class = "skin-blue")
    shinyjs::toggleClass(selector = ".box", class = "dark-background")
  })
  
  #tab1 Dropdown list code
  output$dataTable <- renderDT({
    Dropdown_data <- subset(data, carrier == input$selected_carrier)
    checkbox_data <- subset(data, carrier %in% input$selected_carriers)
   # DataRange_data <- subset(data, year >= input$date_range[1] & year <= input$date_range[2])
  })
  
  
  
  
 
  
  ##tab1 radio button (daily / Cumulative) code
  output$weather_delay_plot <- renderPlotly({
    if (input$data_type == "Daily") {
      # Daily data
      ggplotly(
        ggplot(data, aes(x = date, y = weather_delay, text = paste("Date: ", date, "<br>Weather Delay: ", weather_delay))) +
          geom_line() +
          labs(title = "Weather Delay Over Time (Daily)", x = "Date", y = "Weather Delay") +
          theme_minimal()
      )
    }
    else {
      # Cumulative data
      cumulative_data <- data %>% 
        group_by(date) %>% 
        summarise(weather_delay = sum(weather_delay))
      
      ggplotly(
        ggplot(cumulative_data, aes(x = date, y = weather_delay, text = paste("Date: ", date, "<br>Cumulative Weather Delay: ", weather_delay))) +
          geom_line() +
          labs(title = "Cumulative Weather Delay Over Time", x = "Date", y = "Cumulative Weather Delay") +
          theme_minimal()
      )
    }
  })
  
  #tab2 visualization with hover chart(1,2,3,4,5,6,7,8,9,10,11)
  output$chart1 <- renderPlotly({
    #if ("chart1" %in% input$show_charts) {
    if ( input$show_chart1) {
      ggplotly(ggplot(data, aes(x = carrier, y = arr_flights)) +
                 geom_line(color = "red") +
                 labs(title = "1.Total Number of Flights Over the different carrier", x = "Carrier", y = "Total Flights"))
    }
  })
  #------------------------------------------------------------------------------------------
  output$chart2 <- renderPlotly({
    if (input$show_chart2) {
      ggplotly(ggplot(data, aes(x = factor(month), y = arr_del15, fill = carrier)) +
                 geom_bar(stat = "identity") +
                 labs(title = "2.Monthly Distribution of Delayed Flights by Carrier", x = "Month", y = "Delayed Flights", fill = "Carrier"))
    }
  })
  #------------------------------------------------------------------------------------------
  output$chart3 <- renderPlotly({
    if ( input$show_chart3) {
      pie_data <- data %>%
        group_by(carrier_name) %>%
        summarise(total_cancellations = sum(arr_cancelled))
      plot_ly(
        data = pie_data,
        labels = ~carrier_name,
        values = ~total_cancellations,
        type = "pie",
        textinfo = "label+percent",
        marker = list(colors = rainbow(nrow(pie_data)))
      ) %>%
        layout(title = "3.Proportion of Cancellations by Carrier")
    }
  })
  #------------------------------------------------------------------------------------------
  output$chart4 <- renderPlotly({
    if (input$show_chart4) {
      ggplotly(ggplot(data, aes(x = factor(year), y = arr_cancelled)) +
                 geom_bar(stat = "identity", fill = "grey") +
                 labs(title = "4.Year Distribution of Cancelled Flights", x = "Year", y = "Cancelled Flights"))
    }
  })
  #------------------------------------------------------------------------------------------
  output$chart5 <- renderPlotly({
    if (input$show_chart5) {
      ggplotly(ggplot(data, aes(x = factor(year), y = arr_diverted)) +
                 geom_bar(stat = "identity", fill = "grey") +
                 labs(title = "5.Year Distribution of Diverted Flights", x = "Year", y = "Diverted Flights"))
    }
  })
  #------------------------------------------------------------------------------------------
  output$chart6 <- renderPlotly({
    if (input$show_chart6) {
      ggplotly(ggplot(data, aes(x = factor(year), y = weather_ct)) +
                 geom_bar(stat = "identity", fill = "grey") +
                 labs(title = "6.Year Distribution of Delayed Flights due to Weather", x = "Year", y = "Delayed Flights due to Weather"))
    }
  })
  #------------------------------------------------------------------------------------------
  output$chart7 <- renderPlotly({
    if ( input$show_chart7) {
      ggplotly(ggplot(data, aes(x = year, y = carrier_name, fill = arr_delay)) +
                 geom_tile() +
                 scale_fill_gradient(low = "white", high = "red") +
                 labs(title = "7.Heatmap of Delay Times Over years and Carriers",
                      x = "Year", y = "Carrier Name", fill = "Total Delay Time (minutes)"))
    }
  })
  #------------------------------------------------------------------------------------------
  output$chart8 <- renderPlotly({
    if (input$show_chart8) {
      ggplotly(ggplot(data, aes(x = carrier, y = arr_cancelled)) +
                 geom_point() +
                 labs(title = "8.Relationship Between Carrier and Cancellations", x = "Carrier", y = "Cancelled Flights"))
    }
  })
  #------------------------------------------------------------------------------------------
  output$chart9 <- renderPlotly({
    if ( input$show_chart9) {
      data_long <- tidyr::gather(data, key = "delay_type", value = "delay_count", carrier_ct:late_aircraft_ct)
      ggplotly(ggplot(data_long, aes(x = factor(year), y = delay_count, fill = delay_type)) +
                 geom_bar(stat = "identity", position = "stack") +
                 labs(title = "9.Delay Types Over Years by Airport", x = "Year", y = "Delay Count", fill = "Delay Type"))
    }
  })
  #------------------------------------------------------------------------------------------
  output$chart10 <- renderPlotly({
    if (input$show_chart10) {
      ggplotly(ggplot(data, aes(x = arr_cancelled)) +
                 geom_histogram(binwidth = 15, fill = "skyblue", color = "black", aes(y = ..density..)) +
                 labs(title = "10.Histogram of Arrival Delays", x = "Arrival Delay (minutes)", y = "Density"))
    }
  })
  #------------------------------------------------------------------------------------------
  output$chart11 <- renderPlotly({
    if (input$show_chart11) {
      bubble_data <- data %>%
        group_by(carrier) %>%
        summarise(total_diverted = sum(arr_diverted),
                  total_cancellations = sum(arr_cancelled))
      
      # Create a bubble chart with two different attributes
      ggplotly(ggplot(bubble_data, aes(x = total_diverted, y = total_cancellations, color = carrier)) +
                 geom_point(alpha = 0.7) +
                 scale_size(range = c(5, 20)) +
                 labs(title = "11.Diverted Flights vs. Cancellations by Carrier",
                      x = "Total Diverted Flights", y = "Total Cancellations",
                      color = "Carrier"))
    }
  })
  #------------------------------------------------------------------------------------------
  
  #Dynamic filters (arr_del15 , arr_flights )
  filtered_df <- reactive({
    # Filter data based on user input
    #vizualitation of it
    output$scatter-plot <- renderPlot({
      ggplot(filtered_df(), aes(x = month, y = arr_del15, color = carrier)) +
        geom_line() +
        labs(title = "Arrival Delays",
             x = "Month",
             y = "Number of Flights More Than 15 Minutes Late")
    })
    
    #conditions based on filters
    carriers <- input$carrier1
    if (is.null(carriers)) carriers <- unique(data$carrier1)
    
    filtered_data <- data %>%
      
      filter(carrier1 %in% carriers)
    
    # Group data based on selected data type
    if (input$data-type == "Daily") {
      grouped_data <- radio %>%
        group_by(year, month, carrier) %>%
        summarize(arr_del15 = sum(arr_del15))
    } 
    else {
      grouped_data <- radio %>%
        group_by(year, month, carrier) %>%
        summarize(arr_del15 = cumsum(arr_del15))
    }
    return(grouped_data)
  })
  
  # Reactive function to filter data based on user selections
  filtered_data <- reactive({
    # Filter data based on user-selected filters
    choosen_data <- data
    
    # Filter based on carrier
    #absolute / percentage
    if (!is.null(input$carrier1)) {
      choosen_data <- filter(choosen_data, carrier == input$carrier1)
    }
    
    # Filter based on month
    if (!is.null(input$month)) {
      choosen_data <- filter(choosen_data, month == input$month)
    }
    
    if (input$unit == "Percentage Change") {
      # Calculate percentage change if needed
      choosen_data <- mutate(choosen_data, arr_del15 = (arr_del15 / arr_flights) * 100)
    }
    return(choosen_data)
  })
  
  # Reactive function to generate scatter plot based on filtered data
  output$scatterPlot <- renderPlot({
    # Create the scatter plot based on filtered_data()
    ggplot(filtered_data(), aes(x = arr_flights, y = arr_del15, color = arr_delay)) +
      geom_point() +
      labs(title = "Relationship between Flights and Delays",
           x = "Number of Flights",
           y = "Number of Flights Delayed > 15 mins",
           color = "Total Delayed Time (minutes)") +
      theme_minimal()
  })
  
  # Reactive function to generate scatter plot based on filtered data
  output$scatterPlot1 <- renderPlot({
    # Create the scatter plot based on filtered_data() and radio button choice
    ggplot(data, aes(x = if (input$data_type == "Daily") arr_flights else month,
                     y = if (input$data_type == "Daily") arr_del15 else arr_delay,
                     color = if (input$data_type == "Daily") month else arr_delay)) +
      geom_point() +
      labs(title = "Relationship between Flights and Delays",
           x = if (input$data_type == "Daily") "Number of Flights" else "Month",
           y = if (input$data_type == "Daily") "Number of Flights Delayed > 15 mins" else "Total Delayed Time (minutes)",
           color = if (input$data_type == "Daily") "Month" else "Total Delayed Time (minutes)") +
      theme_minimal()
  })

}

shinyApp(ui, server)