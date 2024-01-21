library(ggplot2)
library(shiny)
library(shinythemes)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(DT)
library(mosaicData)
library(treemapify)
library(forecast)
library(leaflet)
library(viridis)

df = read.csv("turo_cars.csv")
ui <- navbarPage(
  title = "      Dashboard for Turo-Go      ",
  theme = shinytheme("darkly"),
  tabPanel(" Top Car Makes by State ",
           fluidPage(
             titlePanel("Choose the top states with the\n corresponding top 3 car makes"),
             sidebarLayout(sidebarPanel(
               selectInput(
                 inputId = "index",
                 label = "Choose the top states",
                 choices = c(5:10)
               ),
               sliderInput(
                 inputId = "year_top",
                 label = "Choose the production year range of the cars",
                 min = 2000,
                 max = max(df$car.year),
                 value = c(2000, max(df$car.year)),
                 step = 1,
                 sep = "",
                 round = TRUE
               ),
               width = 3
             ),
             mainPanel(plotOutput("plot2", width = "1100px", height = "550px")))
           )),
  
  tabPanel(" Build Scatterplot ",
           fluidPage(
             titlePanel("Analysis of Turo-Go Features: Scatterplot Overview"),
             sidebarLayout(sidebarPanel(
               selectInput(
                 inputId = "label1",
                 label = "Choose the feature for x axis",
                 choices = c("Host.tenure.in.weeks", "Car.extra.mile.fee", "Number.of.cars.host.owns", "Number.of.extra.features", "Displayed.user.review.number.in.past.12.months")
               ),
               selectInput(
                 inputId = "label2",
                 label = "Choose the feature for y axis",
                 choices = c("Car.trip.price.3.days", "Car.extra.mile.fee", "Number.of.cars.host.owns")
               ),
               sliderInput(
                 inputId = "year3",
                 label = "Choose the production year range of the cars",
                 min = 2000,
                 max = max(df$car.year),
                 value = c(2000, max(df$car.year)),
                 step = 1,
                 sep = "",
                 round = TRUE
               ),
               checkboxGroupInput(
                 inputId = "label34",
                 label = "Choose states",
                 choices = c(unique(df$Car_state)),
                 selected = c(unique(df$Car_state)),
                 inline = TRUE
               ),
               width = 3
             ),
             mainPanel(plotOutput("plot3", height = "550px", width = "1100px")))
           )),
  tabPanel(" Build Barplot ",
           fluidPage(
             titlePanel("Analysis of Turo-Go Features: Barplot Overview"),
             sidebarLayout(sidebarPanel(
               selectInput(
                 inputId = "label3",
                 label = "Choose the categorical variable",
                 choices = c("Car.power.type", "Car.photo.number", "Car.rental.type", "Car.transmission.type", "Number.of.doors", "Number.of.cars.host.owns", "Car_state"),
                 selected = c("Car_state")
               ),
               sliderInput(
                 inputId = "year2",
                 label = "Choose the production year range of the cars",
                 min = 2000,
                 max = max(df$car.year),
                 value = c(2000, max(df$car.year)),
                 step = 1,
                 sep = "",
                 round = TRUE
               ),
               selectInput(
                 inputId = "index_top",
                 label = "Choose the top bars",
                 choices = c(2:10)
               ),
               width = 3
             ),
             mainPanel(plotOutput("plot4", height = "550px", width = "1100px"))
             ))),
  
  tabPanel(" Build Historgram ",
           fluidPage(
             titlePanel("Analysis of Turo-Go Features: Frequency Histogram Overview"),
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput("bins", "Number of bins:", min = 5, max = 50, value = 30),
                 selectInput("x_var", "Select variable for x-axis:", choices = c("Car.trip.price.3.days", "Host.tenure.in.weeks", "Car.extra.mile.fee")),
                 width = 3
               ),
               
               mainPanel(
                 plotOutput("histogram", height = "555px", width = "1100px")
               )
             )
           )),
  
  tabPanel(" Build Boxplot ",
           fluidPage(
             titlePanel("Analysis of Turo-Go Features: Boxplot Overview"),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_var_2", "Select categorical variable for x-axis:", 
                             choices = c("Host.location.available", "Host.verified.email", "Host.verified.fb", "Host.verified.phone", "Car.power.type", "Car.photo.number", "Car.rental.type", "Car.transmission.type", "Number.of.doors", "Number.of.cars.host.owns", "Car_state")),
                 selectInput("y_var_2", "Select numeric variable for y-axis:", 
                             choices = c("Car.trip.price.3.days", "Host.tenure.in.weeks", "Car.extra.mile.fee")),
                 checkboxInput("outliers", "Display Outliers", value = TRUE),
                 width = 2
               ),
               
               mainPanel(
                 plotOutput("boxplot", height = "550px", width = "1235px")
               )
             )
           )),
  
  tabPanel("Map: Number of car offers per state",
           mainPanel(leafletOutput(
             "map", width = 1500, height = 1000
           ))
  ),
  
  tabPanel(" Turo-Go app ",
           fluidRow(
             titlePanel("Turo-Go app usage"),
             
             sidebarLayout(sidebarPanel(
               sliderInput(
                 inputId = "year",
                 label = "Choose the production year range of the cars",
                 min = 2000,
                 max = max(df$car.year),
                 value = c(2000, max(df$car.year)),
                 step = 1,
                 sep = "",
                 round = TRUE
               ),
               checkboxGroupInput(
                 inputId = "turo_go",
                 label = "The car is linked with the Turo-Go app",
                 choices = unique(df$car.turo.go),
                 selected = unique(df$car.turo.go)
               ),
               width = 3
             ),
             mainPanel(plotOutput("plot1", height = "550px", width = "1100px")))
           ))
)

                           


server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    tmp <- df[df$car.year >= input$year[1] & df$car.year <= input$year[2] & df$car.turo.go %in% input$turo_go,]
    
    plot1 <- ggplot(data = tmp, aes(x=tmp$car.year))  +
      geom_histogram(color= 'black', binwidth = 1, 
                     aes(x = tmp$car.year, y = ..count.. , fill= tmp$car.turo.go), position = "stack") +
      xlim(2000, 2025) +
      theme_minimal()
    
    if (length(input$turo_go) == 1 && input$turo_go == "TRUE") {
      plot1 <- plot1 + scale_fill_manual(values = c("#00bfc4"))
    }
    
    grid.arrange(
      plot1,
      nrow = 1,
      top = paste("Year Range:", input$year[1], "-", input$year[2]),
      bottom = reactive(input$indicator)
    )
    
  })
  
  output$plot2 <- renderPlot({
    
    df_freq = df %>% count(Car_state, sort = TRUE)
    
    # Reorder car.state by frequency
    df_freq$Car_state = factor(df_freq$Car_state, levels = df_freq$Car_state)
    top_car_states = df_freq[1:input$index,]
    
    # Filter data for the top car states
    New = filter(df, Car_state %in% top_car_states$Car_state & car.year >= input$year_top[1] & car.year <= input$year_top[2])
    
    # Calculate frequency of car.makes within each state
    New1 = New %>% 
      group_by(Car_state, car.make) %>% 
      summarise(count = n(), .groups = "drop") %>% 
      ungroup() 
    
    # Rank car makes within each state by frequency count
    New1 = New1 %>% 
      group_by(Car_state) %>% 
      mutate(rank = dense_rank(desc(count))) %>% 
      filter(rank <= 3) %>% 
      ungroup() %>% 
      mutate(rank = as.numeric(rank))
    
    # Sort by car.state and rank
    New2 = New1[order(New1$Car_state, New1$rank), ]
    as.data.frame(New2)
    

    New2 <- New2 %>%
      group_by(Car_state) %>%
      arrange(Car_state, desc(count), car.make) %>%
      slice(1:3) %>%
      ungroup()
    

    New2$Car_state <- factor(New2$Car_state, levels = top_car_states$Car_state)
    
    plot2 <- ggplot(New2, aes(x = New2$Car_state, y = count, fill = car.make, order = rank, group = car.make)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, direction = -1) +
      labs(title = "Top Three Car Makes by State", x = "State", y = "Frequency") +
      theme_minimal()
    
    grid.arrange(
      plot2,
      nrow = 1,
      top = paste("Top", input$index, "Car_state")
    )
    
  })
  
  
  car_data_geocoded <- read.csv("Turo_with_locations.csv")  
  car_data_geocoded <- na.omit(car_data_geocoded)
  
  state_data <- car_data_geocoded %>%
    group_by(Car_state) %>%
    summarise(n_offers = n(), lat = mean(lat), lon = mean(lon))
  
  city_data <- car_data_geocoded %>%
    group_by(Car_state, car.city) %>%
    summarise(n_offers = n(), lat = mean(lat), lon = mean(lon))
  

  turo_map <- leaflet() %>%
    addTiles()
  

  turo_map <- addCircleMarkers(turo_map,
                               data = state_data,
                               lng = ~lon,
                               lat = ~lat,
                               label = ~paste("State:", Car_state, "",
                                              "Offers:", n_offers),
                               labelOptions = labelOptions(noHide = TRUE),
                               radius = 5,
                               color = "red",
                               stroke = FALSE,
                               fillOpacity = 0.8,
                               group = "State Layer Group"
  )
  
  output$map <- renderLeaflet({
    turo_map
  })
  
  observeEvent(input$map_marker_click, {
    clicked_id <- input$map_marker_click$id
    selected_state <- state_data$Car_state[clicked_id]
    
    if (!is.null(selected_state)) {
      proxy <- leafletProxy("map")
      proxy %>% hideGroup("State Layer Group")
      proxy %>% showGroup(selected_state)
    }
  })
  
  observeEvent(input$map_click, {
    proxy <- leafletProxy("map")
    proxy %>% showGroup("State Layer Group")
  })
  
  output$plot3 <- renderPlot({
    
    # Filter the data by the given column and selected states
    df1 <- df %>%
      filter(input$label1 != 0 & car.year >= input$year3[1] & car.year <= input$year3[2] & Car_state %in% input$label34)
    
    average_trip_prices <- df1 %>%
      group_by(!!sym(input$label1)) %>%
      summarise(average_trip_price = if (input$label2 == "car.trip.price") {
        mean(!!sym(input$label2), na.rm = TRUE)
      } else {
        (!!sym(input$label2))
      })
    
    print(average_trip_prices)
    
    r <- cor(average_trip_prices[[input$label1]], average_trip_prices$average_trip_price, use = "pairwise.complete.obs")
    
    title_text <- paste("Scatterplot of ", input$label1, "vs", input$label2, "for selected states")
    plot3 <- ggplot(average_trip_prices, aes_string(x = input$label1, y = "average_trip_price")) +
      geom_point() +
      labs(x = input$label1, y = input$label2, title = title_text) +
      geom_smooth(method = "lm") +
      annotate("text", label = paste0("r = ", round(r, 2)), hjust = 1.2, vjust = 1.2, size = 5, color = 'red') +
      theme_minimal() + 
      coord_cartesian(xlim = c(0, max(average_trip_prices[[input$label1]], na.rm = TRUE)), 
                      ylim = c(0, max(average_trip_prices$average_trip_price, na.rm = TRUE)))+
      theme(axis.title.x = element_text(size = 19),
            axis.title.y = element_text(size = 19),
            plot.title = element_text(size = 22, face = "bold"))
    
    grid.arrange(
      plot3,
      nrow = 1, 
      bottom = paste("r = ", round(r, 2))
    )
    
  })
  
  
  filtered_data <- reactive({
    req(input$label3)
    df_filtered <- df %>% filter(car.year >= input$year2[1] & car.year <= input$year2[2])
    df_freq <- df_filtered %>% count(!!sym(input$label3), sort = TRUE)
    df_freq[1:input$index_top,]
  })
  
  output$plot4 <- renderPlot({
    req(input$label3, input$index_top)
    
    df_freq <- filtered_data() %>% 
      filter(!is.na(!!sym(input$label3))) %>%  
      mutate(!!input$label3 := factor(!!sym(input$label3), levels = !!sym(input$label3)))
    
    plot4 <- ggplot(df_freq, aes_string(x = input$label3, y = "n")) +
      geom_bar(stat = "identity") +
      labs(x = input$label3, y = "Frequency", 
           title = paste("Barplot:", input$label3, "by Frequency in the year range", input$year2[1], "-", input$year2[2])) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_text(size = 19),     
            axis.title.y = element_text(size = 19),          
            plot.title = element_text(size = 21, face = "bold"))
    
    grid.arrange(
      plot4,
      nrow = 1,
      top = paste("Year Range:", input$year2[1], "-", input$year2[2])
    )
  })
  
  num_choices <- reactive({
    df_filtered <- df %>% filter(car.year >= input$year2[1] & car.year <= input$year2[2])
    df_freq <- df_filtered %>% count(!!sym(input$label3), sort = TRUE)
    nrow(df_freq) - 1
  })
  
  observeEvent(list(input$year2, input$label3), {
    current_selection <- input$index_top
    available_choices <- 2:num_choices()
    
    if (current_selection > max(available_choices)) {
      selected_choice <- max(available_choices)
    } else {
      selected_choice <- current_selection
    }
    
    # Check if the current selection is not in the new available choices
    if (!current_selection %in% available_choices) {
      selected_choice <- max(available_choices)
    }
    
    updateSelectInput(session, "index_top", choices = available_choices, selected = selected_choice)
  })
  
  
  output$histogram <- renderPlot({
    # Assuming your data is in a dataframe named 'df'
    x_var <- input$x_var
    bins <- input$bins
    
    plot5 <- ggplot(df, aes_string(x = x_var)) +
      geom_histogram(bins = bins, color = "black", alpha = 0.7) +
      theme_minimal() +
      labs(x = x_var, y = "Frequency", title = paste("Histogram of", x_var))
    
    grid.arrange(
      plot5,
      nrow = 1,
      top = bins
    )
    
  })
  
  output$boxplot <- renderPlot({
    
    df_tt <- df
    
    # Convert categorical variables to factors
    df_tt$Car.power.type <- as.factor(df_tt$Car.power.type)
    df_tt$Car.photo.number <- as.factor(df_tt$Car.photo.number)
    df_tt$Car.rental.type <- as.factor(df_tt$Car.rental.type)
    df_tt$Car.transmission.type <- as.factor(df_tt$Car.transmission.type)
    df_tt$Number.of.doors <- as.factor(df_tt$Number.of.doors)
    df_tt$Number.of.cars.host.owns <- as.factor(df_tt$Number.of.cars.host.owns)
    df_tt$Car_state <- as.factor(df_tt$Car_state)
    df_tt$Host.verified.email <- as.factor(df_tt$Host.verified.email)
    df_tt$Host.verified.fb <- as.factor(df_tt$Host.verified.fb)
    df_tt$Host.verified.phone <- as.factor(df_tt$Host.verified.phone)
    
    
    df_tt <- na.omit(df_tt)
    df_tt <- na.omit(df_tt)
    outliers <- input$outliers
    
    # Calculate the medians for each group
    medians <- df_tt %>% 
      group_by(!!sym(input$x_var_2)) %>% 
      summarize(median = median(!!sym(input$y_var_2), na.rm = TRUE)) %>% 
      arrange(median)
    
    # Reorder the factor levels of the x-axis categorical variable based on the medians
    df_tt <- df_tt %>% 
      mutate(!!input$x_var_2 := factor(!!sym(input$x_var_2), levels = medians[[input$x_var_2]]))
    
    plot6 <- ggplot(df_tt, aes(x = !!sym(input$x_var_2), y = !!sym(input$y_var_2))) +
      theme_minimal() +
      labs(x = input$x_var_2, y = input$y_var_2, title = paste("Boxplot of", input$y_var_2, "by", input$x_var_2))
    
    if (outliers) {
      plot6 <- plot6 + geom_boxplot(fill = "gray", color = "black", alpha = 0.7)
    } else {
      plot6 <- plot6 + geom_boxplot(fill = "gray", color = "black", alpha = 0.7, outlier.shape = NA)
    }
    
    if (!input$outliers) {
      plot6 <- plot6 + coord_cartesian(ylim = c(NA, max(ggplot_build(plot6)$data[[1]]$ymax)))
    }
    
    grid.arrange(
      plot6,
      nrow = 1,
      top = input$outliers
    )
    
  })
  
}


shinyApp(ui = ui, server = server) # options = list(launch.browser = T))
