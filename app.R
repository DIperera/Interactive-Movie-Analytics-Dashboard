# ============================================
# Interactive IMDb Movie Analyzer - Shiny App
# ============================================

library(shiny)
library(tidyverse)
library(plotly)  # For interactive plots

# Load and prepare data
movies <- read_csv("movies.csv")

movies_clean <- movies %>%
  select(movie_title, genres, imdb_score, gross, budget, year, director_name) %>%
  drop_na() %>%
  separate_rows(genres, sep = "\\|")

# Get unique genres for filter
all_genres <- sort(unique(movies_clean$genres))

# ============================================
# UI (User Interface)
# ============================================
ui <- fluidPage(
  # Custom CSS styling
  tags$head(
    tags$style(HTML("
      .title-panel {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 30px;
        border-radius: 10px;
        margin-bottom: 20px;
      }
      .stats-box {
        background: #f8f9fa;
        padding: 20px;
        border-radius: 10px;
        text-align: center;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .stats-value {
        font-size: 32px;
        font-weight: bold;
        color: #667eea;
      }
      .stats-label {
        font-size: 14px;
        color: #6c757d;
        margin-top: 5px;
      }
    "))
  ),
  
  # Title
  div(class = "title-panel",
    h1("🎬 IMDb Movie Analyzer", style = "margin: 0;"),
    p("Interactive analysis of movie ratings, revenue, and genres", 
      style = "margin: 10px 0 0 0; font-size: 16px;")
  ),
  
  # Sidebar for filters
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("🔍 Filters"),
      
      # Genre selection
      selectInput(
        "selected_genres",
        "Select Genres:",
        choices = c("All Genres", all_genres),
        selected = "All Genres",
        multiple = TRUE
      ),
      
      # Year range slider
      sliderInput(
        "year_range",
        "Year Range:",
        min = min(movies$year),
        max = max(movies$year),
        value = c(min(movies$year), max(movies$year)),
        step = 1,
        sep = ""
      ),
      
      # Rating filter
      sliderInput(
        "rating_filter",
        "Minimum IMDb Rating:",
        min = 0,
        max = 10,
        value = 0,
        step = 0.1
      ),
      
      # Minimum movie count for genre analysis
      sliderInput(
        "min_count",
        "Min Movies per Genre:",
        min = 1,
        max = 50,
        value = 10,
        step = 1
      ),
      
      hr(),
      
      # Action button
      actionButton("reset_btn", "Reset Filters", 
                   style = "width: 100%; background-color: #667eea; color: white;")
    ),
    
    # Main panel with tabs
    mainPanel(
      width = 9,
      
      # Summary statistics boxes
      fluidRow(
        column(3,
          div(class = "stats-box",
            div(class = "stats-value", textOutput("total_movies")),
            div(class = "stats-label", "Total Movies")
          )
        ),
        column(3,
          div(class = "stats-box",
            div(class = "stats-value", textOutput("avg_rating")),
            div(class = "stats-label", "Avg Rating")
          )
        ),
        column(3,
          div(class = "stats-box",
            div(class = "stats-value", textOutput("avg_revenue")),
            div(class = "stats-label", "Avg Revenue")
          )
        ),
        column(3,
          div(class = "stats-box",
            div(class = "stats-value", textOutput("total_genres")),
            div(class = "stats-label", "Active Genres")
          )
        )
      ),
      
      br(),
      
      # Tabset with different visualizations
      tabsetPanel(
        type = "tabs",
        
        # Tab 1: Genre Analysis
        tabPanel("📊 Genre Analysis",
          br(),
          plotlyOutput("genre_scatter", height = "500px"),
          br(),
          plotlyOutput("genre_bar", height = "400px")
        ),
        
        # Tab 2: Movie Explorer
        tabPanel("🎥 Movie Explorer",
          br(),
          plotlyOutput("budget_scatter", height = "500px"),
          br(),
          h4("Top Movies by Revenue"),
          DT::dataTableOutput("top_movies_table")
        ),
        
        # Tab 3: Trends
        tabPanel("📈 Trends",
          br(),
          plotlyOutput("yearly_trends", height = "500px"),
          br(),
          plotlyOutput("rating_distribution", height = "400px")
        ),
        
        # Tab 4: Data Table
        tabPanel("📋 Data Table",
          br(),
          DT::dataTableOutput("data_table")
        )
      )
    )
  )
)

# ============================================
# SERVER (Logic)
# ============================================
server <- function(input, output, session) {
  
  # Reactive: Filter movies based on user inputs
  filtered_movies <- reactive({
    data <- movies_clean
    
    # Filter by genres
    if (!"All Genres" %in% input$selected_genres && length(input$selected_genres) > 0) {
      data <- data %>% filter(genres %in% input$selected_genres)
    }
    
    # Filter by year
    data <- data %>% filter(year >= input$year_range[1] & year <= input$year_range[2])
    
    # Filter by rating
    data <- data %>% filter(imdb_score >= input$rating_filter)
    
    return(data)
  })
  
  # Reactive: Genre analysis
  genre_summary <- reactive({
    filtered_movies() %>%
      group_by(genres) %>%
      summarise(
        avg_revenue = mean(gross, na.rm = TRUE) / 1e6,
        avg_rating = mean(imdb_score, na.rm = TRUE),
        count = n(),
        total_revenue = sum(gross, na.rm = TRUE) / 1e6,
        .groups = "drop"
      ) %>%
      filter(count >= input$min_count) %>%
      arrange(desc(avg_revenue))
  })
  
  # Summary Statistics
  output$total_movies <- renderText({
    format(nrow(filtered_movies()), big.mark = ",")
  })
  
  output$avg_rating <- renderText({
    round(mean(filtered_movies()$imdb_score, na.rm = TRUE), 2)
  })
  
  output$avg_revenue <- renderText({
    paste0("$", round(mean(filtered_movies()$gross, na.rm = TRUE) / 1e6, 1), "M")
  })
  
  output$total_genres <- renderText({
    length(unique(genre_summary()$genres))
  })
  
  # Plot 1: Genre Scatter (Rating vs Revenue)
  output$genre_scatter <- renderPlotly({
    p <- ggplot(genre_summary(), aes(x = avg_rating, y = avg_revenue, 
                                     size = count, color = genres,
                                     text = paste0(
                                       "<b>", genres, "</b><br>",
                                       "Rating: ", round(avg_rating, 2), "<br>",
                                       "Revenue: $", round(avg_revenue, 1), "M<br>",
                                       "Movies: ", count
                                     ))) +
      geom_point(alpha = 0.7) +
      labs(
        title = "Genre Sweet Spot: Rating vs Revenue",
        x = "Average IMDb Rating",
        y = "Average Revenue (Millions $)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Plot 2: Genre Bar Chart
  output$genre_bar <- renderPlotly({
    top_genres <- genre_summary() %>%
      top_n(10, avg_revenue)
    
    p <- ggplot(top_genres, aes(x = reorder(genres, avg_revenue), y = avg_revenue, 
                                fill = genres,
                                text = paste0("$", round(avg_revenue, 1), "M"))) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(
        title = "Top 10 Highest Grossing Genres",
        x = "",
        y = "Average Revenue (Millions $)"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Plot 3: Budget vs Gross Scatter
  output$budget_scatter <- renderPlotly({
    data <- filtered_movies() %>%
      distinct(movie_title, .keep_all = TRUE)
    
    p <- ggplot(data, aes(x = budget/1e6, y = gross/1e6, 
                         color = imdb_score,
                         text = paste0(
                           "<b>", movie_title, "</b><br>",
                           "Rating: ", imdb_score, "<br>",
                           "Budget: $", round(budget/1e6, 1), "M<br>",
                           "Gross: $", round(gross/1e6, 1), "M"
                         ))) +
      geom_point(alpha = 0.6, size = 3) +
      scale_color_gradient(low = "orange", high = "darkgreen") +
      labs(
        title = "Budget vs Gross Revenue",
        x = "Budget (Millions $)",
        y = "Gross Revenue (Millions $)",
        color = "IMDb Score"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Plot 4: Yearly Trends
  output$yearly_trends <- renderPlotly({
    yearly_data <- filtered_movies() %>%
      group_by(year) %>%
      summarise(
        avg_revenue = mean(gross, na.rm = TRUE) / 1e6,
        avg_rating = mean(imdb_score, na.rm = TRUE),
        movie_count = n(),
        .groups = "drop"
      )
    
    p <- ggplot(yearly_data, aes(x = year)) +
      geom_line(aes(y = avg_revenue, color = "Revenue"), size = 1.2) +
      geom_point(aes(y = avg_revenue, color = "Revenue"), size = 2) +
      geom_line(aes(y = avg_rating * 30, color = "Rating"), size = 1.2) +
      geom_point(aes(y = avg_rating * 30, color = "Rating"), size = 2) +
      scale_y_continuous(
        name = "Average Revenue (Millions $)",
        sec.axis = sec_axis(~./30, name = "Average IMDb Rating")
      ) +
      scale_color_manual(values = c("Revenue" = "#E74C3C", "Rating" = "#3498DB")) +
      labs(
        title = "Movie Trends Over Time",
        x = "Year",
        color = "Metric"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot 5: Rating Distribution
  output$rating_distribution <- renderPlotly({
    p <- ggplot(filtered_movies(), aes(x = imdb_score)) +
      geom_histogram(bins = 30, fill = "#667eea", alpha = 0.7, color = "white") +
      labs(
        title = "IMDb Rating Distribution",
        x = "IMDb Score",
        y = "Number of Movies"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Table 1: Top Movies
  output$top_movies_table <- DT::renderDataTable({
    filtered_movies() %>%
      distinct(movie_title, .keep_all = TRUE) %>%
      arrange(desc(gross)) %>%
      select(movie_title, imdb_score, gross, budget, year, director_name) %>%
      head(20) %>%
      mutate(
        gross = paste0("$", format(round(gross/1e6, 1), big.mark = ","), "M"),
        budget = paste0("$", format(round(budget/1e6, 1), big.mark = ","), "M")
      ) %>%
      rename(
        "Movie" = movie_title,
        "Rating" = imdb_score,
        "Gross" = gross,
        "Budget" = budget,
        "Year" = year,
        "Director" = director_name
      )
  }, options = list(pageLength = 10))
  
  # Table 2: All Data
  output$data_table <- DT::renderDataTable({
    filtered_movies() %>%
      select(movie_title, genres, imdb_score, gross, budget, year, director_name) %>%
      mutate(
        gross = paste0("$", format(round(gross/1e6, 1), big.mark = ","), "M"),
        budget = paste0("$", format(round(budget/1e6, 1), big.mark = ","), "M")
      ) %>%
      rename(
        "Movie" = movie_title,
        "Genre" = genres,
        "Rating" = imdb_score,
        "Gross" = gross,
        "Budget" = budget,
        "Year" = year,
        "Director" = director_name
      )
  }, options = list(pageLength = 25))
  
  # Reset button
  observeEvent(input$reset_btn, {
    updateSelectInput(session, "selected_genres", selected = "All Genres")
    updateSliderInput(session, "year_range", value = c(min(movies$year), max(movies$year)))
    updateSliderInput(session, "rating_filter", value = 0)
    updateSliderInput(session, "min_count", value = 10)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
