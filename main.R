library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(ggplot2)
library(ggvis)
library(ggrepel)
library(recipes)

# data preparation
anime_raw <- read_csv("https://uwmadison.box.com/shared/static/hnvznr8i22jnzi8x70lsibkw6umkboaw.csv") |>
  separate_rows(Genres, sep = ",\\s*") |>
  mutate(
    Demographic = substr(Demographic, 0, str_length(Demographic) / 2),
    release_year = as.numeric(str_extract(Aired, "\\b\\d{4}\\b"))
  ) |>
  group_by(across(-Genres)) |>
  summarize(Genres = paste(unique(Genres), collapse = ", "), .groups = "drop") 

anime_tidy <- anime_raw |>
  separate_rows(Genres, sep = ",\\s*")

highest_rated_per_year <- anime_raw |>
  group_by(release_year) |>
  filter(Score == max(Score, na.rm = TRUE)) |>
  ungroup()

# helper function to obtain selection
display_selection <- function(field) {
  field <- field |>
    na.omit() |>
    unique() |>
    sort()
  field <- field[field != "NA"]
  field
}

# helper function to perform PCA
pca <- function(data, include_production_company = T) {
  # preprocess data
  data <- data |>
    separate_rows(Genres, sep = ",\\s*") |>
    drop_na() |>
    filter(Genres != "NA")
  
  # if include production company option is selected
  if (include_production_company) {
    data <- data |>
      select(English, Genres, Score, Popularity, Studios) |>
      separate_rows(Studios, sep = ",\\s*") |>
      pivot_wider(
        names_from = Studios,
        values_from = Studios,
        values_fn = length,
        values_fill = 0
      )
  } else {
    data <- data |>
      select(English, Genres, Score, Popularity)
  }
  
  # prepare PCA recipe
  pca_prep <- recipe(~., data = data) |>
    update_role(English, Genres, new_role = "id") |>
    step_normalize(all_predictors()) |>
    step_pca(all_predictors()) |>
    prep()
  
  # calculate PCA result
  pca_result <- juice(pca_prep) |>
    select(starts_with("PC"))
  
  # get metadata
  metadata <- data |>
    select(English, Genres)
  
  # combine result and metadata
  final_result <- cbind(metadata, pca_result) |>
    distinct(English, .keep_all = TRUE)
  
  list(
    pca_data = final_result, 
    pca_prep = pca_prep 
  )
}

# helper function to format PCA plot
format_pca_plot <- function(plot) {
  plot +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
}

# helper function to plot PCA scatter plot
plot_pca_scatter <- function(pca_result, pc_x = "PC1", pc_y = "PC2") {
  plot <- ggplot(pca_result, aes_string(pc_x, pc_y, label = "English")) +
    geom_point(aes(color = Genres), alpha = 0.7, size = 2) +
    geom_text_repel(size = 3) +
    labs(
      x = paste("Principal Component", gsub("PC", "", pc_x)),
      y = paste("Principal Component", gsub("PC", "", pc_y)),
      color = "Genres"
    )
  format_pca_plot(plot)
}

# helper function to plot PCA component contributions
plot_pca_components <- function(pca_prep, n_components = 2) {
  components <- tidy(pca_prep, 2) |>
    filter(component %in% str_c("PC", 1:n_components)) |>
    mutate(terms = reorder(terms, abs(value)))
  
  plot <- ggplot(components, aes(value, terms)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~component, nrow = 1)
  
  format_pca_plot(plot)
}

# helper function to plot PCA variances
plot_pca_variances <- function(pca_prep) {
  variances <- tidy(pca_prep, 2, type = "variance") |>
    filter(terms == "percent variance")
  
  plot <- ggplot(variances) +
    geom_col(aes(component, value)) +
    labs(x = "Component", y = "Values")
  
  format_pca_plot(plot)
}

# ui
ui <- fluidPage(
  titlePanel("Anime Data Explorer"),
  page_fillable(
    full_screen = TRUE,
    navset_card_tab(
      
      # Anime Scores Over Years Tab
      nav_panel(
        "Anime Scores Over Years",
        
        # Multiple selection filters for genres and ratings
        fluidRow(
          column(
            width = 3,
            selectizeInput(
              "genres",
              "Select Genres:",
              choices = display_selection(anime_tidy$Genres),
              multiple = TRUE, 
              options = list(placeholder = "Select one or more genres")
            )
          ),
          column(
            width = 4,
            selectizeInput(
              "rating",
              "Select Rating:",
              choices = display_selection(anime_tidy$Rating),
              multiple = TRUE, 
              options = list(placeholder = "Select one or more ratings")
            )
          )
        ),
        
        # Scatter plot and anime description
        fluidRow(
          column(
            width = 12,
            helpText("Select a point on the plot to view more details about the anime."),
            plotOutput("plot", click = "plot_click"),
            div(
              style = "margin-top: 20px;",
              uiOutput("anime_description")
            )
          )
        )
      ),
      
      # Top Anime Studios Over Years Tab
      nav_panel(
        "Top Anime Studios Over Years",
        plotOutput("yearly_highest_rated"),
        DTOutput("ranking")
      ),
      
      # PCA tab
      nav_panel(
        "Principal Component Analysis",
        h3("PCA of Anime Dataset"),
        # Add controls for selecting PCs
        fluidRow(
          column(
            width = 2.5,
            checkboxInput(
              "include_production_company",
              "Include Production Company in PCA",
              value = TRUE
            )
          )
        ),
        plotOutput("pca_scatter"), # Scatter plot for PCA scores
        h3("PCA Component Contributions"),
        plotOutput("pca_components"), # PCA component contributions plot
        h3("PCA Variance Explained"),
        plotOutput("pca_variance") # PCA variance
      ),
    )
  )
)

# server
server <- function(input, output, session) {
  
  # Reactive expressions 
  # Filter options
  filtered_data <- reactive({
    data <- anime_raw
    
    # Filter on genres 
    if (!is.null(input$genres) && length(input$genres) > 0) {
      data <- data |>
        filter(str_detect(Genres, paste(input$genres, collapse = "|")))
    }
    
    # Filter on ratings 
    if (!is.null(input$rating) && length(input$rating) > 0) {
      data <- data |>
        filter(Rating %in% input$rating) 
    }
    
    data
  })
  
  # Click events
  selected_anime <- reactive({
    click <- input$plot_click
    if (!is.null(click)) {
      nearest <- nearPoints(
        filtered_data(),
        click,
        xvar = "release_year",
        yvar = "Score",
        maxpoints = 1
      )
      return(nearest)
    }
    return(NULL)
  })
  
  # Filter for including production company 
  anime_pca <- reactive({
    pca(anime_raw, include_production_company = input$include_production_company)
  })
  
  # anime scores over years scattter plot
  output$plot <- renderPlot({
    ribbon_data <- filtered_data() |>
      group_by(release_year) |>
      summarize(
        lower = quantile(Score, 0.25, na.rm = TRUE),
        upper = quantile(Score, 0.75, na.rm = TRUE)
      )
    
    ggplot(filtered_data()) +
      geom_ribbon(
        data = ribbon_data,
        aes(x = release_year, ymin = lower, ymax = upper),
        fill = "gray80", alpha = 0.7
      ) +
      geom_jitter(aes(x = release_year, y = Score, color = Status)) +
      geom_smooth(aes(x = release_year, y = Score), se = F) +
      scale_color_manual(
        values = c("Currently Airing" = "#FC8D62", "Finished Airing" = "#66C2B6"),
        labels = c("Currently Airing" = "Ongoing", "Finished Airing" = "Finished"),
        name = "Airing Status"
      ) +
      scale_x_continuous(
        breaks = seq(min(filtered_data()$release_year, na.rm = TRUE),
                     max(filtered_data()$release_year, na.rm = TRUE),
                     by = 5
        )
      ) +
      labs(
        x = "Release Year",
        y = "Score"
      ) +
      theme_bw() +
      theme(
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)
      )
  })
  
  # Anime description for Click event
  output$anime_description <- renderUI({
    anime <- selected_anime()
    
    img_url <- anime$Image_URL[1]
    trailer_url <- anime$Trailer_URL[1]
    
    if (!is.null(anime) && nrow(anime) > 0) {
      tagList(
        div(
          style = if (!is.na(img_url) || !is.na(trailer_url)) {
            "display: flex; align-items: flex-start; gap: 20px;"
          } else {
            "display: block;"
          },
          if (!is.na(img_url) || !is.na(trailer_url)) {
            div(
              style = "flex: 1; max-width: 300px;",
              if (!is.na(img_url)) {
                tags$img(src = img_url, height = "350px", style = "display: block; margin-bottom: 10px;")
              },
              if (!is.na(trailer_url)) {
                tags$a(
                  href = trailer_url,
                  "Watch Trailer",
                  target = "_blank",
                  style = "display: inline-block; color: blue; font-size: 14px; margin-top: 10px;"
                )
              }
            )
          },
          
          # Right side (or full-width if no image/trailer): Anime Details
          div(
            style = if (!is.na(img_url) || !is.na(trailer_url)) {
              "flex: 2;"
            } else {
              "flex: 1; max-width: 100%;"
            },
            if (!is.na(anime$Japanese) && anime$Japanese != "NA") {
              tags$p(tags$b("Japanese Title: "), anime$Japanese)
            },
            if (!is.na(anime$English) && anime$English != "NA") {
              tags$p(tags$b("English Title: "), anime$English)
            },
            if (!is.na(anime$Score) && anime$Score != "NA") {
              tags$p(tags$b("Score: "), anime$Score)
            },
            if (!is.na(anime$release_year) && anime$release_year != "NA") {
              tags$p(tags$b("Release Year: "), anime$release_year)
            },
            if (!is.na(anime$Genres) && anime$Genres != "NA") {
              tags$p(tags$b("Genres: "), anime$Genres)
            },
            if (!is.na(anime$Studios) && anime$Studios != "NA") {
              tags$p(tags$b("Produced by: "), anime$Studios)
            },
            if (!is.na(anime$Status) && anime$Status != "NA") {
              tags$p(tags$b("Status: "), anime$Status)
            },
            if (!is.na(anime$Description) && anime$Description != "NA") {
              tags$p(tags$b("Synopsis: "), anime$Description)
            }
          )
        )
      )
    }
  })
  
  # Top anime production company over the years plot
  output$yearly_highest_rated <- renderPlot({
    ggplot(highest_rated_per_year, aes(x = as.factor(release_year), y = Score, group = 1)) +
      geom_point(aes(color = Studios),size = 3) +
      geom_line(size = 0.5, color = "grey", alpha = 0.8) +
      geom_text_repel(
        aes(color = Studios, label = Studios),
        size = 4,
        max.overlaps = Inf,
        box.padding = 0.4,
        point.padding = 0.6,
        min.segment.length = 0,
        segment.color = "gray70",
        segment.size = 0.3
      ) +
      scale_x_discrete(breaks = seq(min(highest_rated_per_year$release_year),
                                    max(highest_rated_per_year$release_year),
                                    by = 3
      )) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
        x = "Release Year",
        y = "Score",
      ) +
      theme_bw() +
      theme(
        legend.position = "None",
        axis.text = element_text(size = 12),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.margin = margin(t = 20, r = 10, b = 20, l = 10),
        panel.grid.minor = element_blank(),
      )
  })
  
  # table to show the ranking of production company
  output$ranking <- renderDT({
    highest_rated_per_year |>
      count(Studios, name = "Count") |>
      arrange(desc(Count)) |>
      rename("Number of Highest Score Animation Produced" = Count)
  })
  
  # PCA Scatter Plot
  output$pca_scatter <- renderPlot({
    pca_result <- anime_pca()$pca_data
    plot_pca_scatter(pca_result)
  })
  
  # PCA Component Contributions Plot
  output$pca_components <- renderPlot({
    pca_data <- anime_pca()$pca_prep
    plot_pca_components(pca_data, n_components = 5)
  })
  
  # PCA variance plot
  output$pca_variance <- renderPlot({
    pca_data <- anime_pca()$pca_prep
    plot_pca_variances(pca_data)
  })
}

shinyApp(ui, server)