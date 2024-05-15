ui <- fluidPage(
  # theme = shinythemes::shinytheme("slate"),
  theme = shinythemes::shinytheme("sandstone"),
  # shiny::titlePanel("Data Exploration"),
  br(),
  shinyWidgets::dropdown(
    shinyWidgets::pickerInput(
      inputId = "dataset",
      label = "Dataset",
      choices = c("Automotive",
                  "Beauty & Cosmetics",
                  "Food & Beverages"),
      options = list(iconBase = "fas"),
      choicesOpt = list(
        icon = c("glyphicon-cog", "glyphicon-eye-open", "glyphicon-apple" )
      )
    ),

    # circle = TRUE, status = "danger",
    icon = icon("folder"), width = "300px",

    tooltip = shinyWidgets::tooltipOptions(title = "Change datasets")
  ),
  shiny::div(style = "position: absolute; top: 0.5; right: 0;",
             shinyWidgets::prettySwitch(
               inputId = "plot_selection",
               label = "Cluster View"
             )
  ),
  shiny::div(style = "position: absolute; top: 1; right: 0;",
             shiny::uiOutput("display_plot"),
             shiny::tags$img(src ="share_logo.png", align = "left",
                             width = "200.42px", height = "70.42px")
             ),
  
)

server <- function(input, output) {

 category <- shiny::reactive({

    # these will need to change with new data
    switch(input$dataset,
           "Automotive" = "automotive",
           "Beauty & Cosmetics" = "fashion",
           "Food & Beverages" = "technology")
  })

  df <- reactive({
    file_location <- "~/Google Drive/My Drive/Share_Clients/data_science_project_work/hackafun/data/dummy_data/"

    file_path <- paste0(file_location, "dummy_data_", category(), ".csv")

    # adding dummy variables for now
    data <- readr::read_csv(file_path)

    # This can all go in final version ----
    data <- data %>%
      dplyr::mutate(v1 = rnorm(nrow(data), mean = 4, sd = 1),
                    v2 = rnorm(nrow(data), mean = 4.5, sd =1.2),
                    rowid = dplyr::row_number())

    k <- 10
    kmeans_clusters <- kmeans(data[c("v1", "v2")], centers = k)
    clusters_mapping <- data.frame(cluster_number = 1:10,
                                   clusters = c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten")
    )

    data <- data %>%
      mutate(cluster_number = kmeans_clusters$cluster) %>%
      merge(clusters_mapping, by = "cluster_number")

    # ----
    })

  output$display_plot <- shiny::renderUI({
    if (input$plot_selection == FALSE){
      plotly::plotlyOutput("umap_plot")
    } else {
      shiny::imageOutput("raster_plot")
    }
  })

    output$umap_plot <- plotly::renderPlotly({
      # print(head(df()))
      createUmap(df = df,
                 tracking_id = "test",
                 title = "test")
    })

    # output$raster_plot <- shiny::renderImage({

    # })
}

# Run the application
shinyApp(ui = ui, server = server)
