dataUploadUi <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::br(),
      shinyWidgets::pickerInput(
        inputId = ns("dataset"),
        label = "Dataset",
        choices = c("Automotive",
                    "Beauty & Cosmetics",
                    "Food & Beverages"),
        options = list(iconBase = "fas"),
        choicesOpt = list(
          icon = c("glyphicon-cog", "glyphicon-eye-open", "glyphicon-apple" )
        )
      )
  )
}

dataUploadServer <- function(id){
  moduleServer(id, function(input, output, session) {
    # ns <- session$ns
    
    
    category <- shiny::reactive({
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
        dplyr::mutate(cluster_number = kmeans_clusters$cluster) %>%
        merge(clusters_mapping, by = "cluster_number")
      
      # ----
    })
    
    return(df)
    
  })
}