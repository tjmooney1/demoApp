dataUploadUi <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::br(),
    shiny::tags$head(
      # Ensure Font Awesome is included
      shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css")
    ),
    shinyWidgets::pickerInput(
      inputId = ns("dataset"),
      label = "Dataset",
      choices =  c("Beauty & Cosmetics",
                   "Automotive",
                   "Food & Beverages"),
      options = list(`icon-base` = "fa"),
      choicesOpt = list(
        icon = c("fa-spa",
                 "fa-car",
                 "fa-burger")
      ),
    )
  )
}

dataUploadServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    category <- shiny::reactive({
      switch(input$dataset,
             "Beauty & Cosmetics" = "cosmetic",
             "Automotive" = "automotive",
             "Food & Beverages" = "technology")
    })
    
    
    df <- reactive({
      # data <- readr::read_rds(here::here("app/data/cosmetic_data.rds"))
      
      file <- googledrive::drive_get(paste0("for_app/", category(), "_df.rds"))
      temp_file <- tempfile(fileext = ".rds")
      googledrive::drive_download(file, path = temp_file, overwrite = TRUE)
      readr::read_rds(temp_file) %>%
        dplyr::mutate(text_with_breaks = sapply(text, insert_line_breaks))

      # ----
    })
    
    shiny::observeEvent(df(), {
      r$df <- df
      
      print(colnames(r$df()))
    })

    
  })
}