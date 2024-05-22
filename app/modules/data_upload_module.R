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

dataUploadServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    category <- shiny::reactive({
      switch(input$dataset,
             "Automotive" = "automotive",
             "Beauty & Cosmetics" = "fashion",
             "Food & Beverages" = "technology")
    })
    
    
    df <- reactive({
      data <- readr::read_rds(here::here("app/data/cosmetic_data.rds"))

      # ----
    })
    
    shiny::observeEvent(df(), {
      r$df <- df
    })

    
  })
}