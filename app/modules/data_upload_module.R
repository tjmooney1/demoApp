dataUploadUi <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::br(),
      shinyWidgets::pickerInput(
        inputId = ns("dataset"),
        label = "Dataset",
        choices = c("Beauty & Cosmetics",
                    "Automotive",
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
             "Beauty & Cosmetics" = "fashion",
             "Automotive" = "automotive",
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