embed_text_ui <- function(id){
  tagList(
    shiny::fluidRow(
      shiny::uiOutput(NS("embeddedTexts"))
    )
  )
}

embed_text_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  })
}