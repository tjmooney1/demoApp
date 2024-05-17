embed_text_ui <- function(id){
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::uiOutput(ns("embeddedTexts"))
  )
}

embed_text_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    selected_texts <- shiny::reactive({
      req(shiny::isTruthy(r$selected_range))
      print(r$selected_range)
      permalinks <- r$df() %>%
        dplyr::filter(rowid %in% r$selected_range) %>%
        dplyr::pull(permalink)

      return(permalinks)
    })
    
    permalink_embeds <- reactive({
      if(!shiny::isTruthy(selected_texts()) | !length(selected_texts()) > 0){
        validate("Select some data first!")
      }
      
      lapply(selected_texts(), embed_switch)
    })
   
    output$embeddedTexts <- renderUI({
      do.call(bslib::layout_column_wrap, c(width = 1/3, permalink_embeds()))
    })

  })
}