server <- function(input, output, session) {
  
  r <- shiny::reactiveValues(
    highlight_df = NULL
  )
  
  observe({
    if (is.null(r$highlight_df)){
      print(is.null(r$highlight_df))
    } else {
      class(r$highlight_df())
    }

  })
  dataUploadServer("data_upload_panel", r)
  
  umapServer("umap_panel", r)
  
  semantic_searchServer("semantic_search_panel", r)
 
}

