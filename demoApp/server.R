server <- function(input, output, session) {
  
  r <- shiny::reactiveValues()
  
  df <- dataUploadServer("data_upload_panel", r)
  
  umapServer("umap_panel", r)
  
  # highlighted_df <- semantic_searchServer("semantic_serach_panel")


}

