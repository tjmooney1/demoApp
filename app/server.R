server <- function(input, output, session) {
  
  r <- shiny::reactiveValues()
  
  dataUploadServer("data_upload_panel", r)
  
  umapServer("umap_panel", r)
  
  embed_text_server("embed_text_panel", r)
  
  # highlighted_df <- semantic_searchServer("semantic_serach_panel")


}

