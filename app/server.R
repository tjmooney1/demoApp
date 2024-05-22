server <- function(input, output, session) {
  # bslib::bs_themer()
  r <- shiny::reactiveValues(
    highlight_df = NULL
  )

  dataUploadServer("data_upload_panel", r)
  
  umapServer("umap_panel", r)
  
  embed_text_server("embed_text_panel", r)
  semantic_searchServer("semantic_search_panel", r)
}

