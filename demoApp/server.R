server <- function(input, output, session) {
  
  df <- dataUploadServer("data_upload_panel")
  
  umapServer("umap_panel", df)
}

