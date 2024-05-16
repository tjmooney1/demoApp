server <- function(input, output) {
  
  df <- dataUploadServer("data_upload_panel")
  
  umapServer("umap_panel", df)
  
  

    
}

