ui <- shiny::fluidPage(

  # theme = shinythemes::shinytheme("slate"),
  theme = shinythemes::shinytheme("sandstone"),
  # shiny::titlePanel("Data Exploration"),
  
  dataUploadUi("data_upload_panel"),
  umapUi("umap_panel")
  
)
