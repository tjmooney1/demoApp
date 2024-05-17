library(shiny)
library(here)
library(magrittr)

# Temporary to allow for live reloads in an RStudio background job or app running in a terminal:
options(shiny.autoreload=TRUE)
options(shiny.port = 7775)
options(shiny.host = "127.0.0.1")

# Source Business logic/helper functions
source(here("demoApp/R/helper_functions.R"))
source(here("demoApp/R/semantic_search_helper_functions.R"))
source(here("demoApp/R/html_embed.R"))

# Source modules
source(here("demoApp/modules/umap_module.R"))
source(here("demoApp/modules/data_upload_module.R"))
source(here("demoApp/modules/semantic_search_module.R"))

# Source App Files
source(here("demoApp/ui.R"))
source(here("demoApp/server.R"))

shinyApp(ui, server)