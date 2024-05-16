library(shiny)
library(here)
library(magrittr)
options(shiny.autoreload=TRUE)
options(shiny.port = 7775)
options(shiny.host = "127.0.0.1")

# Source Business logic/helper functions
source(here("R/helper_functions.R"))
source(here("R/semantic_search_helper_functions.R"))

# Source modules
source(here("demoApp/modules/umap_module.R"))
source(here("demoApp/modules/data_upload_module.R"))


# Source App Files
source(here("demoApp/ui.R"))
source(here("demoApp/server.R"))

shinyApp(ui, server)