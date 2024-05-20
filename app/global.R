# This assumes we're in a deployed environment, so we'll use file paths that start at app/
library(shiny)
library(magrittr)

source("modules/umap_module.R")
source("modules/data_upload_module.R")
source("modules/semantic_search_module.R")
source("modules/embed_texts_module.R")

source("R/helper_functions.R")
source("R/html_embed.R")
source("R/semantic_search_helper_functions.R")