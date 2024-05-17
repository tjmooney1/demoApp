semantic_search_UI <- function(id) {
  
  tagList(
    # sidebarPanel(
    textInput(NS(id, "search_term"), "Enter search term:", value = "AI art"),
    numericInput(NS(id, "cosine_sim_thresholds"), "Cosine similarity threshold", value = 0.3, min = 0, max = 1),
    actionButton(NS(id, "update_plot"), "Update Plot")
    # )
  )
}

semantic_searchServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # source("~/Documents/git-repos/demoApp/demoApp/R/semantic_search_helper_functions.R")
    example_sentences <- readr::read_csv("~/Google Drive/My Drive/Share_Clients/data_science_project_work/hackafun/data/semantic_search_data/displayr_example_sentences.csv")

    multi_qa_matrix_sentences <- readr::read_rds("~/Google Drive/My Drive/Share_Clients/data_science_project_work/hackafun/data/semantic_search_data/example_multi-qa_embeddings_sentences.rds")
    
    # Reactive values to store intermediate data
    reactive_vals <- reactiveValues(
      example_sentences_2 = NULL,
      dataframe = NULL,
      grey_points = NULL,
      highlight_points = NULL,
      eg_cluster_lookup = NULL,
      topic_colours = NULL
    )
    
    # Reactive expression to calculate document IDs based on input
    semantic_similarity_output <- reactive({
      cosine_calculation_threshold_sentence(
        reference_statement = input$search_term,
        cosine_sim_threshold = input$cosine_sim_thresholds,
        embedding_model = "multi-qa-mpnet-base-cos-v1",
        sentence_matrix = multi_qa_matrix_sentences,
        df = example_sentences
      ) %>% 
        process_sentences(example_sentences)
    })
    
    return(semantic_similarity_output)
    
    })
  }



