# semantic_search_UI <- function(id) {
#   
#   shiny::tagList(
#     # sidebarPanel(
#     shiny::textInput(NS(id, "search_term"), "Enter search term:", value = "AI art"),
#     shiny::numericInput(NS(id, "cosine_sim_thresholds"), "Cosine similarity threshold", value = 0.3, min = 0, max = 1, step = 0.1),
#     shiny::actionButton(NS(id, "update_plot"), "Update Plot")
#     # )
#   )
# }

searchUi <- function(id) {
  
  shiny::tagList(
    # sidebarPanel(
    shiny::textInput(NS(id, "semantic_search_term"), "Enter neural search term:", placeholder = "Ai Art"),
    shiny::numericInput(NS(id, "cosine_sim_thresholds"), "Cosine similarity threshold", value = 0.3, min = 0, max = 1, step = 0.1),
    shiny::textInput(NS(id, "keyword_search_term"), "Enter keyword search term:", placeholder = "Ai and Art"),
    shiny::actionButton(NS(id, "update_plot"), "Update Plot")
    # )
  )
}

semantic_searchServer <- function(id, r) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    example_sentences <- readr::read_csv("~/Google Drive/My Drive/Share_Clients/data_science_project_work/hackafun/data/semantic_search_data/displayr_example_sentences.csv") %>%
      dplyr::rename(rowid = row_id)

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
    
    observeEvent(input$update_plot, {
      # print("searching")
      # if (!is.null(input$keyword_search_term)){
      #   keyword_search_output <- r$df()[grep(input$keyword_serach_term, r$df()$text, ignore.case = TRUE), ]
      # }
      # 
      # if (!is)
      
      
      semantic_similarity_output <- cosine_calculation_threshold_sentence(
        reference_statement = input$semantic_search_term,
        cosine_sim_threshold = input$cosine_sim_thresholds,
        embedding_model = "multi-qa-mpnet-base-cos-v1",
        sentence_matrix = multi_qa_matrix_sentences,
        df = example_sentences
      ) %>% 
        process_sentences(example_sentences)
      
      r$highlight_df <- shiny::reactive({semantic_similarity_output})

    })
    
    
    })
  }



