semantic_search_UI <- function(id) {
  
  shiny::tagList(
    # sidebarPanel(
    shiny::textInput(NS(id, "search_term"), "Enter search term:", value = "AI art"),
    shiny::numericInput(NS(id, "dot_prod_threshold"), "Dot product threshold", value = 5, min = 0, max = 10, step = 0.2),
    shiny::actionButton(NS(id, "update_plot"), "Update Plot")
    # )
  )
}

semantic_searchServer <- function(id, r) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # source("~/Documents/git-repos/demoApp/demoApp/R/semantic_search_helper_functions.R")

    example_sentences <- readr::read_csv("~/Google Drive/My Drive/Share_Clients/data_science_project_work/hackafun/data/semantic_search_data/displayr_example_sentences.csv") %>%
      dplyr::rename(rowid = row_id)
    
    multi_qa_matrix_sentences <- readr::read_csv(here::here("app/data/cosmetic_sentences_embeddings_quant.csv")) %>% 
                                                   as.matrix()
    
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
      print("searching")
      semantic_similarity_output <- quant_dot_product_threshold_sentence(
        reference_statement = input$search_term,
        cosine_sim_threshold = input$dot_prod_threshold,
        embedding_model = "multi-qa-mpnet-base-cos-v1",
        sentence_matrix = multi_qa_matrix_sentences,
        df = example_sentences
      ) %>% 
        process_sentences(example_sentences)
      
      r$highlight_df <- shiny::reactive({semantic_similarity_output})

    })
    
    
    })
  }



