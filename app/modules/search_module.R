searchUi <- function(id) {
  
  shiny::tagList(
    # sidebarPanel(
    # shiny::textInput(NS(id, "semantic_search_term"), "Enter neural search term:", placeholder = "Ai Art"),
    # shiny::numericInput(NS(id, "cosine_sim_thresholds"), "Cosine similarity threshold", value = 0.3, min = 0, max = 1, step = 0.1),

    shiny::textInput(NS(id, "search_term"), "Enter search term:", placeholder = "AI art"),
    shiny::numericInput(NS(id, "dot_prod_threshold"), "Dot product threshold", value = 5, min = 0, max = 10, step = 0.2),
    # shiny::textInput(NS(id, "keyword_search_term"), "Enter keyword search term:", placeholder = "Ai and Art"),
    shiny::actionButton(NS(id, "update_plot"), "Update Plot")
    # )
  )
}

searchServer <- function(id, r) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # example_sentences <- readr::read_csv("~/Google Drive/My Drive/Share_Clients/data_science_project_work/hackafun/data/cleaned_data/for_app/cosmetic_sentences.csv")
    # 
    # multi_qa_matrix_sentences <- readr::read_csv("~/Google Drive/My Drive/Share_Clients/data_science_project_work/hackafun/data/cleaned_data/for_app/cosmetic_sentences_embeddings_quant.csv") %>%
    #                                                as.matrix()
    
    example_sentences <- readr::read_rds("~/Google Drive/My Drive/Share_Clients/data_science_project_work/hackafun/data/cleaned_data/for_app/cosmetic_sentences.rds")

    multi_qa_matrix_sentences <- readr::read_rds("~/Google Drive/My Drive/Share_Clients/data_science_project_work/hackafun/data/cleaned_data/for_app/cosmetic_sentences_embeddings.rds") %>%
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
      
      keyword_search_term <- reactive({
        shiny::validate(
          shiny::need(grepl("^[a-zA-Z0-9 ]*$", input$search_term), "Invalid characters detected! Please use only alphanumeric characters and spaces.")
        )
        
        r$df() %>%
          dplyr::filter(grepl(text_clean, input$search_term, ignore.case = TRUE)) %>%
          dplyr::select(text_with_breaks, highlighted, V1, V2, universal_message_id, sender_screen_name, topic, topic_title)

      })
      
      print(head(keyword_search_term))
      
      

      print("searching")
      # semantic_similarity_output <- quant_dot_product_threshold_sentence(
      #   reference_statement = input$search_term,
      #   dot_prod_threshold = input$dot_prod_threshold,
      #   embedding_model = "multi-qa-mpnet-base-cos-v1",
      #   sentence_matrix = multi_qa_matrix_sentences,
      #   df = example_sentences
      # ) %>% 
      #   process_sentences_quant(example_sentences)
      
      semantic_similarity_output <- cosine_calculation_threshold_sentence(
        # reference_statement = input$search_term,
        # cosine_sim_threshold = input$dot_prod_threshold,
        reference_statement = "face",
        cosine_sim_threshold = 0.5,
        embedding_model = "multi-qa-mpnet-base-cos-v1",
        sentence_matrix = multi_qa_matrix_sentences,
        df = example_sentences
      ) %>% 
        process_sentences(example_sentences)
      
      r$highlight_df <- shiny::reactive({semantic_similarity_output})

    })
    
    
    })
  }



