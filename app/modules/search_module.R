searchUi <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::textInput(NS(id, "search_term"), "Enter search term:", placeholder = "face"),
    htmltools::tags$style(HTML("
    .irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
      visibility: hidden !important;
    }"
                               )),
    htmltools::div(
      style = "position: relative; margin-top: 20px; width: 200px;",
      shiny::sliderInput(ns("semantic_sim_threshold"), 
                  label = NULL,
                  min = 0, max = 1, value = 0.5, ticks = FALSE),
      htmltools::div( # slider title
        style = "position: absolute; top: -15px; left: 20%; transform: translateX(-30%);",
        "Term Similarity"
      ),
      htmltools::div( # slider lower bound label
        style = "position: absolute; top: 45px; left: 0; transform: translateX(-20%); font-family: Cinzel-Regular; src: fonts/Cinzel-Regular.ttf;",
        "Low"
      ),
      htmltools::div( #  slider halfway label
        style = "position: absolute; top: 45px; left: 50%; transform: translateX(-50%); font-family: Cinzel-Regular; src: fonts/Cinzel-Regular.ttf;",
        "Medium"
      ),
      htmltools::div( # slider upper bound label
        style = "position: absolute; top: 45px; left: 100%; transform: translateX(-70%); font-family: Cinzel-Regular; src: fonts/Cinzel-Regular.ttf;",
        "High"
        )
      ),
  htmltools::tags$div(style = "margin-top: 10px; margin-bottom: 10px;"),   # break before action button
  shiny::actionButton(NS(id, "update_plot"), "Update Plot")
  )
}

searchServer <- function(id, r) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
      
      keyword_search <- reactive({
        shiny::validate(
          shiny::need(grepl("^[a-zA-Z0-9 ]*$", input$search_term), "Invalid characters detected! Please use only alphanumeric characters and spaces.")
        )
        
        r$df() %>%
          dplyr::filter(grepl(input$search_term, text_clean, ignore.case = TRUE))
          # dplyr::select(text_with_breaks, highlighted, V1, V2, universal_message_id, sender_screen_name, topic, topic_title)

      })
      
      semantic_similarity_output <- cosine_calculation_threshold_sentence(
        reference_statement = input$search_term,
        cosine_sim_threshold = input$semantic_sim_threshold,
        # reference_statement = "face",
        # cosine_sim_threshold = 0.5,
        embedding_model = "multi-qa-mpnet-base-cos-v1",
        sentence_matrix = multi_qa_matrix_sentences,
        df = example_sentences
      ) %>% 
        process_sentences(example_sentences)
      
      r$highlight_df <- shiny::reactive({
     
        semantic_similarity_output %>%
          dplyr::filter(highlighted == TRUE) %>%
          dplyr::bind_rows(keyword_search()) %>%
          dplyr::distinct(universal_message_id, .keep_all = TRUE) 
          
        })
      
      r$grey_df <- shiny::reactive(
        
        r$df() %>%
          dplyr::anti_join(r$highlight_df(), by = "universal_message_id")
      )
      

    })
    
    
    })
  }



