# semantic similarity cosine threshold ------------------------------------

cosine_calculation_threshold <- function(reference_statement,
                                         cosine_sim_threshold = 0.5,
                                         embedding_model,
                                         sentence_matrix,
                                         df) {
  # Initialize an empty data frame to store the results
  combined_sentence_candidates <- data.frame()
  
  ref_sentence <- reference_statement
  
  cosine_sim_threshold <- cosine_sim_threshold
  
  reference_vector <- bt_do_embedding(embedding_model, ref_sentence)
  
  sentence_dot_products <- sentence_matrix %*% reference_vector
  
  sentence_norm_matrix <- sqrt(rowSums(sentence_matrix ^ 2))
  
  reference_norm <- sqrt(sum(reference_vector ^ 2))
  
  sentence_cosine_sims <- sentence_dot_products / (sentence_norm_matrix * reference_norm)
  
  # Create a data frame for the current reference sentence
  current_sentence_candidates <- df %>%
    mutate(cosine_sim = as.numeric(sentence_cosine_sims)) %>%
    relocate(cosine_sim) %>%
    filter(cosine_sim > cosine_sim_threshold) %>%
    arrange(desc(cosine_sim)) %>% 
    filter(!duplicated(universal_message_id)) 
  
  return(current_sentence_candidates)
  
}


# semantic search cosine n documents --------------------------------------

top_posts_cosine_similarity <- function(reference_statement,
                                        embedding_model,
                                        sentence_matrix,
                                        df,
                                        n = 50) {
  
  # Initialize an empty data frame to store the results
  combined_sentence_candidates <- data.frame()
  
  ref_sentence <- reference_statement
  
  reference_vector <- bt_do_embedding(embedding_model, ref_sentence)
  
  sentence_dot_products <- sentence_matrix %*% reference_vector
  
  sentence_norm_matrix <- sqrt(rowSums(sentence_matrix ^ 2))
  
  reference_norm <- sqrt(sum(reference_vector ^ 2))
  
  sentence_cosine_sims <- sentence_dot_products / (sentence_norm_matrix * reference_norm)
  
  # Create a data frame for the current reference sentence
  current_sentence_candidates <- df %>%
    mutate(cosine_sim = as.numeric(sentence_cosine_sims)) %>%
    relocate(cosine_sim) %>%
    arrange(desc(cosine_sim)) %>% 
    filter(!duplicated(universal_message_id)) %>% 
    slice_head(n = n)
  
  return(current_sentence_candidates)
}

# semantic search obtain doc_ids ------------------------------------------

get_doc_ids_semantic <- function(reference_statement,
                                 embedding_model,
                                 sentence_matrix,
                                 df,
                                 id_column,
                                 type = c("threshold", "n"),
                                 n = NULL,
                                 cosine_sim_threshold = NULL)
  
{
  type <- match.arg(type)
  
  # Your function logic here
  if (type == "threshold") {
    
    doc_ids <- cosine_calculation_threshold(
      reference_statement = reference_statement,
      embedding_model = embedding_model,
      sentence_matrix = sentence_matrix,
      df = df,
      cosine_sim_threshold =  cosine_sim_threshold
    ) %>% 
      pull(id_column)
    
  } else if (type == "k") {
    
    doc_ids <- top_posts_cosine_similarity(
      reference_statement = reference_statement,
      embedding_model = embedding_model,
      sentence_matrix = sentence_matrix,
      df = df,
      n = n
    ) %>% 
      pull(id_column)
  }
  
  return(doc_ids)
}

