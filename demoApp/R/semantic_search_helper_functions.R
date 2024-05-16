
# Utils -------------------------------------------------------------------

highlight_sentences <- function(text, sentences) {
  sentences <- unique(sentences)
  
  for (sentence in sentences) {
    highlighted_sentence <- paste0("<b>", sentence, "</b>")
    
    text <- str_replace_all(text, fixed(sentence), highlighted_sentence)
  }
  
  return(text)
}

insert_line_breaks <- function(text, n = 10) {
  
  words <- strsplit(text, " ")[[1]]
  
  paste(sapply(seq(1, length(words), n), 
               function(i) { paste(words[i:min(i + n - 1, length(words))], collapse = " ") }), 
        collapse = "<br>")
  
}

adjust_colour_lighter <- function(colour_hex, og_val) {
  
  rgb_vals <- col2rgb(colour_hex)
  
  rgb_new <- rgb_vals * og_val + 255 * (1 - og_val)
  
  rgb_new <- pmin(rgb_new, 255)
  
  new_colour_hex <- rgb(rgb_new[1,] / 255, rgb_new[2,] / 255, rgb_new[3,] / 255)
  return(new_colour_hex)
}

adjust_colour_darker <- function(colour_hex, og_val) {
  
  rgb_vals <- col2rgb(colour_hex)
  
  rgb_new <- rgb_vals * og_val
  
  rgb_new <- pmax(rgb_new, 0)
  
  new_colour_hex <- rgb(rgb_new[1,] / 255, rgb_new[2,] / 255, rgb_new[3,] / 255)
  return(new_colour_hex)
}

process_sentences <- function(doc_id, example_sentences) {
  doc_id %>%
    group_by(document) %>% # Change to appropriate document column
    mutate(
      sentences = list(sentence),
      text_copy = first(text_copy) # Change to appropriate text column
    ) %>%
    ungroup() %>%
    mutate(test_text = map2_chr(text_copy, sentences, highlight_sentences)) %>%
    distinct(document, .keep_all = TRUE) %>% # Change to appropriate document column
    right_join(example_sentences) %>%
    mutate(highlighted = case_when(is.na(cosine_sim) ~ FALSE,
                                   T ~ TRUE)) %>% # Change to appropriate document column
    distinct(document, .keep_all = TRUE) %>%
    mutate(test_text = case_when(
      is.na(cosine_sim) ~ text_copy,
      TRUE ~ test_text
    )) %>%  mutate(text_with_breaks = sapply(test_text, insert_line_breaks)) %>% 
    select(document, text_with_breaks, highlighted)
}

generate_topic_colours <- function(example_sentences_2) {
  k <- n_distinct(example_sentences_2$topic)
  
  eg_colours <- viridis::viridis(k)
  
  adjusted_colours_lighter_0.5 <- map_chr(eg_colours, ~adjust_colour_lighter(.x, og_val = 0.5))
  
  setNames(adjusted_colours_lighter_0.5, unique(example_sentences_2$topic))
}

# Function to prepare example data
prepare_example_data <- function(example_sentences_2, topic_colours) {
  example_sentences_2 %>%
    mutate(colour_mapped = if_else(new_colour == "#cccccc", "#cccccc", topic_colours[new_colour])) %>%
    mutate(text_with_breaks = sapply(test_text, insert_line_breaks))
}

# Function to filter grey points
filter_grey_points <- function(example) {
  example %>% filter(new_colour == "#cccccc") %>% mutate(opacity = 0.2)
}

# Function to filter highlight points
filter_highlight_points <- function(example) {
  example %>% filter(new_colour != "#cccccc") %>% mutate(opacity = 1)
}

# Function to generate cluster lookup
generate_cluster_lookup <- function(example) {
  example %>%
    group_by(topic) %>%
    summarise(
      topic_number = cur_group_id(),
      label = first(topic),
      centroid_x = mean(V1),
      centroid_y = mean(V2)
    )
}


# Cosine similarity calculation -------------------------------------------

cosine_calculation_threshold_sentence <- function(reference_statement,
                                                  cosine_sim_threshold = 0.5,
                                                  embedding_model,
                                                  sentence_matrix,
                                                  df) {
  ref_sentence <- reference_statement
  
  reference_vector <- bt_do_embedding(embedding_model, ref_sentence)
  
  sentence_dot_products <- sentence_matrix %*% reference_vector
  
  sentence_norm_matrix <- sqrt(rowSums(sentence_matrix ^ 2))
  
  reference_norm <- sqrt(sum(reference_vector ^ 2))
  
  sentence_cosine_sims <- sentence_dot_products / (sentence_norm_matrix * reference_norm)
  
  current_sentence_candidates <- df %>%
    mutate(cosine_sim = as.numeric(sentence_cosine_sims)) %>%
    relocate(cosine_sim) %>%
    filter(cosine_sim > cosine_sim_threshold) %>%
    arrange(desc(cosine_sim))
  
  return(current_sentence_candidates)
  
}
