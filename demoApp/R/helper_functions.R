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


createUmapTesting <- function(df, tracking_id, title){
  if (is.reactive(df)){
    # message("is reactive")
    print(head(df()))
  } else {
    message("not reactive")
  }
}

insert_line_breaks <- function(text, n = 10) {
  words <- strsplit(text, " ")[[1]]
  paste(sapply(seq(1, length(words), n), function(i) {
    paste(words[i:min(i + n - 1, length(words))], collapse = " ")
  }), collapse = "<br>")
}

#' UMAP Ui Server Function
#'
#' @param id parameter for shiny identification
#' @param df reactive dataframe containing docs and embedding info 
#' @param cluster_var reactive list of groups vorresponding to docs in df that is the colour var in the umap
#'
#' @noRd
#' 
createUmap <- function(r){
  
  # colour functions ----
  colours <- viridis::viridis(n = length(unique(r$df()$topic)), begin = 0, end = 0.92, direction = 1)

  adjusted_colours_lighter_0.6 <- purrr::map_chr(colours, ~adjust_colour_lighter(.x, og_val = 0.6)) ## for points
  adjusted_colours_lighter_0.05 <- purrr::map_chr(colours, ~adjust_colour_lighter(.x, og_val = 0.05))
  adjusted_colours_darker_1 <- purrr::map_chr(colours, ~adjust_colour_darker(.x, og_val = 1)) ## for labels
  # ----

  # cluster labelling and colouring ----
  centroids <- r$df() %>%
    dplyr::group_by(topic) %>%
    dplyr::summarise(
      x = mean(V1),
      y = mean(V2)
    )

  cluster_lookup <- tibble::tibble(
    cluster = seq_along(unique(r$df()$topic)),
    label = unique(r$df()$topic),
    centroid_x = centroids$x,
    centroid_y = centroids$y
  )
  # ----

  # plot ----

  # p <- plotly::plotly() %>%
  #   plotly::add_trace(data = ~df(),
  #                     x = ~V1, y = ~V2,
  #                     type = "scattergl",
  #                     mode = "markers",
  #                     width = 1000, height = 700,
  #                     colors = colours,
  #                     marker = list(opacity = 0.7),
  #                     hoverinfo ="text",
  #                     text = ~text_with_breaks)
  
  if(is.null(r$highlight_df)){

    p <- r$df() %>%
      dplyr::mutate(text_with_breaks = sapply(text, insert_line_breaks)) %>%
      plotly::plot_ly(x = ~V1,
                      y = ~V2,
                      width = 1000, height = 700,
                      color = ~topic,
                      customdata = ~rowid,
                      type = "scattergl",
                      mode = "markers",
                      text = ~text_with_breaks,
                      hoverinfo = "text",
                      colors = adjusted_colours_lighter_0.6,
                      marker = list(opacity = 0.7),  # Adjust marker size and opacity
                      source = "umap_plot") 
  } else {
    # Keep only rows from df1 that do not have a match in df2
    grey_points <- r$highlight_df()[r$highlight_df()$highlighted == FALSE, ] %>%
      dplyr::mutate(colour = "grey80")
    highlight_points <- r$highlight_df()[r$highlight_df()$highlighted == TRUE, ]

    DT::datatable(grey_points)
    DT::datatable(highlight_points)

    p <- plotly::plot_ly(width = 1000, height = 700,) %>%
      plotly::add_trace(data = grey_points,
                x = ~V1, y = ~V2,
                type = "scattergl",
                mode = "markers",
                marker = list(opacity = 0.7, color = ~colour),
                # marker = list(color = ~colour_mapped, opacity = ~opacity),
                hoverinfo = "skip") %>%
      plotly::add_trace(data = highlight_points,
                x = ~V1, y = ~V2,
                type = "scattergl",
                mode = "markers",
                color = ~topic,
                marker = list(opacity = 0.7, color = adjusted_colours_lighter_0.6),
                # marker = list(color = ~colour_mapped, opacity = ~opacity, size = 10),
                hoverinfo ="text",
                text = ~text) %>%
      config(scrollZoom = TRUE) %>%
          layout(
            showlegend = FALSE,
            xaxis = list(showline = FALSE, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title =""),
            yaxis = list(showline = FALSE, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title =""),
            plot_bgcolor = "rgba(0, 0, 0, 0)",
            paper_bgcolor = "rgba(0, 0, 0, 0)"
          )
  }
  
  # if (!is.null(highlight_points)){
  #  
  #     
  # } else{
  # 
  # }
  
  
  p %>%
    plotly::layout(dragmode = "lasso",
                   xaxis = list(
                     showgrid = FALSE,
                     showline = FALSE,
                     zeroline = FALSE,
                     showticklabels = FALSE,
                     visile = FALSE,
                     title = ""
                   ),
                   yaxis = list(
                     showgrid = FALSE,
                     showline = FALSE,
                     zeroline = FALSE,
                     showticklabels = FALSE,
                     visile = FALSE,
                     title = ""
                   ),
                   showlegend = TRUE,
                   legend = list(title = "Topics")) %>%
    plotly::config(
      scrollZoom = TRUE,
      displaylogo = FALSE,
      edits = list(
        shapePosition = TRUE,
        annotation = TRUE
      )
    )
 
  
  
  # ----

  # cluster labelling ----
for (i in 1:nrow(cluster_lookup)) {

  formatted_label <- sprintf("<b>%s</b>", cluster_lookup$label[i])

  # Adding the shadow
  p <- p %>% plotly::add_annotations(
    x = cluster_lookup$centroid_x[i] ,  # slight offset for the shadow
    y = cluster_lookup$centroid_y[i] ,  # slight offset for the shadow
    text = cluster_lookup$label[i],
    showarrow = FALSE,
    opacity = 1,
    xshift = 2, yshift = -2, # Adjust shadow position
    font = list(size = 22,
                family = "Cinzel",
                # color = adjusted_colours_lighter_0.05[as.numeric(cluster_lookup$cluster[i ])]
                color = "white"
    )
  )

  # Adding the main annotation
  p <- p %>% plotly::add_annotations(
    x = cluster_lookup$centroid_x[i],
    y = cluster_lookup$centroid_y[i],
    text = cluster_lookup$label[i],
    showarrow = FALSE,
    font = list(size = 22,
                family = "Cinzel",
                # color = adjusted_colours_darker_1[as.numeric(cluster_lookup$cluster[i])]
                color = "#4E5180"
                )
  )
}
  # ----
  
  # styling for cluster labelling: This is not at all a finished product ----
  badge_css = "
    border-radius:6px;
    width:fit-content;
    max-width:75%;
    margin:2px;
    padding: 2px 10px 2px 10px;
    font-size: 10pt;
"
  hover_text_template = "
<div>
    <div style=\"font-size:12pt;padding:2px;\">{{hover_text}}</div>
    <div style=\"background-color:{{color}};color:#fff;{badge_css}\">{{primary_field}}</div>
    <div style=\"background-color:#eeeeeeff;{badge_css}\">citation count: {{citation_count}}</div>
    </div>
    "

return(p)


}
