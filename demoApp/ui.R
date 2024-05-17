ui <- shiny::fluidPage(
  
  bslib::page_fillable(
    # bslib::card(
    #   fill = TRUE,
    #   height = "900px",
    #   bslib::card_header("SHARE Creative Data Explore"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          dataUploadUi("data_upload_panel"),
          semantic_search_UI("semantic_search_panel"),
          open = TRUE),
        bslib::card(
            # fill = TRUE,
            height = "900px",
            # bslib::card_header("SHARE Creative Data Explore"),
        umapUi("umap_panel"),
        fillable = TRUE,
        fill = TRUE
      )
    )
  )
 
)
