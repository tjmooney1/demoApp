ui <- bslib::page_fillable(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "minty",
    `enable-rounded` = TRUE,
    # `navbar-bg` = "#000000",
    # primary = "#FF7518",
    # secondary = "#000000",
    # fg = "#000",
    # bg = "white"
    ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          dataUploadUi("data_upload_panel"),
          semantic_search_UI("semantic_search_panel"),
          open = TRUE),
        bslib::card(
            height = "900px",
        umapUi("umap_panel"),
        fillable = TRUE,
        fill = TRUE
      )
    ),
    embed_text_ui("embed_text_panel")
  )

