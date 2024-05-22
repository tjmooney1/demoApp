# sysfonts::font_add(family = "Cinzel-Regular", regular = "/Users/aoiferyan/Library/Fonts/Cinzel-Regular.ttf")
# sysfonts::font_add(family = "Cinzel-SemiBold", regular = "/Users/aoiferyan/Library/Fonts/Cinzel-SemiBold.ttf")

ui <- bslib::page_fillable(
  tags$style(HTML("g.hovertext > path {opacity: .8;}")),
  
  theme = bslib::bs_theme(
    bootswatch = "sandstone",
    heading_font = bslib::font_face(family = "Cinzel-Regular",
                                    src = "/Users/aoiferyan/Library/Fonts/Cinzel-Regular.ttf"),
    base_font = bslib::font_face(family = "Cinzel-SemiBold",
                                 src = "/Users/aoiferyan/Library/Fonts/Cinzel-SemiBold.ttf")
  ),

      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          dataUploadUi("data_upload_panel"),
          searchUi("semantic_search_panel"),
          open = TRUE),
        bslib::navset_card_tab(
          bslib::nav_panel(
            "Data Landscape",
            height = "900px",
            umapUi("umap_panel"),
            fillable = TRUE,
            fill = TRUE
          ),
          bslib::nav_panel(
            "Selected Posts",
            height = "900px",
            embed_text_ui("embed_text_panel"),
            fillable = TRUE,
            fill = TRUE
          ) 
        )
    )
    
  )

