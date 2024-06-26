#' UMAP ui function
#'
#' @param id param for shiny identification
#'
#' @noRd
umapUi <- function(id){
  ns <- shiny::NS(id)
    htmltools::div(
      style = "position: relative",
      shinycssloaders::withSpinner( 
        shiny::uiOutput(ns("display_plot"))
      ),
      htmltools::div(
        style = "position: absolute; top: 10px; left: 10px",
                   shinyWidgets::prettySwitch(
                     inputId = ns("plot_selection"),
                     label = "Cluster View"
                   )
      )
    )
}

#' UMAP server function
#'
#' @param id param for shiny identification
#'
#' @noRd
umapServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$display_plot <- shiny::renderUI({
      if (input$plot_selection == FALSE){
        plotly::plotlyOutput(ns("umap_plot"))
      } else {
        shiny::imageOutput(ns("raster_plot"))
      }
    })
    
    output$umap_plot <- plotly::renderPlotly({
      createUmap(r)
    })
    
   
    
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "umap_plot"), {
      # r$selected_range <- plotly::event_data("plotly_selected", source = "umap_plot")$customdata
      r$selected_range <- plotly::event_data("plotly_selected", source = "umap_plot")$key

    })

    
  })
}