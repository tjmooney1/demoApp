#' UMAP ui function
#'
#' @param id param for shiny identification
#'
#' @noRd
umapUi <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(style = "position: absolute; top: 0.5; right: 0;",
               shinyWidgets::prettySwitch(
                 inputId = ns("plot_selection"),
                 label = "Cluster View"
               )
    ),
    shiny::div(style = "position: absolute; top: 1; right: 0;",
               shiny::uiOutput(ns("display_plot")),
               shiny::tags$img(src ="share_logo.png", align = "left",
                               width = "200.42px", height = "70.42px")
    ),
  )
}

#' UMAP server function
#'
#' @param id param for shiny identification
#'
#' @noRd
umapServer <- function(id, df){
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
      createUmap(df = df,
                 tracking_id = "test",
                 title = "test")
    })
    
    # output$raster_plot <- shiny::renderImage({
    
    # })
    
  })
}