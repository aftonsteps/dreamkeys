#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  library(ggkeyboard)
  
  observeEvent(input$browser,{
    browser()
  })
  
  callModule(mod_keyboard_picker_server, 
             "keyboard_picker")
}
