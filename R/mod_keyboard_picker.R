#' keyboard_picker UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_keyboard_picker_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(sidebarPanel = sidebarPanel(selectInput(inputId = ns("pal_picker"),
                                                          label = "Palette",
                                                          choices = c("serika",
                                                                      "wahtsy",
                                                                      "cyberpunk",
                                                                      "varmilo",
                                                                      "t0mb3ry",
                                                                      "magic",
                                                                      "<build your own>")),
                                              uiOutput(outputId = ns("pal_builder")),
                                              selectInput(inputId = ns("key_type"),
                                                          label = "Layout",
                                                          choices = c("tkl",
                                                                      "full",
                                                                      "sixty percent"),
                                                          selected = "tkl"),
                                              numericInput(inputId = ns("font_size"),
                                                           label = "Font Size",
                                                           value = 3,
                                                           min = 1,
                                                           max = 3,
                                                           step = 0.1)),
                  mainPanel = mainPanel(plotOutput(outputId = ns("keyboard")))
    )
  )
}
    
#' keyboard_picker Server Function
#'
#' @noRd 
mod_keyboard_picker_server <- function(input, output, session){
  ns <- session$ns
  
  output$pal_builder <- renderUI({
    req(input$pal_picker == "<build your own>")
    
    background <- 
      colourpicker::colourInput(inputId = ns("bg"),
                                label = "Background",
                                value = "#FFC6FF")
    
    keyboard <- 
      colourpicker::colourInput(inputId = ns("key"),
                                label = "Keyboard",
                                value = "#BDB2FF")
    
    alphanumeric <-
      colourpicker::colourInput(inputId = ns("alpha"),
                                label = "Alphanumeric",
                                value = "#CAFFBF")
    
    accent <-
      colourpicker::colourInput(inputId = ns("acc"),
                                label = "Accent",
                                value = "#FFADAD")
    
    modifier <- 
      colourpicker::colourInput(inputId = ns("mod"),
                                label = "Modifier",
                                value = "#9BF6FF")
    
    numpad <- 
      colourpicker::colourInput(inputId = ns("num"),
                                label = "Numpad",
                                value = "#FFD6A5")
    
    arrow <- 
      colourpicker::colourInput(inputId = ns("arr"),
                                label = "Arrow",
                                value = "#FDFFB6")
    
    light <-
      colourpicker::colourInput(inputId = ns("lt"),
                                label = "Light",
                                value = "#A0C4FF")
    
    text <-
      colourpicker::colourInput(inputId = ns("text"),
                                label = "Text",
                                value = "#000000")
    
    return(tagList(background,
                   keyboard,
                   alphanumeric,
                   accent,
                   modifier,
                   numpad,
                   arrow,
                   light,
                   text))
  })
  
  pal_val <- reactive({
    if (input$pal_picker == "<build your own>") {
      return(c(background = input$bg, 
               keyboard = input$key, 
               alphanumeric = input$alpha, 
               accent = input$acc, 
               modifier = input$mod, 
               numpad = input$num, 
               arrow = input$arr, 
               light = input$lt, 
               text = input$text))
    } else {
      return(keyboard_palette(input$pal_picker))
    }
  })
  
  keyboard_type <- reactive({
    return(gsub(pattern = " ", replacement = "_", x = input$key_type))
  })
  
  mushroom <- magick::image_read("inst/app/www/mushroom_key.jpg")
  
  output$keyboard <- renderPlot({
    req(input$pal_picker)
    req(input$font_size)
    req(pal_val())
    req(keyboard_type())
    
    pal <- input$pal_picker
    font_size <- input$font_size
    val_pal <- pal_val()
    keyboard_type <- keyboard_type()
    
    k <- 
      ggkeyboard(keyboard = get(keyboard_type),
                 palette = val_pal,
                 font_size = font_size)
    
    g <- add_keycap(k, "Esc", mushroom)
    
    return(g)
  })
}
    
## To be copied in the UI
# mod_keyboard_picker_ui("keyboard_picker_ui_1")
    
## To be copied in the server
# callModule(mod_keyboard_picker_server, "keyboard_picker_ui_1")
 
