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
                                                           step = 0.1),
                                              HTML("<hr/>"),
                                              h4("~ Keycap Upload ~"),
                                              uiOutput(ns("keycap_choice")),
                                              uiOutput(ns("image_upload")),
                                              actionButton(inputId = ns("add_keycap"),
                                                           label = "Add Keycap"),
                                              actionButton(inputId = ns("remove_keycaps"),
                                                           label = "Remove All")
                                              ),
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
    key_name <- gsub(pattern = " ", replacement = "_", x = input$key_type)
    layout <- POSSIBLE_LAYOUTS[[key_name]]
    
    return(layout)
  })
  
  output$keycap_choice <- renderUI({
    req(keyboard_type())
    
    keyboard_type = keyboard_type()
    
    keycap_choice <- selectInput(inputId = ns("keycap_choice"),
                                  label = "Key",
                                  choices = keyboard_type$key)
    
    return(keycap_choice)
  })
  
  output$image_upload <- renderUI({
    req(!is.null(input$add_keycap))
    fileInput(inputId = ns("image_upload"),
              label = "Keycap Image",
              placeholder = "image.png")
  })
  
  images <- reactiveVal(list())
  
  observeEvent(c(input$add_keycap), {
    image <- input$image_upload
    
    if (is.null(image)) { return(NULL) }
    
    image <- magick::image_read(image$datapath)
    images(c(images(), list(image)))
  })
  
  keycap_choices <- reactiveVal(value = c())
  
  observeEvent(c(input$add_keycap), {
    keycap <- input$keycap_choice
    keycap_choices(c(keycap_choices(), keycap))
  })
  
  observeEvent(c(input$remove_keycaps), {
    images(list())
    keycap_choices(c())
  })
  
  output$keyboard <- renderPlot({
    req(input$pal_picker)
    req(input$font_size)
    req(pal_val())
    req(keyboard_type())

    pal <- input$pal_picker
    font_size <- input$font_size
    val_pal <- pal_val()
    keyboard_type <- keyboard_type()
    images <- images()
    keycap_choices <- keycap_choices()
    
    g <-
      ggkeyboard(keyboard = keyboard_type,
                 palette = val_pal,
                 font_size = font_size)
    
    if (length(images) > 0 && length(keycap_choices) > 0) {
      g <- add_keycap(g, keycap_choices, images)
    }
    
    return(g)
  })
}