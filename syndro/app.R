load("latest_output.RData")

source("include.R")

rangeDates <- max(fits[[1]]$dates, na.rm=TRUE)
rangeDates <- c(rangeDates-35, rangeDates)
rangeDates.ini <- range(fits[[1]]$dates, na.rm=TRUE)


library(shiny)

ui <- shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "mine.css")
  ),
  
  uiOutput("ui_title"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("ui_lang"),
      uiOutput("ui_syndromes"),
      uiOutput("ui_camps"),
      uiOutput("ui_rangeDates"),
      img(src='keelpno.png', width=199, height=157, 
          style="display: block; margin-left: auto; margin-right: auto;")
    ),
    mainPanel(
      uiOutput("ui_mytabs")
    )
  )
))


server <- shinyServer(function(input, output) {
  output$ui_mytabs <- renderUI({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    mytabs <- list(
      tabPanel(c(EN="Plot", GR="Διάγραμμα")[lang], plotOutput("mainPlot")),
      tabPanel(c(EN="Table", GR="Πίνακας")[lang])
    )
    do.call(tabsetPanel, mytabs)
  })
  
  output$ui_title <- renderUI({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    if (lang=="GR") {
      titlePanel("Επιδημιολογική επιτήρηση σε σημεία φροντίδας υγείας προσφύγων/μεταναστών")
    } else {
      titlePanel("Epidemiological surveillance in points of care for refugees/migrants")
    }
  })
  
  output$ui_lang <- renderUI({
    radioButtons("lang", c(GR="Γλώσσα", EN="Language")[lang], 
                 c("Ελληνικά" = "GR", "English" = "EN"))
  })
  
  output$ui_syndromes <- renderUI({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    syndroSelected <- 1
    if (exists("input") && ("syndrome" %in% names(input))) syndroSelected <- input$syndrome
    syndroChoices <- syndroDesc$syndroID
    names(syndroChoices) <- syndroDesc[,lang]
      selectInput("syndrome", label = c(EN="Syndrome", GR="Σύνδρομο")[lang], 
                  choices = syndroChoices, 
                  selected = syndroSelected)
  })
  
  output$ui_camps <- renderUI({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    campChoices <- camps$codecamp
    names(campChoices) <- paste(camps$codecamp, "-", camps[,lang])
    campChoices <- c("all", campChoices)
    names(campChoices)[1] <- c(EN="All camps", GR="Όλα")[lang]
    campSelected <- "all"
    if (exists("input") && ("camp" %in% names(input))) campSelected <- input$camp
    selectInput("camp", label = c(EN="Camp", GR="Καταυλισμός")[lang], 
                choices = campChoices,
                selected = campSelected)
  })
  
  output$ui_rangeDates <- renderUI({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    if (exists("input") && ("date_range" %in% names(input))) rangeDates <- input$date_range
    if (lang=="GR") {
      dateRangeInput("date_range", "Εύρος ημερομηνιών", 
                     start=rangeDates[1], end=rangeDates[2], 
                     min=rangeDates.ini[1], max=rangeDates.ini[2], format="dd-mm-yyyy",
                     lang="el", separator=" έως ")
    } else {
      dateRangeInput("date_range", "Date range", 
                     start=rangeDates[1], end=rangeDates[2], 
                     min=rangeDates.ini[1], max=rangeDates.ini[2], format="dd-mm-yyyy",
                     lang="en")
    }
  })
  
  output$mainPlot <- renderPlot({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    if (!exists("input") || !("camp" %in% names(input))) return()
    if (input$camp=="all") {
      myfit <- subset(fits[[as.integer(input$syndrome)]], dates<=input$date_range[2])
      attr(myfit, "title") <- attr(fits[[as.integer(input$syndrome)]], "title")
    } else {
      myfit <- subset(fitsD[[input$camp]][[as.integer(input$syndrome)]], dates<=input$date_range[2])
      attr(myfit, "title") <- attr(fitsD[[input$camp]][[as.integer(input$syndrome)]], "title")
    }
    plotOne(myfit, goback=as.integer(diff(input$date_range)), lang=lang)
  })
})

shinyApp(ui = ui, server = server)

