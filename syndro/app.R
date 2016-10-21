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
      tabPanel(c(EN="Table", GR="Πίνακας")[lang], tableOutput("mainTable")),
      tabPanel(c(EN="By camp", GR="Ανά κέντρο")[lang], 
               uiOutput("ui_campTable"),
               tableOutput("campTable")
      )
    )
    do.call(tabsetPanel, c(mytabs, id="mytbp"))
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
  
  output$mainTable <- renderTable({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    if (!exists("input") || !("camp" %in% names(input))) return()
    if (input$camp=="all") {
      myfit <- subset(fits[[as.integer(input$syndrome)]], dates<=input$date_range[2])
      attr(myfit, "title") <- attr(fits[[as.integer(input$syndrome)]], "title")
    } else {
      myfit <- subset(fitsD[[input$camp]][[as.integer(input$syndrome)]], dates<=input$date_range[2])
      attr(myfit, "title") <- attr(fitsD[[input$camp]][[as.integer(input$syndrome)]], "title")
    }
    #myfit$excess <- with(myfit, pmax(0,round(x-Pnb)))
    myfit <- myfit[,c("dates","x","n","p","zscore","alerts","alarms")]
    myfit$dates <- as.character(myfit$dates)
    myfit$alerts <- with(myfit, c("","NAI")[alerts+1])
    myfit$alarms <- with(myfit, c("","NAI")[alarms+1])
    myfit$x <- as.integer(myfit$x)
    myfit$n <- as.integer(myfit$n)
    if (lang=="GR") {
      names(myfit) <- c("Ημ/νία", "Περιστατικά", "Σύνολο επισκέψεων", "Αναλογική νοσηρότητα", "Z-score", "Ειδοποίηση", "Εγρήγορση")
    } else {
      names(myfit) <- c("Date", "Cases", "Total\nvisits", "Proportional\nmorbidity", "Z-score", "Warning", "Alert")
    }
    return(myfit)
  }, align="lcccccc", striped=TRUE, digits=3)

  output$ui_campTable <- renderUI({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    p(c(EN="Date: ", GR="Ημερομηνία: ")[lang], strong(format(input$date_range[2], "%d-%m-%Y")), 
      em(c(EN="(upper limit of date range)", GR="(άνω όριο εύρους ημερομηνιών). ")[lang], style="font-size:0.8em"),
      br(), syndroDesc[input$syndrome,lang],
      style="font-size:1.2em")
  })

  output$campTable <- renderTable({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    myfit <- lapply(names(fitsD), function(cmp){
      f <- subset(fitsD[[cmp]][[as.integer(input$syndrome)]], dates==input$date_range[2])
      if (nrow(f)!=0) {
        f$camp <- camps[cmp,lang]
      }
      return(f)
    })
    myfit <- do.call(rbind, myfit)
    #myfit$excess <- with(myfit, pmax(0,round(x-Pnb)))
    myfit <- myfit[,c("camp","x","n","p","zscore","alerts","alarms")]
    myfit$alerts <- with(myfit, c("","NAI")[alerts+1])
    myfit$alarms <- with(myfit, c("","NAI")[alarms+1])
    myfit$x <- as.integer(myfit$x)
    myfit$n <- as.integer(myfit$n)
    myfit <- subset(myfit, !is.na(n))
    
    if (lang=="GR") {
      names(myfit) <- c("Κέντρο", "Περιστατικά", "Σύνολο επισκέψεων", "Αναλογική νοσηρότητα", "Z-score", "Ειδοποίηση", "Εγρήγορση")
    } else {
      names(myfit) <- c("Camp", "Cases", "Total\nvisits", "Proportional\nmorbidity", "Z-score", "Warning", "Alert")
    }
    return(myfit)
    
  }, align="lcccccc", striped=TRUE, digits=3)
  

})

shinyApp(ui = ui, server = server)

