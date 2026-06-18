server <- function(input, output, session) {
  
  updateSelectizeInput(
    session,
    "country",
    choices = countries$country,
    server = TRUE
  )
  
  confirmed_country <- eventReactive(input$confirm_country, {
    req(input$country)
    input$country
  })
  
  country_recommendations <- eventReactive(input$confirm_country, {
    
    req(input$country)
    
    tryCatch({
      
      scraped <- scrape_country_recommendations(input$country)
      attr(scraped, "source") <- "CDC Webscraper"
      scraped
      
    }, error = function(e) {
      
      fallback <- cdc_recommendations_fallback %>%
        filter(country == input$country)
      
      attr(fallback, "source") <- paste("Fallback CSV:", e$message)
      fallback
    })
    
  })
  
  observeEvent(country_recommendations(), {
    
    recs <- country_recommendations()
    
    available_vaccines <- recs %>%
      pull(disease) %>%
      unique() %>%
      sort()
    
    updateSelectizeInput(
      session,
      "selected_vaccines",
      choices = available_vaccines,
      selected = character(0),
      server = TRUE
    )
    
  })
  
  selected_vaccine_info <- reactive({
    req(input$selected_vaccines)
    
    tibble(
      Impfung = input$selected_vaccines
    )
  })
  
  output$cdss_output <- renderUI({
    req(confirmed_country())
    
    recs <- country_recommendations()
    
    tagList(
      h4("Ausgewähltes Reiseziel:"),
      strong(confirmed_country()),
      br(),
      br(),
      p(paste("Anzahl geladener CDC-Empfehlungen:", nrow(recs))),
      p(paste("Datenquelle:", attr(recs, "source")))
    )
  })
  
  output$diseases_table <- renderTable({
    req(country_recommendations())
    country_recommendations()
  })
  
  output$vaccines_table <- renderTable({
    req(selected_vaccine_info())
    selected_vaccine_info()
  })
  
  output$vaccination_dates <- renderUI({
    req(input$selected_vaccines)
    
    lapply(input$selected_vaccines, function(vac) {
      dateInput(
        inputId = paste0("date_", make.names(vac)),
        label = paste("Datum für", vac),
        value = Sys.Date()
      )
    })
  })
  
  output$packing_table <- renderTable({
    data.frame(
      Kategorie = c("Medikamente", "Mückenschutz", "Dokumente"),
      Empfehlung = c(
        "Reiseapotheke mitführen",
        "Repellent und Moskitonetz",
        "Impfausweis / Versicherung"
      )
    )
  })
  
  output$diseases_box_title <- renderUI({
    req(confirmed_country())
    paste("CDC Diseases / Recommendations -", confirmed_country())
  })
  
  output$diseases_table <- renderTable({
    req(country_recommendations())
    
    country_recommendations() %>%
      select(disease, recommendation)
  })
  
  output$data_source_info <- renderUI({
    tagList(
      p("Aktuelle Datenquelle: CDC Webscraper mit CSV-Fallback"),
      p(paste("Länder geladen:", nrow(countries))),
      p(paste("Fallback-Empfehlungen geladen:", nrow(cdc_recommendations_fallback))),
      p(paste("Impfungen geladen:", nrow(vaccines_master)))
    )
  })
}