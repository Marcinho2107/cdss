server <- function(input, output, session) {
  
  updateSelectizeInput(
    session,
    "country",
    choices = countries$country,
    server = TRUE
  )
  
  selected_vaccinations <- reactiveVal(
    tibble(
      Impfung = character(),
      Datum = as.Date(character())
    )
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
      filter(!is.na(disease), disease != "") %>%
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
    
    selected_vaccinations(
      tibble(
        Impfung = character(),
        Datum = as.Date(character())
      )
    )
  })
  
  observeEvent(input$add_vaccine, {
    
    req(input$selected_vaccines)
    req(input$vaccination_date)
    
    new_row <- tibble(
      Impfung = input$selected_vaccines,
      Datum = as.Date(input$vaccination_date)
    )
    
    selected_vaccinations(
      bind_rows(selected_vaccinations(), new_row) %>%
        distinct(Impfung, .keep_all = TRUE)
    )
    
    updateSelectizeInput(
      session,
      "selected_vaccines",
      selected = character(0)
    )
  })
  
  observeEvent(input$delete_vaccine_row, {
    
    row_to_delete <- input$delete_vaccine_row
    df <- selected_vaccinations()
    
    if (nrow(df) >= row_to_delete) {
      selected_vaccinations(df[-row_to_delete, ])
    }
  })
  
  output$country_dependent_ui <- renderUI({
    req(confirmed_country())
    
    tagList(
      fluidRow(
        
        box(
          width = 4,
          title = "Reiseimpfung auswählen",
          status = "success",
          solidHeader = TRUE,
          
          selectizeInput(
            "selected_vaccines",
            "Impfung",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              placeholder = "Impfung eingeben",
              maxOptions = 1000
            )
          ),
          
          dateInput(
            "vaccination_date",
            "Impfdatum",
            value = Sys.Date()
          ),
          
          actionButton(
            "add_vaccine",
            "Impfung hinzufügen",
            icon = icon("plus"),
            class = "btn-success"
          )
        ),
        
        box(
          width = 8,
          title = "Ausgewählte Impfungen",
          status = "success",
          solidHeader = TRUE,
          DTOutput("vaccines_table")
        )
      )
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
  
  output$diseases_box_title <- renderUI({
    req(confirmed_country())
    paste("CDC Diseases / Recommendations -", confirmed_country())
  })
  
  output$diseases_table <- renderTable({
    req(country_recommendations())
    
    country_recommendations() %>%
      select(disease, recommendation)
  })
  
  output$vaccines_table <- renderDT({
    
    df <- selected_vaccinations()
    
    if (nrow(df) == 0) {
      return(
        datatable(
          df,
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }
    
    df <- df %>%
      mutate(
        Datum = format(Datum, "%d.%m.%Y"),
        Löschen = sprintf(
          '<button class="btn btn-danger btn-xs" onclick="Shiny.setInputValue(\'delete_vaccine_row\', %d, {priority: \'event\'})">❌</button>',
          seq_len(n())
        )
      )
    
    datatable(
      df,
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE
      )
    )
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
  
  observeEvent(input$update_backup_csv, {
    
    output$update_status <- renderUI({
      p("Update läuft... bitte warten.")
    })
    
    tryCatch({
      
      updated_data <- load_cdc_data()
      
      countries <<- updated_data$countries
      cdc_recommendations_fallback <<- updated_data$recommendations
      
      updateSelectizeInput(
        session,
        "country",
        choices = countries$country,
        selected = NULL,
        server = TRUE
      )
      
      output$update_status <- renderUI({
        tagList(
          strong("Backup CSVs erfolgreich aktualisiert."),
          br(),
          p(paste("Länder geladen:", nrow(updated_data$countries))),
          p(paste("Empfehlungen geladen:", nrow(updated_data$recommendations))),
          p(paste("Diseases geladen:", nrow(updated_data$diseases)))
        )
      })
      
    }, error = function(e) {
      
      output$update_status <- renderUI({
        tagList(
          strong("Update fehlgeschlagen."),
          br(),
          p(e$message)
        )
      })
    })
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