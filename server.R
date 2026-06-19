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
  
  vaccine_master <- tibble::tribble(
    ~vaccine_pattern, ~doses_required, ~dose_interval_months, ~booster_months,
    "covid", 3, 1, 12,
    "chickenpox|varicella", 2, 1, 0,
    "cholera", 2, 1, 24,
    "diphtheria|tetanus|pertussis|dtap|tdap|td", 4, 2, 120,
    "flu|influenza", 1, 12, 12,
    "hepatitis a", 2, 6, 240,
    "hepatitis b", 3, 1, 0,
    "japanese encephalitis", 2, 1, 12,
    "measles-mumps-rubella|mmr", 2, 1, 0,
    "measles", 2, 1, 0,
    "polio", 4, 2, 120,
    "rabies", 3, 1, 24,
    "shingles|zoster", 2, 2, 0,
    "typhoid", 1, 0, 36,
    "yellow fever", 1, 0, 999
  )
  
  
  add_vaccine_master_data <- function(df) {
    
    df %>%
      rowwise() %>%
      mutate(
        match_row = list(
          vaccine_master %>%
            filter(str_detect(str_to_lower(disease), vaccine_pattern)) %>%
            slice(1)
        ),
        doses_required = ifelse(nrow(match_row) == 0, NA_real_, match_row$doses_required),
        dose_interval_months = ifelse(nrow(match_row) == 0, NA_real_, match_row$dose_interval_months),
        booster_months = ifelse(nrow(match_row) == 0, NA_real_, match_row$booster_months)
      ) %>%
      ungroup() %>%
      select(-match_row)
  }
  
  classify_priority <- function(recommendation) {
    
    rec <- stringr::str_to_lower(recommendation)
    
    dplyr::case_when(
      stringr::str_detect(rec, "required") ~ "required",
      stringr::str_detect(rec, "routine vaccine|routine vaccines") ~ "recommended",
      stringr::str_detect(rec, "recommended for most travelers") ~ "recommended",
      stringr::str_detect(rec, "recommended for unvaccinated travelers") ~ "recommended",
      stringr::str_detect(rec, "some travelers|consider") ~ "consider",
      stringr::str_detect(rec, "not recommended") ~ "not_recommended",
      TRUE ~ "info"
    )
  }
  
  confirmed_country <- eventReactive(input$confirm_country, {
    req(input$country)
    input$country
  })
  
  output$header_destination <- renderUI({
    req(confirmed_country())
    tags$span(
      paste0('Reiseziel: ', confirmed_country(), '')
    )
  })
  
  country_recommendations <- eventReactive(input$confirm_country, {
    req(input$country)
    
    tryCatch({
      scraped <- scrape_country_recommendations(input$country)
      attr(scraped, "source") <- "CDC Web Scraper"
      scraped
    }, error = function(e) {
      fallback <- cdc_recommendations_fallback %>%
        filter(country == input$country)
      
      attr(fallback, "source") <- paste("Fallback CSV:", e$message)
      fallback
    })
  })
  
  non_vaccine_diseases <- eventReactive(input$confirm_country, {
    req(input$country)
    
    tryCatch({
      data <- scrape_non_vaccine_diseases(input$country)
      attr(data, "source") <- "CDC Web Scraper"
      data
    }, error = function(e) {
      fallback <- non_vaccine_fallback %>%
        filter(country == input$country) %>%
        transmute(
          `Disease Name` = disease,
          `Common ways the disease spreads` = transmission,
          Advice = advice
        )
      
      attr(fallback, "source") <- paste("Fallback CSV:", e$message)
      fallback
    })
  })
  
  packing_list <- eventReactive(input$confirm_country, {
    req(input$country)
    
    tryCatch({
      data <- scrape_packing_list(input$country)
      attr(data, "source") <- "CDC Web Scraper"
      data
    }, error = function(e) {
      fallback <- packing_fallback %>%
        filter(country == input$country) %>%
        select(country, category, item)
      
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
      bind_rows(selected_vaccinations(), new_row)
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
    
    tagList(
      p(
        "Die Eingabe personenbezogener Daten ist optional.",
        style = "font-weight:bold; color:#666;"
      ),
      
      textInput(
        "patient_name",
        "Name",
        placeholder = "Name eingeben (optional)"
      ),
      
      textInput(
        "svnr",
        "Sozialversicherungsnummer",
        placeholder = "SVNR eingeben (optional)"
      )
    )
  })
  
  output$diseases_box_title <- renderUI({
    req(confirmed_country())
    paste("CDC Diseases / Recommendations -", confirmed_country())
  })
  
  output$selected_country_text <- renderUI({
    
    req(confirmed_country())
    
    tagList(
      h4("Ausgewähltes Reiseziel:"),
      strong(confirmed_country())
    )
    
  })
  
  output$diseases_table <- renderTable({
    req(country_recommendations())
    
    country_recommendations() %>%
      select(disease, recommendation)
  })
  
  output$non_vaccine_box_title <- renderUI({
    req(confirmed_country())
    paste("Non-Vaccine-Preventable Diseases -", confirmed_country())
  })
  
  output$non_vaccine_table <- renderDT({
    req(non_vaccine_diseases())
    
    datatable(
      non_vaccine_diseases(),
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE
      )
    )
  })
  
  output$packing_box_title <- renderUI({
    req(confirmed_country())
    paste("First Aid / Packing List -", confirmed_country())
  })
  
  output$vaccine_requirements_box_title <- renderUI({
    req(confirmed_country())
    
    paste0('Zusätzliche Impfungen - "', confirmed_country(), '"')
  })
  
  output$vaccine_requirements_table <- renderDT({
    req(country_recommendations())
    
    user_vaccines <- selected_vaccinations() %>%
      rename(
        disease = Impfung,
        vaccination_date = Datum
      ) %>%
      group_by(disease) %>%
      summarise(
        doses_entered = n(),
        last_vaccination_date = max(vaccination_date),
        .groups = "drop"
      )
    
    df <- country_recommendations() %>%
      mutate(
        priority = classify_priority(recommendation)
      ) %>%
      add_vaccine_master_data() %>%
      group_by(disease, priority) %>%
      summarise(
        doses_required = first(doses_required),
        dose_interval_months = first(dose_interval_months),
        .groups = "drop"
      ) %>%
      left_join(user_vaccines, by = "disease") %>%
      mutate(
        doses_entered = ifelse(is.na(doses_entered), 0, doses_entered),
        
        fehlende_dosen = case_when(
          is.na(doses_required) ~ NA_real_,
          doses_entered >= doses_required ~ 0,
          TRUE ~ doses_required - doses_entered
        ),
        
        naechste_dosis_ab = case_when(
          doses_entered == 0 ~ Sys.Date(),
          fehlende_dosen > 0 ~ last_vaccination_date %m+% months(dose_interval_months),
          TRUE ~ as.Date(NA)
        ),
        
        priority_order = case_when(
          priority == "required" ~ 1,
          priority == "recommended" ~ 2,
          priority == "consider" ~ 3,
          TRUE ~ 4
        )
      ) %>%
      filter(is.na(fehlende_dosen) | fehlende_dosen > 0) %>%
      arrange(priority_order, disease) %>%
      select(
        priority_order,
        disease,
        priority,
        doses_required,
        doses_entered,
        fehlende_dosen,
        naechste_dosis_ab
      )
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(0, "asc")),
        columnDefs = list(
          list(
            targets = 0,
            visible = FALSE
          )
        )
      )
    ) %>%
      formatStyle(
        "priority",
        target = "row",
        backgroundColor = styleEqual(
          c("required", "recommended", "consider", "info", "not_recommended"),
          c("#f8d7da", "#fff3cd", "#d1ecf1", "#d1ecf1", "#d1ecf1")
        )
      )
  })
  
  output$packing_checklist <- renderUI({
    req(packing_list())
    
    df <- packing_list() %>%
      filter(!is.na(item), item != "") %>%
      mutate(
        checkbox_id = map2_chr(category, item, make_packing_id)
      )
    
    if (nrow(df) == 0) {
      return(
        p("No packing list data was found for this destination.")
      )
    }
    
    categories <- unique(df$category)
    
    tagList(
      lapply(categories, function(cat) {
        
        items_cat <- df %>% filter(category == cat)
        
        tagList(
          h4(cat, style = "color:#0073b7; margin-top:20px; font-weight:bold;"),
          
          lapply(seq_len(nrow(items_cat)), function(i) {
            checkboxInput(
              inputId = items_cat$checkbox_id[i],
              label = items_cat$item[i],
              value = FALSE
            )
          })
        )
      })
    )
  })
  
  get_packing_status <- reactive({
    req(packing_list())
    
    df <- packing_list() %>%
      filter(!is.na(item), item != "") %>%
      mutate(
        checkbox_id = map2_chr(category, item, make_packing_id),
        available = map_lgl(checkbox_id, ~ isTRUE(input[[.x]]))
      )
    
    df
  })
  
  output$download_packing_list <- downloadHandler(
    
    filename = function() {
      paste0(
        "Travel_Health_Kit_",
        str_replace_all(confirmed_country(), " ", "_"),
        ".html"
      )
    },
    
    content = function(file) {
      req(confirmed_country())
      
      df <- get_packing_status()
      
      available <- df %>%
        filter(available == TRUE)
      
      missing <- df %>%
        filter(available == FALSE)
      
      make_section <- function(data, title) {
        if (nrow(data) == 0) {
          return(paste0("<h2>", title, "</h2><p>No entries.</p>"))
        }
        
        html <- paste0("<h2>", title, "</h2>")
        
        for (cat in unique(data$category)) {
          items <- data %>%
            filter(category == cat) %>%
            pull(item)
          
          html <- paste0(
            html,
            "<h3>", htmlEscape(cat), "</h3>",
            "<ul>",
            paste0("<li>", htmlEscape(items), "</li>", collapse = ""),
            "</ul>"
          )
        }
        
        html
      }
      
      html <- paste0(
        "<!DOCTYPE html>",
        "<html>",
        "<head>",
        "<meta charset='UTF-8'>",
        "<title>Travel Health Kit</title>",
        "<style>",
        "body { font-family: Arial, sans-serif; margin: 35px; color: #222; }",
        "h1 { color: #0073b7; border-bottom: 3px solid #0073b7; padding-bottom: 10px; }",
        "h2 { color: #333; margin-top: 30px; }",
        "h3 { color: #0073b7; margin-bottom: 5px; }",
        "li { margin-bottom: 6px; }",
        ".note { background: #f3f8fc; border-left: 4px solid #0073b7; padding: 10px; margin: 20px 0; }",
        "@media print { button { display: none; } }",
        "</style>",
        "</head>",
        "<body>",
        "<h1>Travel Health Kit - ", htmlEscape(confirmed_country()), "</h1>",
        "<div class='note'>",
        "This list is based on the CDC Packing List. Checked items are already available at home; unchecked items should still be obtained.",
        "</div>",
        make_section(missing, "Items Still Needed"),
        make_section(available, "Already Available"),
        "<br><br>",
        "<button onclick='window.print()'>Print / Save as PDF</button>",
        "</body>",
        "</html>"
      )
      
      writeLines(html, file, useBytes = TRUE)
    }
  )
  
  output$personal_data_box <- renderUI({
    req(confirmed_country())
    
    box(
      width = 8,
      title = "Personenbezogene Daten",
      status = "primary",
      solidHeader = TRUE,
      uiOutput("cdss_output")
    )
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
      p(paste("Impfungen geladen:", nrow(vaccines_master))),
      p(paste("Non-vaccine fallback rows loaded:", nrow(non_vaccine_fallback))),
      p(paste("Packing-list fallback rows loaded:", nrow(packing_fallback)))
    )
  })
}