library(shiny)
library(shinydashboard)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)
library(DT)

countries_file <- "cdc_countries.csv"
recommendations_file <- "cdc_all_countries_recommendations.csv"
diseases_file <- "cdc_all_diseases.csv"
vaccines_file <- "reiseimpfungen.csv"

load_cdc_data <- function() {
  
  base_url <- "https://wwwnc.cdc.gov"
  list_url <- "https://wwwnc.cdc.gov/travel/destinations/list"
  
  page <- read_html(list_url)
  
  links <- page %>%
    html_elements("a")
  
  country_data <- tibble(
    href = links %>% html_attr("href")
  ) %>%
    filter(
      !is.na(href),
      str_detect(href, "^/travel/destinations/traveler/none/")
    ) %>%
    distinct(href) %>%
    mutate(
      full_url = paste0(base_url, href),
      country = href %>%
        str_remove("^/travel/destinations/traveler/none/") %>%
        str_replace_all("-", " ") %>%
        tools::toTitleCase()
    ) %>%
    select(country, href, full_url)
  
  scrape_country <- function(country, url) {
    
    cat("Scraping:", country, "\n")
    Sys.sleep(0.5)
    
    tryCatch({
      
      page <- read_html(url)
      rows <- page %>% html_elements("tr")
      
      map_dfr(rows, function(row) {
        
        disease_node <- row %>%
          html_element(".clinician-disease")
        
        recommendation_node <- row %>%
          html_element(".clinician-recomendations")
        
        if (length(disease_node) == 0 || length(recommendation_node) == 0) {
          return(NULL)
        }
        
        disease <- disease_node %>%
          html_text2() %>%
          str_squish()
        
        recommendation <- recommendation_node %>%
          html_text2() %>%
          str_squish()
        
        if (str_to_lower(disease) == "routine vaccines") {
          
          vaccines <- recommendation_node %>%
            html_elements("li") %>%
            html_text2() %>%
            str_squish()
          
          vaccines <- vaccines[vaccines != ""]
          
          if (length(vaccines) > 0) {
            return(
              tibble(
                country = country,
                disease = vaccines,
                recommendation = "Routine Vaccine",
                source_url = url
              )
            )
          }
        }
        
        if (disease == "" || recommendation == "") {
          return(NULL)
        }
        
        tibble(
          country = country,
          disease = disease,
          recommendation = recommendation,
          source_url = url
        )
      })
      
    }, error = function(e) {
      tibble(
        country = country,
        disease = NA_character_,
        recommendation = NA_character_,
        source_url = url
      )
    })
  }
  
  all_data <- map2_dfr(
    country_data$country,
    country_data$full_url,
    scrape_country
  ) %>%
    filter(!is.na(disease))
  
  all_diseases <- all_data %>%
    select(disease) %>%
    distinct() %>%
    arrange(disease)
  
  write_excel_csv(country_data, countries_file)
  write_excel_csv(all_data, recommendations_file)
  write_excel_csv(all_diseases, diseases_file)
  
  list(
    countries = country_data,
    recommendations = all_data,
    diseases = all_diseases,
    source = "CDC Scraper"
  )
}

countries <- read_csv(countries_file, show_col_types = FALSE)

cdc_recommendations_fallback <- read_csv(
  recommendations_file,
  show_col_types = FALSE
)

vaccines_master <- read_csv(
  vaccines_file,
  show_col_types = FALSE
)

scrape_country_recommendations <- function(country_name) {
  
  country_row <- countries %>%
    filter(country == country_name) %>%
    slice(1)
  
  if (nrow(country_row) == 0) {
    stop("Land nicht in cdc_countries.csv gefunden.")
  }
  
  url <- country_row$full_url[1]
  
  page <- read_html(url)
  rows <- page %>% html_elements("tr")
  
  result <- map_dfr(rows, function(row) {
    
    disease_node <- row %>%
      html_element(".clinician-disease")
    
    recommendation_node <- row %>%
      html_element(".clinician-recomendations")
    
    if (length(disease_node) == 0 || length(recommendation_node) == 0) {
      return(NULL)
    }
    
    disease <- disease_node %>%
      html_text2() %>%
      str_squish()
    
    recommendation <- recommendation_node %>%
      html_text2() %>%
      str_squish()
    
    if (str_to_lower(disease) == "routine vaccines") {
      
      vaccines <- recommendation_node %>%
        html_elements("li") %>%
        html_text2() %>%
        str_squish()
      
      vaccines <- vaccines[vaccines != ""]
      
      if (length(vaccines) > 0) {
        return(
          tibble(
            country = country_name,
            disease = vaccines,
            recommendation = "Routine Vaccine",
            source_url = url
          )
        )
      }
    }
    
    if (disease == "" || recommendation == "") {
      return(NULL)
    }
    
    tibble(
      country = country_name,
      disease = disease,
      recommendation = recommendation,
      source_url = url
    )
  })
  
  if (nrow(result) == 0) {
    stop("Keine CDC-Empfehlungen gefunden.")
  }
  
  result
}