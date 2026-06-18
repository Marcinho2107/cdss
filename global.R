library(shiny)
library(shinydashboard)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)

countries_file <- "cdc_countries.csv"
recommendations_file <- "cdc_all_countries_recommendations.csv"
vaccines_file <- "reiseimpfungen.csv"

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