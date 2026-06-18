library(shiny)
library(shinydashboard)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)
library(DT)
library(httr)
library(htmltools)

countries_file <- "cdc_countries.csv"
recommendations_file <- "cdc_all_countries_recommendations.csv"
diseases_file <- "cdc_all_diseases.csv"
vaccines_file <- "reiseimpfungen.csv"
non_vaccine_file <- "cdc_non_vaccine_diseases_FULL.csv"
packing_file <- "cdc_packing_lists_ALL_COUNTRIES.csv"

countries <- read_csv(countries_file, show_col_types = FALSE)

cdc_recommendations_fallback <- read_csv(
  recommendations_file,
  show_col_types = FALSE
)

vaccines_master <- read_csv(
  vaccines_file,
  show_col_types = FALSE
)

non_vaccine_fallback <- read_csv(
  non_vaccine_file,
  show_col_types = FALSE
)

packing_fallback <- read_csv(
  packing_file,
  show_col_types = FALSE
)

desired_packing_order <- c(
  "Prescription medicines",
  "Medical supplies",
  "Over-the-counter medicines",
  "Supplies to prevent illness or injury",
  "First-aid kit",
  "Documents"
)

make_packing_id <- function(category, item) {
  paste0(
    "pack_",
    digest::digest(
      paste(category, item, sep = "_"),
      algo = "xxhash32"
    )
  )
}

scrape_country_recommendations <- function(country_name) {
  
  country_row <- countries %>%
    filter(country == country_name) %>%
    slice(1)
  
  if (nrow(country_row) == 0) {
    stop("Country not found in cdc_countries.csv.")
  }
  
  url <- country_row$full_url[1]
  
  page <- read_html(url)
  rows <- page %>% html_elements("tr")
  
  result <- map_dfr(rows, function(row) {
    
    disease_node <- row %>% html_element(".clinician-disease")
    recommendation_node <- row %>% html_element(".clinician-recomendations")
    
    if (length(disease_node) == 0 || length(recommendation_node) == 0) {
      return(NULL)
    }
    
    disease <- disease_node %>% html_text2() %>% str_squish()
    recommendation <- recommendation_node %>% html_text2() %>% str_squish()
    
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
    stop("No CDC recommendations found.")
  }
  
  result
}

scrape_non_vaccine_diseases <- function(country_name) {
  
  country_row <- countries %>%
    filter(country == country_name) %>%
    slice(1)
  
  if (nrow(country_row) == 0) {
    stop("Country not found in cdc_countries.csv.")
  }
  
  url <- country_row$full_url[1]
  
  page <- read_html(url)
  
  rows <- page %>%
    html_elements("#non-vaccine-preventable-diseases table.disease tbody tr")
  
  if (length(rows) == 0) {
    return(
      tibble(
        `Disease Name` = character(),
        `Common ways the disease spreads` = character(),
        Advice = character()
      )
    )
  }
  
  result <- map_dfr(rows, function(row) {
    
    disease_node <- row %>% html_element(".other-clinician-disease")
    transmission_node <- row %>% html_element(".other-clinician-notes")
    advice_node <- row %>% html_element(".other-clinician-patienteduction")
    
    disease <- disease_node %>% html_text2() %>% str_squish()
    transmission <- transmission_node %>% html_text2() %>% str_squish()
    advice <- advice_node %>% html_text2() %>% str_squish()
    
    if (is.na(disease) || disease == "") {
      return(NULL)
    }
    
    tibble(
      `Disease Name` = disease,
      `Common ways the disease spreads` = transmission,
      Advice = advice
    )
  })
  
  result
}

scrape_packing_list <- function(country_name) {
  
  country_slug <- country_name %>%
    str_to_lower() %>%
    str_replace_all("&", "and") %>%
    str_replace_all("[^a-z0-9 ]", "") %>%
    str_squish() %>%
    str_replace_all(" ", "-")
  
  url <- paste0(
    "https://wwwnc.cdc.gov/travel/destinations/",
    country_slug,
    "/traveler/packing-list"
  )
  
  response <- GET(
    url,
    user_agent("Mozilla/5.0")
  )
  
  if (status_code(response) != 200) {
    stop("CDC Packing List is not reachable.")
  }
  
  page <- read_html(response)
  
  categories <- page %>%
    html_elements("h4.traveler-text-color")
  
  if (length(categories) == 0) {
    stop("No packing-list categories found.")
  }
  
  result <- map_dfr(categories, function(cat_node) {
    
    cat_name <- cat_node %>%
      html_text2() %>%
      str_squish()
    
    items_list <- cat_node %>%
      html_element(xpath = "following-sibling::ul[1]")
    
    items <- items_list %>%
      html_elements("li")
    
    item_names <- items %>%
      map_chr(function(item_node) {
        
        strong_node <- item_node %>%
          html_element("strong")
        
        if (!is.na(strong_node)) {
          strong_node %>%
            html_text2() %>%
            str_squish()
        } else {
          item_node %>%
            html_text2() %>%
            str_squish()
        }
      })
    
    tibble(
      country = country_name,
      category = cat_name,
      item = item_names
    )
  })
  
  result <- result %>%
    filter(!is.na(item), item != "") %>%
    mutate(
      category = factor(category, levels = desired_packing_order)
    ) %>%
    arrange(category, item) %>%
    mutate(category = as.character(category)) %>%
    select(country, category, item)
  
  if (nrow(result) == 0) {
    stop("No packing-list items found.")
  }
  
  result
}