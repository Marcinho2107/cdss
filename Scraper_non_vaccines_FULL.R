# =========================================================
# CDC Non-Vaccine-Preventable Diseases Scraper (FULL RUN)
# =========================================================

library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)

# 0. PFAD SETZEN
my_path <- "C:/Users/Marco/OneDrive - FH JOANNEUM/Desktop/CDSS"
setwd(my_path)

# 1. Basis-Setup
base_url <- "https://wwwnc.cdc.gov"
list_url <- "https://wwwnc.cdc.gov/travel/destinations/list"

# 2. Länderliste holen
cat("Hole Länderliste von CDC...\n")
page <- read_html(list_url)
country_data <- tibble(
  href = page %>% html_elements("a") %>% html_attr("href")
) %>%
  filter(str_detect(href, "^/travel/destinations/traveler/none/")) %>%
  distinct(href) %>%
  mutate(
    full_url = paste0(base_url, href),
    country = href %>% str_remove("^/travel/destinations/traveler/none/") %>% 
      str_replace_all("-", " ") %>% tools::toTitleCase()
  )

# 3. Die spezialisierte Scraper-Funktion
scrape_nvp <- function(country, url) {
  cat("Scraping:", country, "...\n")
  Sys.sleep(0.5) # Höflichkeits-Pause für den Server
  
  tryCatch({
    page <- read_html(url)
    
    # Selektiert die Tabellenzeilen im spezifischen Container
    rows <- page %>% 
      html_elements("#non-vaccine-preventable-diseases table.disease tbody tr")
    
    if (length(rows) == 0) return(NULL)
    
    result <- map_dfr(rows, function(row) {
      disease_node      <- row %>% html_element(".other-clinician-disease")
      transmission_node <- row %>% html_element(".other-clinician-notes")
      advice_node       <- row %>% html_element(".other-clinician-patienteduction")
      
      # Validierung: Nur Zeilen mit Inhalten verarbeiten
      if (is.na(html_text(disease_node))) return(NULL)
      
      tibble(
        country = country,
        disease = disease_node %>% html_text2() %>% str_squish(),
        transmission = transmission_node %>% html_text2() %>% str_squish(),
        advice = advice_node %>% html_text2() %>% str_squish(),
        source_url = url
      )
    })
    return(result)
    
  }, error = function(e) {
    message("Fehler bei ", country, ": ", e$message)
    return(NULL)
  })
}

# =========================================================
# 4. Ausführung (ALLE LÄNDER)
# =========================================================

cat("Starte Scraping für alle Länder. Bitte warten...\n")

all_nvp_data <- map2_dfr(
  country_data$country, 
  country_data$full_url, 
  scrape_nvp
)

# =========================================================
# 5. Speichern & Abschluss
# =========================================================

if (exists("all_nvp_data") && nrow(all_nvp_data) > 0) {
  final_filename <- "cdc_non_vaccine_diseases_FULL.csv"
  write_csv(all_nvp_data, final_filename)
  
  cat("\n=========================================================\n")
  cat("FERTIG!\n")
  cat("Anzahl Länder gescrapt:", length(unique(all_nvp_data$country)), "\n")
  cat("Anzahl Datensätze insgesamt:", nrow(all_nvp_data), "\n")
  cat("Datei gespeichert unter:", paste0(my_path, "/", final_filename), "\n")
  cat("=========================================================\n")
} else {
  cat("\nFehler: Es konnten keine Daten extrahiert werden.\n")
}