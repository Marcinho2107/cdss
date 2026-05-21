library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title = "Travel CDSS"),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("CDSS", tabName = "cdss", icon = icon("stethoscope")),
      menuItem("Infos", tabName = "info", icon = icon("book"))
      
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # =========================
      # CDSS TAB
      # =========================
      tabItem(tabName = "cdss",
              
              fluidRow(
                
                box(
                  width = 4,
                  title = "Landauswahl",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  selectizeInput(
                    "country",
                    "Reiseziel",
                    choices = NULL,
                    options = list(
                      placeholder = "Land eingeben (z.B. Aus → Austria)",
                      maxOptions = 10
                    )
                  ),
                  
                  actionButton(
                    "confirm_country",
                    "Land bestätigen",
                    icon = icon("check"),
                    class = "btn-primary"
                  )
                ),
                
                box(
                  width = 8,
                  title = "CDSS Output",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  h4("Bitte Land auswählen und bestätigen")
                )
              ),
              
              fluidRow(
                
                box(
                  width = 4,
                  title = "Reiseimpfungen",
                  status = "success",
                  solidHeader = TRUE,
                  tableOutput("vaccines_table")
                ),
                
                box(
                  width = 4,
                  title = "Nicht-impfbare Krankheiten",
                  status = "warning",
                  solidHeader = TRUE,
                  tableOutput("diseases_table")
                ),
                
                box(
                  width = 4,
                  title = "First Aid / Packing List",
                  status = "info",
                  solidHeader = TRUE,
                  tableOutput("packing_table")
                )
              )
      ),
      
      # =========================
      # INFO TAB
      # =========================
      tabItem(tabName = "info",
              
              fluidRow(
                
                box(
                  width = 12,
                  title = "CDSS Informationen",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  h4("Travel Medicine CDSS"),
                  p("Dieses System basiert auf CDC-Reisedaten und unterstützt klinische Entscheidungen."),
                  
                  tags$ul(
                    tags$li("Reiseimpfungen nach CDC"),
                    tags$li("Nicht impfpräventable Krankheiten"),
                    tags$li("First Aid & Packing Empfehlungen")
                  )
                )
              ),
              
              fluidRow(
                
                box(
                  width = 12,
                  title = "Datenquellen",
                  status = "info",
                  solidHeader = TRUE,
                  
                  p("CDC Scraper Module werden nach Landauswahl geladen.")
                )
              )
      )
    )
  )
)