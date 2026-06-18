dashboardPage(
  
  dashboardHeader(title = "Travel CDSS"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("CDSS", tabName = "cdss", icon = icon("stethoscope")),
      menuItem("CDC Diseases / Recommendations", tabName = "diseases", icon = icon("virus")),
      menuItem("First Aid / Packing List", tabName = "packing", icon = icon("briefcase-medical")),
      menuItem("Infos", tabName = "info", icon = icon("book"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "cdss",
        
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
              selected = NULL,
              options = list(
                placeholder = "Land eingeben",
                maxOptions = 1000
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
            uiOutput("cdss_output")
          )
          
        ),
        
        uiOutput("country_dependent_ui")
        
      ),
      
      tabItem(
        tabName = "diseases",
        
        fluidRow(
          box(
            width = 12,
            title = uiOutput("diseases_box_title"),
            status = "warning",
            solidHeader = TRUE,
            tableOutput("diseases_table")
          )
        )
      ),
      
      tabItem(
        tabName = "packing",
        
        fluidRow(
          box(
            width = 12,
            title = "First Aid / Packing List",
            status = "info",
            solidHeader = TRUE,
            tableOutput("packing_table")
          )
        )
      ),
      
      tabItem(
        tabName = "info",
        
        fluidRow(
          
          box(
            width = 12,
            title = "CDSS Informationen",
            status = "primary",
            solidHeader = TRUE,
            
            h4("Travel Medicine CDSS"),
            p("Dieses System basiert auf CDC-Reisedaten."),
            uiOutput("data_source_info"),
            
            br(),
            
            actionButton(
              "update_backup_csv",
              "Backup CSVs aktualisieren",
              icon = icon("sync"),
              class = "btn-warning"
            ),
            
            br(),
            br(),
            
            uiOutput("update_status")
            
          )
          
        )
      )
      
    )
    
  )
  
)