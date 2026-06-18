dashboardPage(
  
  dashboardHeader(title = "Travel CDSS"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("CDSS", tabName = "cdss", icon = icon("stethoscope")),
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
        
        fluidRow(
          box(
            width = 4,
            title = "Reiseimpfungen auswählen",
            status = "success",
            solidHeader = TRUE,
            
            selectizeInput(
              "selected_vaccines",
              "Impfungen",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Impfung eingeben",
                maxOptions = 1000
              )
            ),
            
            uiOutput("vaccination_dates")
          ),
          
          box(
            width = 4,
            title = "Ausgewählte Impfungen",
            status = "success",
            solidHeader = TRUE,
            tableOutput("vaccines_table")
          ),
          
          box(
            width = 4,
            title = "First Aid / Packing List",
            status = "info",
            solidHeader = TRUE,
            tableOutput("packing_table")
          )
        ),
        
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
        tabName = "info",
        
        fluidRow(
          box(
            width = 12,
            title = "CDSS Informationen",
            status = "primary",
            solidHeader = TRUE,
            
            h4("Travel Medicine CDSS"),
            p("Dieses System basiert auf CDC-Reisedaten."),
            uiOutput("data_source_info")
          )
        )
      )
    )
  )
)