dashboardPage(
  
  dashboardHeader(
    title = "Travel CDSS",
    
    tags$li(
      class = "dropdown",
      style = "
      position:absolute;
      left:50%;
      transform:translateX(-50%);
      top:15px;
      color:white;
      font-size:18px;
      list-style:none;
      font-weight:bold;
    ",
      uiOutput("header_destination", inline = TRUE)
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("CDSS", tabName = "cdss", icon = icon("stethoscope")),
      menuItem("CDC Diseases / Recommendations", tabName = "diseases", icon = icon("virus")),
      menuItem("Non-Vaccine-Preventable Diseases", tabName = "non_vaccine", icon = icon("briefcase-medical")),
      menuItem("First Aid / Packing List", tabName = "packing", icon = icon("kit-medical")),
      menuItem("Information / Update", tabName = "info", icon = icon("book"))
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
    
      /* Sidebar breiter */
      .main-sidebar {
        width: 250px !important;
      }

      .content-wrapper,
      .right-side,
      .main-footer {
        margin-left: 250px !important;
      }

      /* Sidebar Schrift */
      .sidebar-menu > li > a {
        white-space: normal !important;
        font-size: 13px;
      }

      /* Box Design */
      .box {
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }

      .box-header {
        border-top-left-radius: 8px;
        border-top-right-radius: 8px;
      }

      /* Buttons */
      .btn {
        border-radius: 6px;
        font-weight: 600;
      }

      /* Eingabefelder */
      .form-control {
        border-radius: 6px;
      }

      /* CDC Info Box */
      .info-note {
        background: #f3f8fc;
        border-left: 4px solid #0073b7;
        padding: 10px;
        margin-bottom: 15px;
      }

      /* Success */
      .success-note {
        color: #008d4c;
        font-weight: bold;
      }

      /* Error */
      .error-note {
        color: #dd4b39;
        font-weight: bold;
      }

      /* Tabellen schöner */
      table {
        font-size: 14px;
      }

      /* Header */
      .main-header .logo {
        font-weight: bold;
      }

    "))
    ),
    
    tabItems(
      
      tabItem(
        tabName = "cdss",
        fluidRow(
          box(
            width = 4,
            title = "Destination Selection",
            status = "primary",
            solidHeader = TRUE,
            selectizeInput(
              "country",
              "Travel Destination",
              choices = NULL,
              selected = NULL,
              options = list(
                placeholder = "Enter destination",
                maxOptions = 1000
              )
            ),
            actionButton(
              "confirm_country",
              "Confirm Destination",
              icon = icon("check"),
              class = "btn-primary"
            ),
            br(), br(),
            uiOutput("selected_country_text")
          ),
          
          uiOutput("personal_data_box")
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
        tabName = "non_vaccine",
        fluidRow(
          box(
            width = 12,
            title = uiOutput("non_vaccine_box_title"),
            status = "primary",
            solidHeader = TRUE,
            DTOutput("non_vaccine_table")
          )
        )
      ),
      
      tabItem(
        tabName = "packing",
        fluidRow(
          box(
            width = 12,
            title = uiOutput("packing_box_title"),
            status = "info",
            solidHeader = TRUE,
            div(
              class = "info-note",
              "Check all items that the traveler already has at home."
            ),
            uiOutput("packing_checklist"),
            br(),
            downloadButton(
              "download_packing_list",
              "Generate Travel Health Kit",
              class = "btn-primary"
            )
          )
        )
      ),
      
      tabItem(
        tabName = "info",
        fluidRow(
          box(
            width = 12,
            title = "CDSS Information",
            status = "primary",
            solidHeader = TRUE,
            h4("Travel Medicine CDSS"),
            p("This system is based on CDC travel data."),
            uiOutput("data_source_info"),
            br(),
            actionButton(
              "update_backup_csv",
              "Update Backup CSVs",
              icon = icon("sync"),
              class = "btn-warning"
            ),
            br(), br(),
            uiOutput("update_status")
          )
        )
      )
    )
  )
)