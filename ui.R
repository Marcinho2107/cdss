#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)



# ---------------------------------------------------------------
# UI — Dashboard Layout
# ---------------------------------------------------------------

shinyDashboardStructure <- a("Shiny Dashboard Structure", href="https://rstudio.github.io/shinydashboard/structure.html")
shinyTutorial <- a("Shiny Tutorial", href="https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/")
icons <- a("Icon-Browser", href="https://fontawesome.com")

dashboardPage(
  dashboardHeader(title = "Antibiotika Entscheidungshilfe"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Eingabe", tabName = "eingabe", icon = icon("stethoscope")),
      menuItem("Infos", tabName = "infos", icon = icon("circle-info"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # ---------------- CDSS-Tab ----------------
      tabItem(tabName = "eingabe",
              h2("Patientendaten eingeben"),
              
              numericInput("age", "Alter (Jahre):", min = 0, max = 120, value = 35),
              numericInput("temp", "Temperatur (°C):", min = 34, max = 42, value = 37.8),
              numericInput("crp", "CRP (mg/L):", min = 0, max = 300, value = 15),
              #numericInput("duration", "Dauer Symptome (Tage):", min = 0, max = 30, value = 3),
              sliderInput("duration", "Dauer Symptome (Tage):", min = 0, max = 30, value = 3),
              
              checkboxGroupInput("symptoms", "Symptome:",
                                 choices = c("Husten", "Halsschmerzen", "Ohrenschmerzen", "Atemnot")),
              
              checkboxInput("risk", "Risikofaktoren vorhanden?", FALSE),
              
              actionButton("calculate", "Empfehlung berechnen", icon = icon("play")),
              
              h2("Empfehlung"),
              verbatimTextOutput("resultText"),
              uiOutput("ampelBox")
      ),
      
      # ---------------- Info-Tab ----------------
      tabItem(tabName = "infos",
              h2("Links and Infos"),
              fluidRow(
                # A static valueBox
                box( title = "Link", shinyDashboardStructure),
                box(  title = "Link", shinyTutorial),
                box(  title = "Link", icons)
                
                
              )
      )
      
    )
  )
)
