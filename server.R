#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)

# ---------------------------------------------------------------
# Entscheidungslogik — regelbasiertes Modell
# ---------------------------------------------------------------
decision_logic <- function(age, temp, crp, duration, symptoms, risk) {
  
  score <- 0
  
  # CRP Bewertung
  if (crp < 20) score <- score + 0
  else if (crp < 100) score <- score + 1
  else score <- score + 2
  
  # Fieber
  if (temp >= 38.5) score <- score + 1
  
  # Dauer der Symptome
  if (duration >= 7) score <- score + 1
  
  # Anzahl Symptome
  score <- score + length(symptoms) * 0.25
  
  # Risikofaktoren
  if (risk) score <- score + 1
  
  return(score)
}

interpretation_logic <- function(score){
  # Finales Ergebnis
  if (score <= 1) {
    return("Keine Antibiotika empfohlen.")
  } else if (score <= 2.5) {
    return("Beobachten / erneute Untersuchung empfohlen.")
  } else {
    return("Antibiotika wahrscheinlich sinnvoll.")
  }
}


# ---------------------------------------------------------------
# Server — Berechnung & Ausgabe
# ---------------------------------------------------------------
##################################
## server - variante 1 mit reactive
function(input, output) {
  result <- reactive({
    
    decision_logic(
      age = input$age,
      temp = input$temp,
      crp  = input$crp,
      duration = input$duration,
      symptoms = input$symptoms,
      risk = input$risk
    )
  })
  
  # Text-Ausgabe
  output$resultText <- renderText(interpretation_logic(result()))
  
  # AmpelBox
  output$ampelBox <- renderUI({
    
    
    
    color <- if (result() <= 1) "green" 
    else if (result() <= 2.5) "orange" 
    else "red"
    
    
    
    box(
      width = 6,
      background = color,
      h3(result())
    )
  })
  system("git --version")
  
}

#########################################
## server variante 2 mit ObserveEvent und Button
# var2 <- function(input, output) {
#   observeEvent(input$calculate, {
#   # observeEvent(c(input$age,input$temp, input$crp, input$duration,input$symptoms,input$risk), {
#     result <- decision_logic(
#       age = input$age,
#       temp = input$temp,
#       crp  = input$crp,
#       duration = input$duration,
#       symptoms = input$symptoms,
#       risk = input$risk
#     )
# 
# 
#     # Text-Ausgabe
#     output$resultText <- renderText(interpretation_logic(result))
# 
#     # AmpelBox
#     output$ampelBox <- renderUI({
#       # req(input$calculate)
# 
# 
# 
# 
#       color <- if (result <= 1) "green"
#       else if (result <= 2.5) "orange"
#       else "red"
# 
# 
#       box(
#         width = 6,
#         background = color,
#         h3(paste("score: ",result))
#       )
#     })
# 
#   })
# 
# }

