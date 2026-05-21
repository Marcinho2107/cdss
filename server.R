server <- function(input, output, session) {
  
  output$vaccines_table <- renderTable(NULL)
  output$diseases_table <- renderTable(NULL)
  output$packing_table <- renderTable(NULL)
}