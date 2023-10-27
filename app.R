library(shiny)
library(svDialogs)

#GET SAVED COLLEGE SCORECARD DATA
sc_display <- read.csv("./scdata.csv")
colnames(sc_display) <- c("Institution","Major","Avg Debt","Avg Payment","Salary 1 Yr","Salary 4 Yr","Projected Salary 10 Yr")

#CREATE LIST OF MAJORS FOR DROP-DOWN BOX
major_list <- sc_display[!duplicated(sc_display$Major), ]
major_list <- sort(major_list[,2])

#DEFINE UI
ui <- fluidPage(
  
  tags$head(
    tags$style(
      "td:nth-child(2) {width: 50px}",
      "td:nth-child(3) {width: 10px}",
      "td:nth-child(4) {width: 20px}",
      "td:nth-child(5) {width: 20px}",
      "td:nth-child(6) {width: 40px}"
    )
  ),
  
  #APP TITLE
  titlePanel("Student Debt and Future Income by Undergraduate Major"),
  
  #SIDEBAR LAYOUT FOR INPUTS AND OUTPUTS
  sidebarLayout(
    
    #SIDEBAR PANELS FOR INPUTS
    sidebarPanel(
      #INPUT-SELECT A MAJOR
      selectInput(inputId = "major",
                  label = "Choose a major:",
                  choices = major_list),
      
      #INPUT-SELECT A SORT VARIABLE
      selectInput(inputId = "sorter",
                  label = "Sort variable:",
                  choices = colnames(sc_display)[3:ncol(sc_display)],
                  selected=colnames(sc_display)[5]),
      
      #INPUT-SELECT SORT ORDER
      selectInput(inputId="sort_order", 
                  label="Select sort order:", 
                  choices = c("Ascending", "Descending"),
                  selected='Descending'),
      
      #INPUT-NUMBER OF OBSERVATIONS TO DISPLAY
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
    ),
    
    #MAIN PANEL TO DISPLAY OUTPUTS
    mainPanel(
      tableOutput("view")
    )
  )
)

#DEFINE SERVER
server <- function(input, output) {
  #OUTPUT FIRST N ROWS, ALIGN ACCORDING TO ASSIGNED VALUE
  output$view <- renderTable({
    if (input$sort_order=='Ascending') {
      sc_display <- sc_display[order(sc_display[, input$sorter], decreasing = FALSE), ] 
    } else {
      sc_display <- sc_display[order(sc_display[, input$sorter], decreasing = TRUE), ] 
    }
    sc_display$`Avg Debt` <- paste0("$", formatC(sc_display$`Avg Debt`, big.mark=",", format="d", digits=0))
    sc_display$`Avg Payment` <- paste0("$", formatC(sc_display$`Avg Payment`, big.mark=",", format="d", digits=0))
    sc_display$`Salary 1 Yr` <- paste0("$", formatC(sc_display$`Salary 1 Yr`, big.mark=",", format="d", digits=0))
    sc_display$`Salary 4 Yr` <- paste0("$", formatC(sc_display$`Salary 4 Yr`, big.mark=",", format="d", digits=0))
    sc_display$`Projected Salary 10 Yr` <- paste0("$", formatC(sc_display$`Projected Salary 10 Yr`, big.mark=",", format="d", digits=0))
    sc_display[, input$sorter] <- paste0("<b>", sc_display[, input$sorter], "</b>")
    
    head(sc_display[which(sc_display$Major==input$major), -2], n=input$obs)},
    sanitize.text.function = function(x) x,
    align='lrrrrr'
  )
}

#CREATE SHINY APP
shinyApp(ui = ui, server = server)
