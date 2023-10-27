library(shiny)
library(stringr)
library(svDialogs)

#GET COLLEGE SCORECARD DATA
temp <- tempfile()
download.file("https://ed-public-download.app.cloud.gov/downloads/Most-Recent-Cohorts-Field-of-Study_09262023.zip", temp)
sc_data <- read.table(unz(temp,"Most-Recent-Cohorts-Field-of-Study.csv"), header=T, quote="\"", sep=",")
unlink(temp)

#LIMIT TO BACHELOR'S DEGREE LEVEL WITH ALL NECESSARY FIELDS AVAILABLE FOR DEBT AND INCOME
sc <- subset(sc_data, sc_data$CREDLEV=='3' & sc_data$DEBT_ALL_STGP_EVAL_MEAN!='PrivacySuppressed' &
               sc_data$DEBT_ALL_STGP_EVAL_MDN10YRPAY!='PrivacySuppressed' & sc_data$EARN_MDN_1YR!='PrivacySuppressed' &
               sc_data$EARN_MDN_4YR!='PrivacySuppressed')
sc <- sc[,c(1:11,16,75,124,130)]

#CONVERT VARIABLES AS NEEDED
sc$OPEID6 <- padzero(sc$OPEID6,6)
sc$CIPDESC <- str_sub(sc$CIPDESC, end = -2)
sc$DEBT_ALL_STGP_EVAL_MEAN <- as.numeric(sc$DEBT_ALL_STGP_EVAL_MEAN)
sc$DEBT_ALL_STGP_EVAL_MDN10YRPAY <- as.numeric(sc$DEBT_ALL_STGP_EVAL_MDN10YRPAY)
sc$EARN_MDN_1YR <- as.numeric(sc$EARN_MDN_1YR)
sc$EARN_MDN_4YR <- as.numeric(sc$EARN_MDN_4YR)

#CALCULATE AVERAGE PERCENTAGE INCREASE IN INCOME FROM YEAR 1 TO YEAR 4 AND ASSUME YEAR 4 THROUGH YEAR 10 INCREASE IS HALF THAT RATE
sc$EARN_MDN_10YR_PROJECTED <- round(sc$EARN_MDN_4YR*((1+((sc$EARN_MDN_4YR/sc$EARN_MDN_1YR)^(1/3)-1)/2)^6), 0)
sc$EARN_MDN_10YR_PROJECTED <- ifelse(sc$EARN_MDN_4YR<sc$EARN_MDN_1YR, sc$EARN_MDN_4YR, sc$EARN_MDN_10YR_PROJECTED)

sc_display <- sc[, c(3,7,12:16)]
colnames(sc_display) <- c("Institution","Major","Avg Debt","Avg Payment","Salary 1 Yr","Salary 4 Yr","Projected Salary 10 Yr")

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
