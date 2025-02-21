
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
    #inv{
      color: white;
      background-color: blue
    }
    #err{
      color:red;
      font-family: Helvetica;
      font-weight:800;
      font-size: 120%
    }
    #now{
      color:purple;
      font-size: 200%;
      font-family: Calibri
    }
    .modal-title{
      font-size: 150%;
      color:orange;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Test Taker",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  
  actionBttn("q1_bttn","Multiplying matrices by vectors"), br(),
  actionBttn("q2_bttn","Dot products"), br(),
  actionBttn("q3_bttn","Eigenvalues, eigenvectors, eigenspaces"), br(),
  actionBttn("q4_bttn", "Orthonormal bases"),
  
  fluidRow(stylesheet,
           column(width=4,
                  
           ),
           column(width = 4,
                  uiOutput("M"),
                  uiOutput("Minv")
           )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "purple") #other colors available


server <- function(session, input, output) {
  #Variables that are shared among server functions (use <<-)
  m <- NULL
  
  #variables to track the user response
  rv_list = reactiveValues(
    q1_count = 0,
    q2_count = 0,
    q3_count = 0,
    q4_count = 0,
    q1_message = "",
    q2_message = "",
    q3_message = "",
    q4_message = ""
    
  )
  
  #Functions that respond to events in the input
  observeEvent(input$q1_bttn, {
    
    showModal(modalDialog(title = "Question 1", id = "q1_modal", footer = NULL, easyClose = T, size = "l",
                          
                          img(src = "Question_1.png", height="100%", width="100%"),
                          
                          textOutput("q1_message_o"),
                          
                          splitLayout(
                            cellWidths = "25%",
                            align = "center",
                            actionButton("q1_a1", img(src = "Question_1_A1.png", width = "100%", height = "100%"), style= "background:white"),
                            actionButton("q1_a2", img(src = "Question_1_A2.png", width = "100%", height = "100%"), style= "background:white"),
                            actionButton("q1_a3", img(src = "Question_1_A3.png", width = "100%", height = "100%"), style= "background:white"),
                            actionButton("q1_a4", img(src = "Question_1_A4.png", width = "100%", height = "100%"), style= "background:white")
                          )
                          
                          
    ))
  })
  
  observeEvent( input$q1_a1 , { 
    
    if(rv_list$q1_count < 2){
      rv_list$q1_message = "Try again!"
      rv_list$q1_count = rv_list$q1_count + 1 
    }
    if(rv_list$q1_count == 2){
      rv_list$q1_message = "The correct answer is B"
    }
  })
  
  observeEvent( input$q1_a3 , { 
    
    if(rv_list$q1_count < 2){
      rv_list$q1_message = "Try again!"
      rv_list$q1_count = rv_list$q1_count + 1 
    }
    if(rv_list$q1_count == 2){
      rv_list$q1_message = "The correct answer is B"
    }
  })
  
  observeEvent( input$q1_a4 , { 
    
    if(rv_list$q1_count < 2){
      rv_list$q1_message = "Try again!"
      rv_list$q1_count = rv_list$q1_count + 1 
    }
    if(rv_list$q1_count == 2){
      rv_list$q1_message = "The correct answer is B"
    }
  })
  
  output$q1_message_o = renderText(rv_list$q1_message)
  
  observeEvent( input$q1_a2 , { 
    
    showModal(modalDialog("Correct!"))
  })
  
  
  
  observeEvent(input$q2_bttn, {
    
    showModal(modalDialog(title = "Question 2", id = "q2_modal", footer = NULL, easyClose = T, size = "l",
                          
                          img(src = "Question_2.png", height="100%", width="100%"),
                          
                          textOutput("q2_message_o"),
                          
                          splitLayout(
                            cellWidths = "25%",
                            align = "center",
                            actionButton("q2_a1", img(src = "Question_2_A1.png", width = "100%", height = "100%"),style= "background:white" ),
                            actionButton("q2_a2", img(src = "Question_2_A2.png", width = "100%", height = "100%"), style= "background:white"),
                            actionButton("q2_a3", img(src = "Question_2_A3.png", width = "100%", height = "100%"), style= "background:white"),
                            actionButton("q2_a4", img(src = "Question_2_A4.png", width = "100%", height = "100%"), style= "background:white")
                          )
                          
                          
    ))
  })
  
  observeEvent( input$q2_a1 , { 
    
    if(rv_list$q2_count < 2){
      rv_list$q2_message = "Try again!"
      rv_list$q2_count = rv_list$q2_count + 1 
    }
    if(rv_list$q2_count == 2){
      rv_list$q2_message = "The correct answer is B"
    }
  })
  
  observeEvent( input$q2_a3 , { 
    
    if(rv_list$q2_count < 2){
      rv_list$q2_message = "Try again!"
      rv_list$q2_count = rv_list$q2_count + 1 
    }
    if(rv_list$q2_count == 2){
      rv_list$q2_message = "The correct answer is B"
    }
  })
  
  observeEvent( input$q2_a4 , { 
    
    if(rv_list$q2_count < 2){
      rv_list$q2_message = "Try again!"
      rv_list$q2_count = rv_list$q2_count + 1 
    }
    if(rv_list$q2_count == 2){
      rv_list$q2_message = "The correct answer is B"
    }
  })
  
  output$q2_message_o = renderText(rv_list$q2_message)
  
  observeEvent( input$q2_a2 , { 
    
    showModal(modalDialog("Correct!"))
  })
  
  observeEvent(input$q3_bttn, {
    
    showModal(modalDialog(title = "Question 3", id = "q3_modal", footer = NULL, easyClose = T, size = "l",
                          
                          img(src = "Question_3.png", height="100%", width="100%"),
                          
                          textOutput("q3_message_o"),
                          
                          splitLayout(
                            cellWidths = "25%",
                            align = "center",
                            actionButton("q3_a1", img(src = "Question_3_A1.png", width = "100%", height = "100%"),style= "background:white" ),
                            actionButton("q3_a2", img(src = "Question_3_A2.png", width = "100%", height = "100%"), style= "background:white"),
                            actionButton("q3_a3", img(src = "Question_3_A3.png", width = "100%", height = "100%"), style= "background:white"),
                            actionButton("q3_a4", img(src = "Question_3_A4.png", width = "100%", height = "100%"), style= "background:white")
                          )
                          
                          
    ))
  })
  
  observeEvent( input$q3_a1 , { 
    
    if(rv_list$q3_count < 2){
      rv_list$q3_message = "Try again!"
      rv_list$q3_count = rv_list$q3_count + 1 
    }
    if(rv_list$q3_count == 2){
      rv_list$q3_message = "The correct answer is B"
    }
  })
  
  observeEvent( input$q3_a3 , { 
    
    if(rv_list$q3_count < 2){
      rv_list$q3_message = "Try again!"
      rv_list$q3_count = rv_list$q3_count + 1 
    }
    if(rv_list$q3_count == 2){
      rv_list$q3_message = "The correct answer is B"
    }
  })
  
  observeEvent( input$q3_a4 , { 
    
    if(rv_list$q3_count < 2){
      rv_list$q3_message = "Try again!"
      rv_list$q3_count = rv_list$q3_count + 1 
    }
    if(rv_list$q3_count == 2){
      rv_list$q3_message = "The correct answer is B"
    }
  })
  
  output$q3_message_o = renderText(rv_list$q3_message)
  
  observeEvent( input$q3_a2 , { 
    
    showModal(modalDialog("Correct!"))
  })
  
  observeEvent(input$q4_bttn, {
    
    showModal(modalDialog(title = "Question 4", id = "q4_modal", footer = NULL, easyClose = T, size = "l",
                          
                          img(src = "Question_4.png", height="100%", width="100%"),
                          
                          textOutput("q4_message_o"),
                          
                          splitLayout(
                            cellWidths = "25%",
                            align = "center",
                            actionButton("q4_a1", img(src = "Question_4_A1.png", width = "100%", height = "100%"),style= "background:white" ),
                            actionButton("q4_a2", img(src = "Question_4_A2.png", width = "100%", height = "100%"), style= "background:white"),
                            actionButton("q4_a3", img(src = "Question_4_A3.png", width = "100%", height = "100%"), style= "background:white"),
                            actionButton("q4_a4", img(src = "Question_4_A4.png", width = "100%", height = "100%"), style= "background:white")
                          )
                          
                          
    ))
  })
  
  observeEvent( input$q4_a1 , { 
    
    if(rv_list$q4_count < 2){
      rv_list$q4_message = "Try again!"
      rv_list$q4_count = rv_list$q4_count + 1 
    }
    if(rv_list$q4_count == 2){
      rv_list$q4_message = "The correct answer is C"
    }
  })
  
  observeEvent( input$q4_a2 , { 
    
    if(rv_list$q4_count < 2){
      rv_list$q4_message = "Try again!"
      rv_list$q4_count = rv_list$q4_count + 1 
    }
    if(rv_list$q4_count == 2){
      rv_list$q4_message = "The correct answer is C"
    }
  })
  
  observeEvent( input$q4_a4 , { 
    
    if(rv_list$q4_count < 2){
      rv_list$q4_message = "Try again!"
      rv_list$q4_count = rv_list$q4_count + 1 
    }
    if(rv_list$q4_count == 2){
      rv_list$q4_message = "The correct answer is C"
    }
  })
  
  output$q4_message_o = renderText(rv_list$q4_message)
  
  observeEvent( input$q4_a3 , { 
    
    showModal(modalDialog("Correct!"))
  })
  
  
}

#Run the app
shinyApp(ui = ui, server = server)
