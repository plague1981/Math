library(shiny)
library(shinythemes)
library(shinydashboard)

ui <- dashboardPage(
  
  # App title ----
  dashboardHeader(title = "Math exercise!"),
  
  dashboardSidebar(  
    sidebarMenu(
      menuItem(text = 'PRES', tabName = 'PRES', icon = icon("school")),
      menuItem(text = 'Middle', tabName = 'Middle', icon = icon('school-flag')),
      menuItem(text = 'High', tabName = 'High', icon = icon('graduation-cap'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'PRES',
              navbarPage(title = 'PRES',
                         tabPanel('1st', icon = icon('dice-one'),
                                  box(width = 10
                                    
                                  )
                         ),
                         tabPanel('2nd', icon = icon('dice-two'),
                                  box(width = 10, title = 'Addition',
                                      selectInput(inputId = 'digits', label = 'How many digits?', choices = c(1,2,3), selected = 1),
                                      actionButton(inputId = 'generate', label = 'Generate',class='btn'),
                                      textOutput(outputId = 'fst_number'),
                                      textOutput(outputId = 'sec_number'),
                                      textInput(inputId = 'add_ans'),
                                      actionButton(inputId = 'add_submit', label = 'Generate',class='btn'),
                                      textOutput(outputId = 'add_reply')
                                  ),
                                  box(width = 10, title = 'Substraction'
                                      
                                  ),
                                  box(width = 10, title = 'Multiplying'
                                      
                                  ),
                                  box(width = 10, title = 'Dividing'
                                      
                                  )
                         ),
                         tabPanel('3rd', icon = icon('dice-three'),
                                  box(width = 10
                                      
                                  )
                         ),
                         tabPanel('4th', icon = icon('dice-four'),
                                  box(width = 10
                                      
                                  )
                         ),
                         tabPanel('5th', icon = icon('dice-five'),
                                  box(width = 10
                                      
                                  )
                         )
              )
      ),
      tabItem(tabName = "Middle"
        
      ),
      tabItem(tabName = "High"
        
      )
    )
 
  )
)

server <- function(input, output,session) {
  output$fst_number <- eventReactive(input$generate,{
    sample(1:100,1)
  })
  output$sec_number <- eventReactive(input$generate,{
    sample(1:100,1)
  })

  
}



addition <- function(){
  correct <- 0
  test<- 0
  test_total <- 2

  repeat{
    a <- sample(0:100,1)
    b <- sample(0:100,1)
  
    ans <-  readline(paste0("What is the answer of", a, '+' ,b, "="))
    if (a+b == ans){
        correct <- correct + 1
        print ('That is correct')
    } else print ('That is wrong')

    test <- test +1 
    print(paste('You have finished',test,'questions.'))
    print(paste0(correct,'/',test_total,' are right'))
    
    if (test == test_total){
      print(paste('Well done! Your score is', correct/test_total*100))
      break
    } 
  }
}

shinyApp(ui, server)
