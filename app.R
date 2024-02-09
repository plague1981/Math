library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
source('word_questions.R')
ui <- dashboardPage(

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
                                  box(width = 10, title = 'Environment settings',
                                      selectInput(inputId = 'add_digits', label = 'How many digits?', choices = c(1,2,3), selected = 1),
                                      actionButton(inputId = 'set', label = 'set',class='btn')
                                  ),
                                  box(width = 10, title = 'Addition',
                                      uiOutput(outputId = 'add_equation'),
                                      textInput(inputId = 'add_ans',label = 'Answer'),
                                      actionButton(inputId = 'add_submit', label = 'Submit',class='btn'),
                                      textOutput(outputId = 'add_reply')
                                  ),
                                  box(width = 10, title = 'Substraction',
                                      uiOutput(outputId = 'sub_equation'),
                                      textInput(inputId = 'sub_ans',label = 'Answer'),
                                      actionButton(inputId = 'sub_submit', label = 'Submit',class='btn'),
                                      textOutput(outputId = 'sub_reply')
                                  ),
                                  box(width = 10, title = 'Multiplying',
                                      uiOutput(outputId = 'mul_equation'),
                                      textInput(inputId = 'mul_ans',label = 'Answer'),
                                      actionButton(inputId = 'mul_submit', label = 'Submit',class='btn'),
                                      textOutput(outputId = 'mul_reply')
                                  ),
                                  box(width = 10, title = 'Dividing',
                                      uiOutput(outputId = 'div_equation'),
                                      textInput(inputId = 'div_ans',label = 'Answer'),
                                      actionButton(inputId = 'div_submit', label = 'Submit',class='btn'),
                                      textOutput(outputId = 'div_reply')
                                  ),
                                  box(width = 10, title = 'Fraction',
                                      plotOutput(outputId = 'fra_piechart'),
                                      uiOutput(outputId = 'fra_equation'),
                                      actionButton(inputId = 'fra_submit', label = 'Submit',class='btn'),
                                      textOutput(outputId = 'fra_reply')
                                  ),
                                  box(width = 10, title = 'Word questions',
                                      uiOutput(outputId = 'w_q_a'),
                                      textInput(inputId = 'w_q_a_ans',label = 'Answer'),
                                      actionButton(inputId = 'w_q_a_submit', label = 'Submit',class='btn'),
                                      textOutput(outputId = 'w_q_a_reply'),
                                      uiOutput(outputId = 'w_q_c'),
                                      textInput(inputId = 'w_q_c_ans',label = 'Answer'),
                                      actionButton(inputId = 'w_q_c_submit', label = 'Submit',class='btn'),
                                      textOutput(outputId = 'w_q_c_reply')
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
  numbers<- eventReactive(input$set,{
    if (input$add_digits==1){
      fst<-sample(0:9,1)
      sec<-sample(0:9,1)
      trd<-sample(0:9,1)
    } else if (input$add_digits==2){
      fst<-sample(0:99,1)
      sec<-sample(0:99,1)
      trd<-sample(0:99,1)
    } else {
      fst<-sample(0:999,1)
      sec<-sample(0:999,1)
      trd<-sample(0:999,1)
    }
    c(fst,sec,trd)
  })
  coins<- eventReactive(input$set,{
    one_dollar<- sample(0:9,1)
    quarter<- sample(0:9,1)
    dime<- sample(0:9,1)
    nickel<- sample(0:9,1)
    cent<- sample(0:9,1)
    c(one_dollar, quarter, dime, nickel, cent)
  })
# Addition
  output$add_equation<-renderUI({
    return(paste(numbers()[1],'+',numbers()[2],'=?'))
  })
  output$add_reply<-eventReactive(input$add_submit,{
    if(input$add_ans==(numbers()[1]+numbers()[2])){
      return("That is correct!!!")
    } else return("Sorry! Please try again")
  })
# Substraction
  output$sub_equation<-renderUI({
    if (numbers()[1] >= numbers()[2]){
      return(paste(numbers()[1],'-',numbers()[2],'=?'))
    } else return(paste(numbers()[2],'-',numbers()[1],'=?'))
    
  })
  output$sub_reply<-eventReactive(input$sub_submit,{
    if(input$sub_ans==(abs(numbers()[1]-numbers()[2]))){
      return("That is correct!!!")
    } else return("Sorry! Please try again")
  })
# Multiply
  output$mul_equation<-renderUI({
    return(paste(numbers()[1],'x',numbers()[2],'=?'))
    
  })
  output$mul_reply<-eventReactive(input$mul_submit,{
    if(input$mul_ans==(numbers()[1]*numbers()[2])){
      return("That is correct!!!")
    } else return("Sorry! Please try again")
  })
# Dividing (positive integer only)
  
  output$div_equation<-renderUI({
    denominator <- numbers()[1]
    numerator <- denominator*numbers()[2]
    return(paste0(numerator,'/',denominator,'=?'))
  })
  output$div_reply<-eventReactive(input$div_submit,{
    
    if(input$div_ans==numbers()[2]){
      return("That is correct!!!")
    } else return("Sorry! Please try again")
  })
  
# Fraction
  output$fra_equation<-renderUI({
    if (numbers()[1] >= numbers()[2]){
      ans <- paste0(numbers()[2],'/',numbers()[1])
    } else ans <- paste0(numbers()[1],'/',numbers()[2])
    
    n <- 0
    ans_w<-NULL
    repeat {
      n <- n + 1
      a <- sort(sample(1:10,2),decreasing = TRUE)
      b <- paste0(a[2],'/',a[1])
      ans_w<-c(ans_w,b)
      if (n == 2){
        ans_s <-sample(c(ans_w, ans), 3 ,replace = FALSE)
        if (anyDuplicated(ans_s)){
          next
        } else break
      }
    }
    checkboxGroupInput(inputId = "fra_equation", inline = TRUE, label = "Please select one to represent A!", choices = ans_s, selected = NULL)
  })
  
  output$fra_piechart <- renderPlot({
    data <- data.frame(
      group=LETTERS[1:(abs(numbers()[1]-numbers()[2])+1)],
      value=c(min(c(numbers()[1],numbers()[2])),rep(1,abs(numbers()[1]-numbers()[2])))
    )
    
    # Basic piechart
    ggplot(data, aes(x="", y=value, fill=group)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0)
  })
  output$fra_reply <- eventReactive(input$fra_submit,{
    if (length(input$fra_equation)>1){
      return("Please select one answer!")
    } else if(input$fra_equation %in% c(paste0(numbers()[1],'/',numbers()[2]),paste0(numbers()[2],'/',numbers()[1]))){
      return("That is correct!!!")
    } else return("Sorry! Please try again")
  })
  output$w_q_a<-renderUI({
    word_q_add(numbers()[1],numbers()[2],numbers()[3])
  })
  output$w_q_c<-renderUI({
    q<-word_q_coin(coins()[1],coins()[2],coins()[3],coins()[4],coins()[5])
    return(q[[1]])
           
    #word_q_coin(dollar=coins()[1], quarter=coins()[2], dime=coins()[3], nickel=coins()[4], cent=coins()[5])
  })
  output$w_q_c_reply <- eventReactive(input$w_q_c_submit,{
    
    if (input$w_q_c_ans==word_q_coin(coins()[1],coins()[2],coins()[3],coins()[4],coins()[5])[[2]]){
      return("That is correct!!!")
    } else return("Sorry! Please try again")
  })
}



shinyApp(ui, server)

