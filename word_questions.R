# ========= 2nd grader
# Addition and Subtraction

word_q_add<- function(a=NULL, b=NULL, c=NULL){
  question<-c(
    paste('Their plastic Christmas tree comes in 3 parts. The bottom part has', a, 'branches. The middle part has', b, 'branches, and the top part has', c, 'branches. How many branches are there in total?')
    ,paste('There were some marshmallows in th bag. Jack ate', a,'. David ate', b,'. There are', c, 'marshmallows left. How many marshmallows were in the bag before Jac and David ate some?')
      )
  return(question[sample(1:length(question),1)])
}

word_q_coin<- function(dollar=NULL, quarter=NULL, dime=NULL, nickel=NULL, cent=NULL){
  questions <-list(c(
    paste('Lucas has', dollar, 'one-dollar bills,', dime, 'dimes and', nickel ,'nickels. How much money does Lucas have?'),
    dollar+0.1*dime+0.05*nickel))
  return(questions)
}
#, quarter, 'quarters,'
word_q_sub<- function(a=NULL, b=NULL, c=NULL){
  questions<-c(
    paste('Jemima Puddle-Duck had', a, '. She bought a bag of corn for', b, '.How much money did she have left?'),
    paste('Mr. Martin made', a, 'cups of macaroni and cheese for the school lunch. There were', b, 'cups left after lunch.How many cups of macaroni and cheese were eaten?')
  )
  return(questions[sample(1:length(questions),1)])
}

a<-function(){
  return(list('a','b'))
}



