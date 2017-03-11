library(shiny)

shinyUI(fluidPage(
  tabsetPanel(     
    tabPanel(title = "Linear classifiers",
             sidebarPanel(
               sliderInput("obj4",
                           "The number of objects of each class",
                           min = 10,  max = 200, value = 100),
               textInput("sigma1_all", "Coefficients for sigma1", "2,0,0,10"),
               textInput("mu1_all", "Coefficients for mu1", "0,0"),
               textInput("sigma2_all", "Coefficients for sigma2", "4,1,1,2"),
               textInput("mu2_all", "Coefficients for mu2", "10,-10")
             ),
             mainPanel(
               plotOutput('plot4')
             )
    ),
    tabPanel(title = "ADALINE",
             sidebarPanel(
               sliderInput("obj1",
                           "The number of objects of each class",
                           min = 10,  max = 200, value = 100),
               textInput("sigma1_adaline", "Coefficients for sigma1", "2,0,0,10"),
               textInput("mu1_adaline", "Coefficients for mu1", "0,0"),
               textInput("sigma2_adaline", "Coefficients for sigma2", "4,1,1,2"),
               textInput("mu2_adaline", "Coefficients for mu2", "10,-10")
             ),
             mainPanel(
               plotOutput('plot1')
             )
    ),
    tabPanel(title = "Hebb's rule",
             sidebarPanel(
               sliderInput("obj2",
                           "The number of objects of each class",
                           min = 10,  max = 200, value = 100),
               textInput("sigma1_hebb", "Coefficients for sigma1", "2,0,0,10"),
               textInput("mu1_hebb", "Coefficients for mu1", "0,0"),
               textInput("sigma2_hebb", "Coefficients for sigma2", "4,1,1,2"),
               textInput("mu2_hebb", "Coefficients for mu2", "10,-10")
             ),
             mainPanel(
               plotOutput('plot2')
             )
    ),
    tabPanel(title = "Logistic regression",
             sidebarPanel(
               sliderInput("obj3",
                           "The number of objects of each class",
                           min = 10,  max = 200, value = 100),
               textInput("sigma1_regr", "Coefficients for sigma1", "2,0,0,10"),
               textInput("mu1_regr", "Coefficients for mu1", "0,0"),
               textInput("sigma2_regr", "Coefficients for sigma2", "4,1,1,2"),
               textInput("mu2_regr", "Coefficients for mu2", "10,-10")
             ),
             mainPanel(
               plotOutput('plot3')
             )
    ),
    tabPanel(title = "SVM",
             sidebarPanel(
               sliderInput("obj5",
                           "The number of objects of each class",
                           min = 10,  max = 200, value = 100),
               textInput("sigma1_svm", "Coefficients for sigma1", "2,0,0,10"),
               textInput("mu1_svm", "Coefficients for mu1", "0,0"),
               textInput("sigma2_svm", "Coefficients for sigma2", "4,1,1,2"),
               textInput("mu2_svm", "Coefficients for mu2", "10,-10"),
               checkboxInput("do2", "ROC curve", value = F)
             ),
             mainPanel(
               plotOutput('plot5'),
               plotOutput('plot6'),
               plotOutput('plot7')
             )
    )
    
    
    
  ))
)
