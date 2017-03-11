library(shiny)

shinyUI(fluidPage(
  tabsetPanel(              
    tabPanel(title = "Naive Bayes",
             sidebarPanel(
               #actionButton("NaiveMap", "Classificaion Map")
               tags$blockquote("Naive Bayes classifier is  a simple probabilistic classifier based on applying Bayes' theorem with strong (naive) independence assumptions between the features.")
               ),
             mainPanel(
               plotOutput('plot1')
             )
    ),
    tabPanel(title = "Plugin",
             sidebarPanel(
               sliderInput("obj1",
                           "The number of objects of each class",
                           min = 10,  max = 200, value = 100),
               textInput("sigma1_plugin", "Coefficients for sigma1", "10,0,0,1"),
               textInput("mu1_plugin", "Coefficients for mu1", "1,0"),
               textInput("sigma2_plugin", "Coefficients for sigma2", "1,0,0,5"),
               textInput("mu2_plugin", "Coefficients for mu2", "15,0")
             ),
             mainPanel(
               plotOutput('plot2')
             )
    ),
    tabPanel(title = "LDF",
             sidebarPanel(
               sliderInput("obj2",
                           "The number of objects of each class",
                           min = 10,  max = 200, value = 100),
               textInput("sigma_LDF", "Coefficients for sigmas", "2,0,0,2"),
               textInput("mu1_LDF", "Coefficients for mu1", "1,0"),
               textInput("mu2_LDF", "Coefficients for mu2", "15,0")
             ),
             mainPanel(
               plotOutput('plot3')
             )
    ),
    tabPanel(title = "The level lines of normal distribution",
             sidebarPanel(
               sliderInput("obj3",
                           "The number of objects of each class",
                           min = 10,  max = 200, value = 100),
               textInput("sigma1_levels", "Coefficients for sigma1", "10,0,0,1"),
               textInput("mu1_levels", "Coefficients for mu1", "1,0"),
               textInput("sigma2_levels", "Coefficients for sigma2", "1,0,0,5"),
               textInput("mu2_levels", "Coefficients for mu2", "15,0")
             ),
             mainPanel(
               plotOutput('plot4')
             )
    ),
    tabPanel(title = "EM algorithm",
             sidebarPanel(
               sliderInput("obj4",
                           "The number of objects of each class",
                           min = 10,  max = 200, value = 100),
               textInput("sigma1_em", "Coefficients for sigma1", "10,0,0,1"),
               textInput("mu1_em", "Coefficients for mu1", "1,0"),
               textInput("sigma2_em", "Coefficients for sigma2", "1,0,0,5"),
               textInput("mu2_em", "Coefficients for mu2", "15,0"),
               numericInput('eps', 'eps', 0.1,
                            min = 0, max = 1),
               h4("iterations"),
               verbatimTextOutput("iterations")
             ),
             mainPanel(
               plotOutput('plot5')
             )
    )
    
  ))
)
