library(shiny)

shinyUI(fluidPage(
  tabsetPanel(              
    tabPanel(title = "kNN",
             sidebarPanel(
               numericInput('x1', 'Petal.Length', 2.5,
                            min = 1, max = 7),
               numericInput('y1', 'Petal.Width', 1.5,
                            min = 0, max = 2.5),
               sliderInput("k1",
                           "K value",
                           min = 1,  max = 149, value = 6),
               actionButton("kNNCl", "Classificate Object"),
               actionButton("kNNMap", "Classification Map")
             ),
             mainPanel(
               plotOutput('plot1')
             )
    ),
    tabPanel(title = "wkNN",
             sidebarPanel(
               numericInput('x2', 'Petal.Length', 2.5,
                            min = 1, max = 7),
               numericInput('y2', 'Petal.Width', 1.5,
                            min = 0, max = 2.5),
               sliderInput("k2",
                           "K value",
                           min = 1,  max = 149, value = 6),
               actionButton("wkNNCl", "Classificate Object"),
               actionButton("wkNNMap", "Classification Map")
             ),
             mainPanel(
               plotOutput('plot2')
             )
    ),
    tabPanel(title = "Parzen Window",
             sidebarPanel(
               selectInput(inputId = "Kernel",
                           label = "Select Kernel:",
                           choices = c("Krectangle", "Kepanchnikov", "Ktriangle", "Kgauss", "Kkvartich"),
                           selected = "Krectangle"),
               numericInput('x3', 'Petal.Length', 2.5,
                            min = 1, max = 7),
               numericInput('y3', 'Petal.Width', 1.5,
                            min = 0, max = 2.5),
               numericInput('h', 'h', 1,
                            min = 0, max = 3),
               actionButton("ParzenCl", "Classificate Object"),
               actionButton("ParzenMap", "Classification Map")
             ),
             mainPanel(
               plotOutput('plot3')
             )
    ),
    tabPanel(title = "Parzen Window with temp h",
             sidebarPanel(
               selectInput(inputId = "Kernel_2",
                           label = "Select Kernel:",
                           choices = c("Krectangle", "Kepanchnikov", "Ktriangle", "Kgauss", "Kkvartich"),
                           selected = "Krectangle"),
               numericInput('x4', 'Petal.Length', 2.5,
                            min = 1, max = 7),
               numericInput('y4', 'Petal.Width', 1.5,
                            min = 0, max = 2.5),
               sliderInput('k', 'k', min = 0, max = 50, value = 6),
               h4("h"),
               verbatimTextOutput("hval"),
               actionButton("ParzenTempHCl", "Classificate Object"),
               actionButton("ParzenTempHMap", "Classification Map")
             ),
             mainPanel(
               plotOutput('plot4')
             )
    ),
    tabPanel(title = "Potential functions",
             sidebarPanel(
               selectInput(inputId = "Kernel_potential",
                           label = "Select Kernel:",
                           choices = c("Krectangle", "Kepanchnikov", "Ktriangle", "Kgauss", "Kkvartich"),
                           selected = "Krectangle"),
               numericInput('x5', 'Petal.Length', 2.5,
                            min = 1, max = 7),
               numericInput('y5', 'Petal.Width', 1.5,
                            min = 0, max = 2.5),
               numericInput('h_potential', 'h', 1,
                            min = 0, max = 3),
               actionButton("PotencialCl", "Classificate Object"),
               actionButton("PotencialMap", "Classification Map")
             ),
             mainPanel(
               plotOutput('plot5')
             )
    )
  ))
)
