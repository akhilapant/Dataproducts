library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("Iris Species Prediction"),  
    
    sidebarPanel(
      p(strong("Documentation:",style="color:red"), a("User Help Page",href="https://lmcmahan.shinyapps.io/project/userHelpPage.html")),    
      h3('User Input'),
      numericInput('id1', strong('Sepal.Length (cm)'), 5.1, min = 4.3, max = 7.9, step = 0.1),
      numericInput('id2', strong('Sepal.Width (cm)'), 3.5, min = 2, max = 4.4, step = 0.1),
      numericInput('id3', strong('Petal.Length (cm)'), 1.4, min = 1, max = 6.9, step = 0.1),
      numericInput('id4', strong('Petal.Width (cm)'), 0.2, min = 0.1, max = 2.5, step = 0.1),
      submitButton('Submit')
    ),
    mainPanel(
        h3('Prediction Output'),
        h4('Predicted species'),
        verbatimTextOutput("irisSpeciesPrediction"),
        h4('Predicted species based on this modeled classification tree:'),
        plotOutput('tree')
    )
  )
)

