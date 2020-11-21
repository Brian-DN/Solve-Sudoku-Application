library(shiny)
library(shinyMatrix)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Solve Sudoku Application"),
    sidebarLayout(
        sidebarPanel(
            helpText("Fill in the numeric in the matrix below. Note that, enter 0 in the blank fields."),
            matrixInput("df","What sudoku do you want to solve?",
                        value = matrix(rep(0,81),9,9),
                        class = "numeric"),
            submitButton("Submit")
            ),
        mainPanel(
            h5("Note: The harder the sudoku, the longer it will take to solve. Otherwise, it's no longer than one minute."),
            tableOutput("solve")
        )
    )
))