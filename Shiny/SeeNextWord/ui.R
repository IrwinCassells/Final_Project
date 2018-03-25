library(shiny)


shinyUI
(
    pageWithSidebar
    (
        headerPanel
            (
                "Let's try and predict the next word in a sentence:"
            ),
        sidebarPanel
            (
                h3("Enter sentence here:"),
                textInput(inputId = "userSen",label = "Enter here:",value = "Try this"),
                width = 4,
                h3("Click here to predict:"),
                submitButton(text = "Predict!")
            ),
  
        mainPanel
            (
                h3("The next word is:"),
                textOutput(outputId = "nextWord"),
                h3("Other words:"),
                textOutput(outputId = "otherWord")
            )
    )
)
