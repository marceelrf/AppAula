library(shiny)
library(lorem)
library(bslib)
library(tidyverse)



ui <- fluidPage(
  h1("Seja bem-vindo"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      h2("Entrada"),
      numericInput(inputId = "salario",
                   label = "Qual seu salario?",
                   value = 2000,
                   min = 1100,
                   max = 5000,
                   step = 200)
      ),
    mainPanel(
      h2("Principal"),
      h3("Salario do usuario:"),
      textOutput(outputId = "user_salario"),
      textOutput(outputId = "faixa_salario")
    )
  )

)

server <- function(input, output, session) {

  # output$resposta <- renderText({
  #
  #   Sys.sleep(.5)
  #
  #   paste0("Olar ", input$nome_usuario, ". Seja muito bem vindo!")
  # })

  output$user_salario <- renderText({

    if(input$salario > 5000) {

      return(paste0("O valor máxio de salário é R$ 5000.\nEntre com um valor apropriado."))
    }

    input$salario
  })


  output$faixa_salario <- renderText({
    if(input$salario > 4000) {

      return(paste0("Faixa A"))
    }

    if(input$salario > 2000) {
      return(paste0("Faixa B"))
    }

    return(paste0("Faixa C"))

  })

}

shinyApp(ui, server)
