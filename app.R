#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(thematic)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(scales)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),

    # Application title
    titlePanel(title=div(img(src="dnd.png", width="300"), "Probabilidad de obtener un número con múltiple dados")),
    p("Creado por",
      HTML("<a style=color:#ce0000ff;  href='https://jdleongomez.info/es/'>Juan David Leongómez</a>"),
      "· 2021"),
    
    fluidRow(
        column(2,
               tags$h2("Dados", img(src="dice.png", width="80"), img(src="dice.png", width="80")),
               tags$h4("Primer tipo de dado"),
               numericInput(inputId = "Dado1Max",
                            label = "¿Cuántas caras tiene?",
                            min = 1,
                            max = 100,
                            value = 20,
                            step = 1,
                            width = '400px'),
               numericInput(inputId = "Dado1Num",
                            label = "¿Cuántos dados de este tipo son?",
                            min = 1,
                            max = 100,
                            value = 1,
                            step = 1,
                            width = '400px'),
               tags$h4("Segundo tipo de dado"),
               numericInput(inputId = "Dado2Max",
                            label = "¿Cuántas caras tiene?",
                            min = 1,
                            max = 100,
                            value = 6,
                            step = 1,
                            width = '400px'),
               numericInput(inputId = "Dado2Num",
                            label = "¿Cuántos dados de este tipo son?",
                            min = 0,
                            max = 100,
                            value = 1,
                            step = 1,
                            width = '400px'),
               tags$h4("Tercer tipo de dado"),
               numericInput(inputId = "Dado3Max",
                            label = "¿Cuántas caras tiene?",
                            min = 1,
                            max = 100,
                            value = 4,
                            step = 1,
                            width = '400px'),
               numericInput(inputId = "Dado3Num",
                            label = "¿Cuántos dados de este tipo son?",
                            min = 0,
                            max = 100,
                            value = 0,
                            step = 1,
                            width = '400px')
               ),
        column(2,
               tags$h2("Modificador"),
               numericInput(inputId = "modifier",
                            label = "Escribe el modificador (si hay)",
                            min = -100,
                            max = 100,
                            value = 0,
                            step = 1,
                            width = '400px'),
               hr(),
               tags$h2("Número mínimo deseado"),
               numericInput(inputId = "Need",
                            label = "¿Valor mínimo total esperado?",
                            min = 1,
                            max = 100,
                            value = 10,
                            step = 1,
                            width = '400px'),
               hr(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               tags$h4("Número de simulaciones"),
               numericInput(inputId = "reps",
                            label = "No es necesario cambiarlo",
                            min = 1,
                            max = 10000000,
                            value = 500000,
                            step = 1,
                            width = '400px')
               ),
        column(5,
               tags$h1("Distribución de resultados"),
               plotOutput("probPlot"),
               hr(),
               tags$h1("Probabilidad"),
               htmlOutput("probText"),
               htmlOutput("probPerc")))
  )


# Define server logic required to draw a histogram
server <- function(input, output, session){
    #Simulaciones
    datPre <- reactive({
      dats <- data.frame(
        d1 = rowSums(replicate(input$Dado1Num, sample(1:input$Dado1Max, input$reps, replace=T))),
        d2 = ifelse(input$Dado2Num == 0, rep(0, times = reps), rowSums(replicate(input$Dado2Num, sample(1:input$Dado2Max, input$reps, replace=T)))),
        d3 = ifelse(input$Dado3Num == 0, rep(0, times = reps), rowSums(replicate(input$Dado3Num, sample(1:input$Dado3Max, input$reps, replace=T)))))
      return(dats)
      })
    data <- reactive({
      datos <- datPre()
      datos$d2 <- if(input$Dado2Num != 0){
        datos$d2 = rowSums(replicate(input$Dado2Num, sample(1:input$Dado2Max, input$reps, replace=T)))
      }
      datos$d3 <- if(input$Dado3Num != 0){
        datos$d3 = rowSums(replicate(input$Dado3Num, sample(1:input$Dado3Max, input$reps, replace=T)))
      }
      return(datos)
    })
    probPre <- reactive({
      dat <- data()
      conmod <- rowSums(dat) + input$modifier
      return(conmod)
    })
    #Calcular probabilidad por encima del número deseado
    probAbovePre <- reactive({
      prob <- probPre()
      percent(sum(prob > input$Need)/input$reps)}) 

    #Crear histograma
    output$probPlot <- renderPlot({
      prob <- probPre()
      probAbove <- probAbovePre()
      #Histograma de valores
      ggplot() + 
        geom_histogram(aes(prob, y = ..density.., fill = ..density..), bins = max(prob)-min(prob)+1, color = "black", alpha = 0.5) +
        #geom_density(aes(prob, y = ..density..), color = "black", size = 3) +
        scale_fill_gradient(low = "midnightblue", high = "red") +
        scale_x_continuous(breaks = seq(min(prob), max(prob), by = 1)) +
        geom_vline(xintercept = input$Need, size = 2, alpha = 0.5) +
        annotate(geom = "text", 
                 x = input$Need, y = 0.04, vjust = 2, hjust = 1, angle=90,
                 text = element_text(size = 30),
                 label = paste0("Número mínimo deseado", " = ", input$Need)) +
        labs(x = "Suma dados y modificador", y = "Probabilidad", fill = "Probabilidad")
        })
        #Crear texto
    output$probText <- renderText({
      #Mostrar probabilidad de obtener número igual o mayor a
      paste("Al lanzar <font color=\'#FF0000\'><b>",input$Dado1Num,"d", input$Dado1Max,
            ifelse(input$Dado2Num == 0, "", paste(" + ", input$Dado2Num,"d",input$Dado2Max)),
            ifelse(input$Dado3Num == 0, "", paste(" + ", input$Dado3Num,"d",input$Dado3Max)),
            ifelse(input$modifier == 0, "", paste(" + ", input$modifier)),
            "</b></font>, la probabilidad de obtener un valor total de <font color=\'#FF0000\'><b>",
            input$Need, "</b></font> o mayor, es de aproximadamente:")
      })
    output$probPerc <- renderText({
      probAbove <- probAbovePre()
      paste("<font size='20' color='red'>",
        probAbove,
        "</font>"
      )
    })
}

thematic_shiny()
# Run the application 
shinyApp(ui = ui, server = server)
