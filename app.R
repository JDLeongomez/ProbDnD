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
library(fontawesome)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),

    # Application title
    titlePanel(
      title = tags$link(rel = "icon", 
                        type = "image/gif", 
                        href = "https://image.pngaaa.com/393/402393-middle.png"),
      "Probalilidad Dados DnD"),
    HTML("<center><img src='dnd.png'' width='200'></center>"),
    p(HTML("<center>Cálculo de la probabilidad de obtener un número con múltiples dados, 
           calculado a partir de simulaciones</center>")),
    p(HTML("<center>Código disponible en
      <a style=color:#ce0000ff;  href='https://github.com/JDLeongomez/ProbDnD'>GitHub</a>
      - Creado por 
      <a style=color:#ce0000ff;  href='https://jdleongomez.info/es/'>Juan David Leongómez</a>
      · 2021</center>")),
    p(),
    hr(),
    fluidRow(
        column(3,
               tags$h2("Dados", img(src="dice.png", width="80"), img(src="dice.png", width="80")),
               tags$h4("Primer tipo de dado (obligatorio)"),
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
               tags$h4("Segundo tipo de dado (opcional)"),
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
               tags$h4("Tercer tipo de dado (opcional)"),
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
        column(3,
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
               tags$p(HTML("<b style=color:#ce0000ff;>NOTA: </b>"), 
                      " Para que el histograma de la simulación se vea bien, el", 
                      HTML("<b>  valor mínimo total esperado</b>"),
                      "debe estar dentro de las posibilidades de la suma 
                      de los dados lanzados y el modificador. Por ejemplo, al lanzar",
                      HTML("<b> 1d20 + 3</b>"), "el resultado solo puede estar entre 4 y 23.
                      Si seleccionas un", HTML("<b>  valor mínimo total esperado</b>"), 
                      "menor a 4 o mayor a 23, el histograma se verá extraño, aunque la",
                      HTML("<b>  probabilidad</b>"), 
                      "(mostrada más abajo) será correcta."),
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
        d2 = rep(0, times = input$reps),
        d3 = rep(0, times = input$reps))
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
      percent(sum(prob >= input$Need)/input$reps)}) 

    #Crear histograma
    output$probPlot <- renderPlot({
      prob <- probPre()
      probAbove <- probAbovePre()
      #Histograma de valores
      ggplot() +
        annotate("rect", xmin = input$Need, xmax =  Inf, ymin = 0, ymax = Inf, 
                 alpha = 0.4, fill = "white") +
        geom_histogram(aes(prob, y = ..density.., fill = ..density..), bins = max(prob)-min(prob)+1, color = "black", alpha = 0.5) +
        #geom_density(aes(prob, y = ..density..), size = 0.5, adjust = 4) +
        scale_fill_gradient(low = "midnightblue", high = "red") +
        scale_x_continuous(breaks = seq(min(prob), max(prob), by = 1)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        geom_vline(xintercept = input$Need, size = 2, alpha = 0.2) +
        annotate(geom = "text", 
                 x = input$Need, y = -Inf, vjust = 0.5, hjust = -0.1, angle=90,
                 size = 5,
                 label = paste0("Valor mínimo total esperado", " = ", input$Need)) +
        annotate(geom = "text", 
                 x = Inf, y = Inf, vjust = 2, hjust = 1.1,
                 size = 7,
                 color = "white",
                 label = probAbove) +
        annotate(geom = "text", 
                 x = input$Need, y = sum(prob == input$Need)/input$reps, vjust = -0.5, hjust = 0.5,
                 size = 5,
                 color = "white",
                 label = percent(sum(prob == input$Need)/input$reps)) +
        theme(legend.position = "none") +
        labs(x = "Suma dados y modificador", y = "Probabilidad", fill = "Probabilidad")
        })
    #Crear texto con resultados detallados
    output$probText <- renderText({
      prob <- probPre()
      #Mostrar probabilidad de obtener número igual o mayor a
      paste("Al lanzar <font color=\'#FF0000\'><b>",input$Dado1Num,"d", input$Dado1Max,
            ifelse(input$Dado2Num == 0, "", paste(" + ", input$Dado2Num,"d",input$Dado2Max)),
            ifelse(input$Dado3Num == 0, "", paste(" + ", input$Dado3Num,"d",input$Dado3Max)),
            ifelse(input$modifier == 0, "", paste(" + ", input$modifier)),
            "</b></font>, puedes obtener valores totales que estén entre <font color=\'#FF0000\'><b>",
            min(prob), "</b></font> y <font color=\'#FF0000\'><b>", max(prob),
            "</b></font>. La probabilidad de obtener específicamente un valor total de <font color=\'#FF0000\'><b>",
            input$Need, "</b></font>es de aproximadamente <font color=\'#FF0000\'><b>", percent(sum(prob == input$Need)/input$reps),
            "</b></font>, mientras que la probabilidad de obtener un valor de <font color=\'#FF0000\'><b>",
            input$Need, "o más</b></font>, es de aproximadamente:")
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
