library(shiny)
library(readxl)

ui <- shinyUI(
  
  fluidPage(
    title = "Tesis M.Daza!",
    tabsetPanel(
      tabPanel("MMM",
               titlePanel("MMM - Optimizacion"),
               sidebarLayout(
                 sidebarPanel(
                   fileInput('file1', 'Seleccione un archivo xlsx', accept = c(".xlsx")),
                   textInput('vec1', 'Ingrese los betas de la funcion (separados por coma)', "770.296, 353.166, 45.382, 48.751, 87.643, 216.672"),
                   numericInput("Presupuestomin", "Presupuesto min (en millones):", 1),
                   numericInput("Presupuestomax", "Presupuesto maximo(en millones):", 1),
                   sliderInput("digital", "Rango de inversion en Digital:",
                               min = 1, max = 100,
                               value = c(20,50)),
                   sliderInput("radio", "Rango de inversion en Radio:",
                               min = 1, max = 100,
                               value = c(20,50)),
                   sliderInput("TV", "Rango de inversion en TV:",
                               min = 1, max = 100,
                               value = c(20,50)),
                   sliderInput("vp", "Rango de inversion en Via Publica:",
                               min = 1, max = 100,
                               value = c(20,50)),
                   sliderInput("prensa", "Rango de inversion en Prensa:",
                               min = 1, max = 1000,
                               value = c(20,50)),
                   dateInput("FechaInicio", "Fecha de Inicio:", value = NULL, min = NULL, max = NULL,
                             format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                             language = "en", width = NULL),
                   dateInput("FechaFin", "Fecha de Fin:", value = NULL, min = NULL, max = NULL,
                             format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                             language = "en", width = NULL),
                   actionButton('Do', 'Run'),
                   actionButton('Do2', 'Run2')
                 ),
                 mainPanel(
                   verbatimTextOutput("view"),
                   verbatimTextOutput("oid2"),
                   textOutput("algo"),
                   plotOutput('grafico')),
                 fluid = TRUE
               )
      ))
    
    
  )
)

server <- function(input, output){
  
}

shinyApp(ui, server)
