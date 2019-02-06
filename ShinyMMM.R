library(shiny)
library(readxl)
library(sqldf)

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
                   tags$h1("Resultados: "),
                   verbatimTextOutput("view"),
                   plotOutput('grafico')),
                 fluid = TRUE
               )
      ))
    
    
  )
)

server <- function(input, output){
  
  #Para leer la funcion a optimizar
  vector2 <- reactive({
    if(input$Do){
      vector2 <- as.numeric(unlist(strsplit(input$vec1,",")))
      x <- as.Date(input$FechaInicio, "%d/%m/%Y")
      weekIni <- format(x, "%V")
      x <- as.Date(input$FechaFin, "%d/%m/%Y")
      weekFin <- format(x, "%V")
      
      #Esto no funcionara para cuando haya una semana en un anio y otra en el anio siguiente. ***REVISAR***
      weeks <- as.numeric(weekFin)-as.numeric(weekIni)
      print(weeks)
      #Ahora se sabe el numero de semanas por lo que al momento de la distrubicion lo dividira en las semanas correspondientes
      vector2
    }
  })
  
  #Funcion para optimizar
  
  opt <- reactive({
    if(input$Do){
      
      library(Rsolnp)
      
      #x[2] -> Digital
      #x[3] -> Radio
      #x[4] -> TV
      #x[5] -> VP
      #x[6] -> Prensa
      
      opt_func <- function(x) {
        isolate(vector2())[1] + isolate(vector2())[2]*x[1] + isolate(vector2())[3]*x[2] + isolate(vector2())[4]*x[3] + isolate(vector2())[5]*x[4] + isolate(vector2())[6]*x[5]
        #770.296 + 353.166*x[1] + 45.382*x[2] + 48.751*x[3] + 87.643*x[4] + 216.672*x[5]
      }
      
      inequal <- function(x) {
        x[2] + x[3] + x[4] + x[5] + x[6]
      }
      
      Presupuesto <- input$Presupuestomax
      
      digitalmin <- input$digital[1]
      digitalmax <- input$digital[2]
      
      radiomin <- input$radio[1]
      radiomax <- input$radio[2]
      
      tvmin <- input$TV[1]
      tvmax <- input$TV[2]
      
      vpmin <- input$vp[1]
      vpmax <- input$vp[2]
      
      prensamin <- input$prensa[1]
      prensamax <- input$prensa[2]
      
      salida <- solnp(c(1,1,1,1,1,1), #starting values (random - obviously need to be positive and sum to 15)
                      opt_func, #function to optimise
                      #eqfun=equal, #equality function 
                      #eqB=Presupuesto,   #the equality constraint
                      ineqfun=inequal,
                      ineqLB=Presupuesto/2,
                      ineqUB=Presupuesto,
                      LB=c(0.1,digitalmin,radiomin,tvmin,vpmin,prensamin), #lower bound for parameters i.e. greater than zero
                      UB=c(0.1,digitalmax,radiomax,tvmin,vpmax,prensamax),
                      control = list(trace = 0)) #Para que no muestre el mensaje de error se pone trace = 0
      Nombres <- c("Digital","Radio", "TV", "VP", "Prensa")
      dataOpt <- data.frame(medio = c("Digital","Radio", "TV", "VP", "Prensa"), 
                            valor = c(salida$pars[2],salida$pars[3],salida$pars[4],salida$pars[5],salida$pars[6]),
                            camp = "Solucion")
      
      dataOpt
    }
  })
  
  #Calcula el share de inversion
  share <- reactive({
    dataOpt <- opt()
    
    Total <- 0
    
    for (i in 1:nrow(dataOpt)){
      Total = Total + dataOpt$valor[i]
    }
    
    Total
    
    dataOpt$valor <- round(((dataOpt$valor/Total)*100),2)
    print("Share de inversion")
    dataOpt
  })
  
  
  #Muestra la optimización en pantalla
  output$view <- renderPrint({
    opt() 
  })
  
  #Crea Staked Bar para mostrar el Mix actual
  
  output$grafico <- renderPlot({
    inFile <- input$file1
    if(input$Do){
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
      db <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      
      Invmedios <- sqldf("Select sum(Digital) as Digital, sum(Radio) as Radio, sum(TV_Group) as TV, sum(VP_Group) as VP, sum(Prensa_Group) as Prensa from db")
      
      Totalmedios <- 0
      
      for(i in 1:ncol(Invmedios)){
        Totalmedios <- Totalmedios + Invmedios[i]
      }
      
      Invmedios2 <- Invmedios
      
      for(i in 1:ncol(Invmedios)){
        Invmedios2[i] <- Invmedios[i]/Totalmedios
      }
      
      library(ggplot2)
      
      nombres <- names(Invmedios2)
      
      dataf <- data.frame(medio = nombres, valor = t(Invmedios2))
      dataf$camp <- "Actual"
      dataf$valor <- round((dataf$valor *100),2)
      dataf
      
      graf <- ggplot() + geom_bar(aes(y = valor, x = camp, fill = medio), data = dataf,
                                  stat="identity")
      graf <- graf + geom_text(data=dataf, aes(x = camp, y = valor,
                                               label = paste0(valor,"%")), size=4)
      graf
      
      Nuevo <- rbind(dataf, share())
      
      library(plyr)
      library(reshape2)
      
      #dataf <- ddply(dataf, "camp", transform, pos = cumsum(valor)-0.5*valor)
      
      ggplot(data=Nuevo, aes(x=camp, y=valor, fill=medio)) + 
        geom_bar(stat="identity", position=position_stack()) +
        geom_text(aes(y = valor, ymax = valor, label = paste0(valor,"%")), 
                  position = position_stack(), size=3, vjust=1, hjust=0.5 ,col="white")
      #candleChart(graf, name=input$Do)
      #graf
    }
    
  })
}

shinyApp(ui, server)
