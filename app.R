# Load packages ----
library(RSQLite)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(readr)
library(magrittr)
library(htmltools)
library(shinycssloaders)


# Source helper functions -----
#source("helpers.R")

dbases <- c("tabla12.sqlite", "tabla13.sqlite", "tabla14.sqlite", "tabla15.sqlite", "tabla16.sqlite", "tabla17.sqlite", "tabla18.sqlite", "season19.sqlite", "season20.sqlite")

conTabla <- dbConnect(RSQLite::SQLite(), dbname=dbases[length(dbases)])
queryRango <- dbGetQuery(conTabla, paste("SELECT DISTINCT Jornada FROM Partidos"))
finalRango <- queryRango$Jornada


#for select in shiny
torneos <- c(2012:2020)
jornadas <- (1:34)
tempActual <- c(1:length(finalRango))
lugares <- (1:18)
current <- 2020
dbDisconnect(conTabla)

#format
a <- list(
  autotick = FALSE,
  ticks = "outside",
  tick0 = 1,
  dtick = 1,
  ticklen = 1,
  tickwidth = 1,
  tickcolor = toRGB("blue"),
  zeroline = FALSE,
  shape = "spline"
  
)

#format
b <- list(
  autotick = FALSE,
  ticks = "outside",
  tick0 = 1,
  dtick = 1,
  ticklen = 1,
  tickwidth = 1,
  tickcolor = toRGB("blue"),
  zeroline = FALSE,
  showlegend = FALSE
)

#Load data

getTabla <- function(entrada){
  n <- match(entrada, torneos)
  salida <- dbases[n]
  salida
  
}


getRangoJornadas <- function(torneo){
  if(torneo == current){
    rango <- c(tempActual)
  }
  else{
    rango <- c(jornadas)
  }
}

header <- dashboardHeader(title = "Gráficos y tablas de la Bundesliga", titleWidth = 600)

body <- dashboardBody( 
  tags$link(rel = "stylesheet", type = "text/css", href = "add.css"),
  #twitter
  tags$head(tags$link(rel="icon", href="favicon.ico")),
  tags$head(tags$meta(name="twitter:card", content="summary_large_image")),
  tags$head(tags$meta(name="twitter:site", content="@EnriqueALopezM")),
  tags$head(tags$meta(name="twitter:title", content="Herramientas interactivas y análisis visual de datos.")),
  tags$head(tags$meta(name="twitter:description", content="Gráficos y consultas a bases de datos acerca de la Bundesliga.")),
  tags$head(tags$meta(name="twitter:creator", content="@EnriqueALopezM")),
  tags$head(tags$meta(name="twitter:image", content="http://www.enadol.de/images/tablas.PNG")),
  
  #facebook
  tags$head(tags$meta(property="og:title", content="Herramientas interactivas de la Bundesliga")),
  tags$head(tags$meta(property="og:type", content="article")),
  tags$head(tags$meta(property="og:url", content="http://data.enadol.de/shiny/tablas")),
  tags$head(tags$meta(property="og:image", content="http://www.enadol.de/images/tablas.PNG")),
  tags$head(tags$meta(property="og:description", content="Goles y puntos de cada equipo, por jornada, por temporada")),
  
  
  fluidRow(
    column(width = 9,
           tabBox(
             width = 900,
             tabPanel("Puntos", plotlyOutput("puntos", height = 600)),
             tabPanel("Goles a Favor", plotlyOutput("gfavor", height = 600)),
             tabPanel("Goles en Contra", plotlyOutput("gcontra", height = 600)),
             tabPanel("Diferencia de Goles", plotlyOutput("diferencia", height = 600))
             
           ),
           #textOutput("encabezado"),
           htmlOutput("cuali"),
           dataTableOutput("general")
           
    ),
    
    column(width = 3,
           box(width = NULL,status = "warning",
               h5("Una herramienta interactiva para consultar datos sobre los torneos de la Bundesliga a partir de 2012."),
               selectInput("torneo",
                           label = "Seleccione el torneo:",
                           choices = c(torneos),selected = torneos[length(torneos)]
               ),
               selectInput("jornada",
                           label="Seleccione la jornada a analizar:",
                           choices = c(jornadas)
               ),
               helpText("Oprima el botón para invocar los datos"),
               actionButton("send", "Obtener gráficas"),
               helpText(h6(strong("Desarrollo:"), "Enrique A. López Magallón - ", strong("@EnriqueALopezM")))
           )
    )
  )
)





# Define server logic
server <- function(input, output, session) {
  
  outVar <- reactive({
    rango <- getRangoJornadas(input$torneo)
    rango
  })
  
  observe({
    updateSelectInput(session, "jornada", label = "Seleccione la jornada a analizar: ", choices = outVar())
  })
  
  observeEvent(input$send, {
    
    tablaIn <- getTabla(input$torneo)
    md <- input$jornada
    my <- input$torneo
    
    con <- dbConnect(RSQLite::SQLite(), dbname=tablaIn)
    
    general <- dbGetQuery(con, paste("SELECT Partidos.Equipo, Partidos.PJ, Partidos.PG, Partidos.PE, Partidos.PP, Goles.Goles_a_favor AS GF, Goles.Goles_en_contra AS GC, Goles.Diferencia AS DIF, Puntos.Total_Puntos AS Puntos FROM Partidos JOIN Goles, Puntos WHERE Partidos.Equipo = Goles.Equipo AND Goles.Equipo=Puntos.Equipo AND Puntos.Jornada=Partidos.Jornada AND Goles.Jornada=Puntos.Jornada AND Puntos.Jornada<=",md,"GROUP BY Partidos.Equipo ORDER BY Puntos DESC, DIF DESC, GF DESC"))
    tcompleta <- list(lugares, general) %>% as.data.frame()
    colnames(tcompleta)[1] <- "Posición"
    #partidostodos <- 
    #dbGetQuery(con, paste("SELECT Equipo, Jornada, Goles_a_favor, Goles_en_contra, Diferencia FROM Goles WHERE Jornada<=", md))
    
    
    puntostodos <- dbGetQuery(con, paste("SELECT Equipo, Jornada, Total_Puntos FROM Puntos WHERE Jornada<=", md,"ORDER BY `Jornada` ASC LIMIT 0, 50000;"))
    #puntostodos
    #golestodos <-  dbGetQuery(con, paste("SELECT Equipo, Jornada, Goles_a_favor, Goles_en_contra, Diferencia FROM Goles WHERE Jornada<=", md))
    tablagf <- dbGetQuery(con, paste("SELECT Equipo, Jornada, Goles_a_favor AS GF FROM `Goles` WHERE `Jornada` <=",md," ORDER BY `Jornada` ASC LIMIT 0, 50000;"))
    tablagc <- dbGetQuery(con, paste("SELECT Equipo, Jornada, Goles_en_contra AS GC FROM `Goles` WHERE `Jornada` <=",md," ORDER BY `Jornada` ASC LIMIT 0, 50000;"))
    tablagdif <- dbGetQuery(con, paste("SELECT Equipo, Jornada, Diferencia AS DIF FROM `Goles` WHERE `Jornada` <=",md," ORDER BY `Jornada` ASC LIMIT 0, 50000;"))
    
    dbDisconnect(con)
    
    hovertextgf <- paste("Equipo: ", tablagf$Equipo, "<br>Jornada: ", tablagf$Jornada, "<br>Goles a favor: ", tablagf$GF)
    hovertextgc <- paste("Equipo: ", tablagc$Equipo, "<br>Jornada: ", tablagc$Jornada, "<br>Goles en contra: ", tablagc$GC)
    hovertextgdif <- paste("Equipo: ", tablagdif$Equipo, "<br>Jornada: ", tablagdif$Jornada, "<br>Diferencia de goles: ", tablagdif$DIF)
    hovertextpuntos <- paste("Equipo: ", puntostodos$Equipo, "<br>Jornada: ", puntostodos$Jornada, "<br>Puntos: ", puntostodos$Total_Puntos)
    
    output$gfavor <- renderPlotly(plot_ly(data=tablagf, x=~Jornada, y=~GF, color = ~Equipo, text= hovertextgf, hoverinfo="skip") %>% add_lines(line=list(shape="spline", smoothing = 1.0)) %>% layout(xaxis=c(a), showlegends=FALSE) %>% add_markers(x=tablagf$Jornada, y=tablagf$GF, showlegend=FALSE, hoverinfo="text"))
    output$gcontra <- renderPlotly(plot_ly(data=tablagc, x=~Jornada, y=~GC, color = ~Equipo, text= hovertextgc, hoverinfo="skip") %>% add_lines(line=list(shape="spline", smoothing = 1.0)) %>% layout(xaxis=c(a), showlegends=FALSE) %>% add_markers(x=tablagc$Jornada, y=tablagc$GC, showlegend=FALSE, hoverinfo="text"))
    output$diferencia <- renderPlotly(plot_ly(data=tablagdif, x=~Jornada, y=~DIF, color = ~Equipo, text= hovertextgdif, hoverinfo="skip") %>% add_lines(line=list(shape="spline", smoothing = 1.0)) %>% layout(xaxis=c(a), showlegends=FALSE) %>% add_markers(x=tablagdif$Jornada, y=tablagdif$DIF, showlegend=FALSE, hoverinfo="text"))
    output$puntos <- renderPlotly(plot_ly(data= puntostodos, x= ~Jornada, y= ~Total_Puntos, color = ~Equipo, text= hovertextpuntos, hoverinfo="skip") %>% add_lines(line=list(shape="spline", smoothing = 1.0)) %>% layout(xaxis=c(a), showlegends=FALSE) %>% add_markers(x=puntostodos$Jornada, y=puntostodos$Total_Puntos, showlegend=FALSE, hoverinfo="text"))
    output$general <- renderDataTable(tcompleta, options = list(searching = FALSE, paging = FALSE, ordering = FALSE, info = FALSE))
    output$cuali <- renderUI({
      tagList(
        
        tags$p(),
        tags$h2(paste("TABLA DE POSICIONES", " - TORNEO ", input$torneo, " - JORNADA ", input$jornada)),
        #tags$h3("Competencias europeas y descenso"),
        tags$h5(tags$span(class = "dotchampions"),"Sitio de Champions League"),
        tags$h5(tags$span(class = "doteleague"), "Sitio de Liga Europa (clasificación)"),
        tags$h5(tags$span(class = "dotrelegation"),"Repechaje primera división"),
        tags$h5(tags$span(class = "dotabstieg"),"Descenso a segunda división")
        
      )
    })
  }
  )
}

# Run the application 
shinyApp(ui = dashboardPage(skin="red",
                            header,
                            dashboardSidebar(disable = TRUE),
                            body
                            
), server = server)

