library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)

dashboardPage(
    dashboardHeader(title = "Análisis de indices IPT-IH-IR"),
    dashboardSidebar(
            sidebarMenu(
                        menuItem("Modelo Topicos", tabName = "LDA", icon = icon("dashboard")),
                        menuItem("About", tabName = "about", icon = icon("flash"))

                        )
                    ),
dashboardBody(
                tabItems(
                    tabItem(tabName = "LDA",
                            h2("Mineria de texto"),
                            p(" La mineria de texto se define como un proceso que permite la extracción de patrones
                            relevantes y no triviales de documentos no estructurados o semi estructurados."),
                            p("Dicho proceso se encuntra marcado por las actividades mostradas en la siguiente figura:"),
                            img(src='textProces.png',width=1000, alt="Proceso mineria de texto", align="center" ),
                            
                            h2("Limpieza texto"),
                              selectInput('doc','Seleccione un Documento',docsList, selected = "1",
                                          selectize = FALSE ),
                              verbatimTextOutput('textIni'),
                              verbatimTextOutput('textCl1'),
                              verbatimTextOutput('textLem'),
                            
                            
                                h2("Análisis de datos"),
                            sliderInput(inputId = "words",
                                        label = "Top Palabras:",
                                        min = 5,
                                        max = 30,
                                        value = 10),
                            plotOutput("distPlot"),
                            
                            h3("Palabras representativas"),
                            
  
                                    sliderInput("freq",
                                                "Frecuencia mínima:",
                                                min = 1,  max = 50, value = 15),
                                    sliderInput("max",
                                                "Número máximo de palabras:",
                                                min = 1,  max = 300,  value = 100),
                            
                            plotOutput("wclo"),
                            plotOutput("topicNum")
                            
                            
                            
                    
                            
                            
                            ),
                    tabItem(tabName = "about",
                            
                            
                                h1("About"),
                            

                            
                            
                    )
                        )
        
             )
)
