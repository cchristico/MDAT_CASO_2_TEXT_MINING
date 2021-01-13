library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)

docsList <- c(seq(1,86, by=1))

dashboardPage(
  
    dashboardHeader(title = "Modelos de Tópicos"),
    dashboardSidebar(
            sidebarMenu(
                        menuItem("Modelo Topicos", tabName = "LDA", icon = icon("dashboard"))#,
                        #menuItem("About", tabName = "about", icon = icon("flash"))

                        )
                    ),
dashboardBody(
                tabItems(
                    tabItem(tabName = "LDA",
                            ###
                            
                            
                            tags$head(
                              tags$style(
                                HTML(
                                  "

                                #textIni {
                                font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
                                font-size: 14px;
                                width: 300px; 
                                max-width: 100%;
                                padding: 6px 12px; 
                                white-space: pre-wrap;
                                }
                                
                                #textCl1 {
                                font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
                                font-size: 14px;
                                width: 300px; 
                                max-width: 100%;
                                padding: 6px 12px; 
                                white-space: pre-wrap;
                                }
                                
                                #textLem {
                                font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
                                font-size: 14px;
                                width: 300px; 
                                max-width: 100%;
                                padding: 6px 12px; 
                                white-space: pre-wrap;
                                }
                                
                                "
                                )
                              )
                            ),
                            
                            
                            ###
                            
                            h2("Mineria de texto"),
                            p(" La mineria de texto se define como un proceso que permite la extracción de patrones
                            relevantes y no triviales de documentos no estructurados o semi estructurados."),
                            p("Dicho proceso se encuntra marcado por las actividades mostradas en la siguiente figura:"),
                            img(src='textProces.png',width=1000, alt="Proceso mineria de texto", align="center" ),
                            
                            h2("Limpieza texto"),
                            p("La limpieza de documentos permite obtener palabras que pueden ser interpretadas y clasificadas 
                              por los modelos de tópicos. Para este ejemplo se manejaron varias técnicas de preprocesamiento de texto."),
                              selectInput('doc','Seleccione un Documento',docsList, selected = "1",
                                          selectize = FALSE ),
                            fluidRow(style = "height:200px",
                              column(4,h4("Texto Original"),verbatimTextOutput('textIni'),p("Texto extraido de las fuentes")),
                              column(4,h4("Texto Procesado"),verbatimTextOutput('textCl1'),
                                     tags$ul(
                                       tags$li("Remoción Stop Words"), 
                                       tags$li("Eliminación caracteres especiales"), 
                                       tags$li("Todo a minusculas")
                                     )
                                     ),
                              column(4,h4("Steeming"),verbatimTextOutput('textLem'),p("Reducción de terminos usando Steeming"))
                                    ),
                              hr(),
                            hr(),
                            
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
                            br(),
                            h3("Topicos Encontrados"),
                            p("El proceso de análisis y modelos de tópicos da un total de 8 tópicos"),
                            
                           # plotOutput("topicNum"),
                            
                            plotOutput("Topics"),
                            
                            
                            selectizeInput(
                              inputId = "docs", 
                              label = "Seleccione un Documento", 
                              choices = unique(docsList), 
                              selected = c(1,50,85),
                              multiple = TRUE
                            ), 
                            plotlyOutput(outputId = "docTopic"),
                            
                           dataTableOutput('tblTopics')
                            
                    
                            
                            
                            )#,
                    # tabItem(tabName = "about",
                    #         
                    #         
                    #             h1("About"),
                    #         
                    # 
                    #         
                    #         
                    # )
                        )
        
             )
)
