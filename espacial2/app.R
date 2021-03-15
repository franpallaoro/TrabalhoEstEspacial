library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(shinydashboard)
library(dashboardthemes)
library(leaflet)
library(lubridate)
library(leaflet.extras)



crash <- read.csv2('crash1.csv')
crash$crash_date <- strptime(crash$crash_date, "%Y-%m-%d %H:%M")

crash$data <- as.Date(crash$crash_date, format = "%Y-%m-%d")
dia = max(crash$data)-7
dcrash <- subset(crash, crash_date > dia)

ui <- dashboardPage(
    dashboardHeader(title = "Acidentes de trânsito em Chicago",
                    titleWidth = 340),
    dashboardSidebar(
        width = 340,
        sidebarMenu(
            menuItem("", tabName = "home", icon = icon("home")),
            menuItem("Mapa Estático", icon = icon("globe-americas"), 
                     startExpanded = TRUE,
                     menuSubItem("Marcadores", tabName = "marker", icon = icon("map-marker-alt")),
                     menuSubItem("Heatmap", tabName = "heatmap", icon = icon("fire-alt"))),
            menuItem("Mapa animado", tabName = "map", icon = icon("globe-americas")),
            menuItem("Informações", tabName = "us", icon = icon("info-circle")),
            menuItem(span("Source code", style = "padding-left: 5px"), icon = icon("github"), href = "https://github.com/franpallaoro/app1DisciplinaEstatisticaEspacial")
        )
    ),
    dashboardBody(
        shinyDashboardThemes(theme = "grey_dark"),
        tabItems(
            tabItem(tabName = "home",
                    fluidPage(
                        fluidRow(
                            box(width = 4,
                                title = span("A Disciplina",
                                             style = "color: white; font-size: 20px"),
                                status = "warning",
                                solidHeader = T,
                                span("Esse aplicativo foi desenvolvido para a disciplina 
                                     de Estatística Espacial do curso de Estatística da UFRGS, 
                                     ministrada pela Professora Márcia Barbian.", 
                                     style = "font-size: 16px")
                            ),
                            box(width = 4,
                                title = span("Sobre os Dados",
                                             style = "color: white; font-size: 20px"),
                                status = "success",
                                solidHeader = T,
                                span("O banco de dados mostra várias informações sobre cada acidente de 
                                trânsito dentro dos limites da cidade de Chicago e sob a jurisdição do Departamento de Polícia de Chicago (CPD). 
                                Os dados disponíveis nessa base são para alguns distritos policiais em 2015, mas de toda a cidade começam a ser 
                                registrados apenas a partir de setembro de 2017.", 
                                     style = "font-size: 16px"),
                                br(),
                                span("A variável que será plotada em todos os gráficos será a", strong("INJURIES_TOTAL"),", que determina o total de pessoas que sofreram lesões fatais, incapacitantes, não incapacitantes e possíveis lesões, 
                                conforme determinado pelo oficial de relatório. Essa variável foi transformada em binária, ou seja, caso o número de pessoas que sofreram lesões tenha sido maior que zero, recebe \"Sim\", caso contrário, \"Não\".
                                Os filtros feitos em cima da variável principal e que podem ser encontrados no aplicativo são relativos as seguintes váriaveis do banco de dados:",
                                     style = "font-size: 16px"),
                                br(),
                                span(strong(".	CRASH_DATE:"),"Data e hora do acidente;", style = "font-size: 16px"),
                                br(),
                                span(strong(".	POSTED_SPEED_LIMIT:"), "Limite de velocidade publicado;", style = "font-size: 16px"),
                                br(),
                                span(strong(".	WEATHER_CONDITION:"),"Condições meteorológicas no momento do acidente;", style = "font-size: 16px"),
                                br(),
                                span(strong(".	TRAFFICWAY_TYPE:"),"Tipo de via de tráfego;", style = "font-size: 16px"),
                                br(),
                                span(strong(".	CRASH_DAY_OF_WEEK:"),"O componente do dia da semana da variável CRASH_DATE.", style = "font-size: 16px"),
                                br(),
                                span("Vale salientar que qualquer um desses parâmetros de colisão são registrados pelo oficial do caso com base nas melhores informações disponíveis no momento,
                                     mas muitos deles podem discordar das informações publicadas ou outras avaliações sobre as condições das estradas.", style = "font-size: 16px"),
                            ),
                            box(width = 4,
                                title = span("Visão Gráfica",
                                             style = "color: white; font-size: 20px"),
                                status = "danger",
                                solidHeader = T,
                                span("O objetivo principal desse aplicativo é mostrar através dos diferentes tipos de mapas e utilizando os mais diversos filtros, já especificados anteriormente, 
                                     os locais em Chicago que tiveram acidentes de trânsito com e sem feridos durante o último ano (contando do dia atual para trás).",
                                     style = "font-size: 16px"),
                                br(),
                                span("As ferramentas que estão sendo representadas são:",
                                     style = "font-size: 16px"),
                                br(),
                                span(strong(".	Mapa de pontos:"),"Irá fazer a análise da densidade espacial do nosso fenômeno em questão. Na visão inicial, temos dados agrupados por localização, 
                                     mas conforme à feita a aproximação em alguma área através do cursor, esses pontos vão se dividindo até que possamos ver cada acidente individual;",
                                     style = "font-size: 16px"),
                                br(),
                                span(strong(".	Heatmap:"), "É uma representação gráfica que irá mostrar em quais locais de Chicago houve maior volume de acidentes de trânsito. 
                                     Assim, as indicações de maior ou menor presença nas determinadas áreas do mapa de calor são feitas por cores,",
                                     style = "font-size: 16px"),
                                br(),
                                span(strong(".	Mapa Animado:"), "Em especial nessa representação, utilizamos um mapa de pontos, mas a diferença é a animação. 
                                Através de um filtro de data e hora, representado por uma barra que pode ser executada apenas clicando no ícone,
                                     mostramos o número de acidentes por hora na última semana. Vale observar que a iluminação dos mapas muda de acordo com as horas.",
                                     style = "font-size: 16px"),
                            )
                        )
                    )
            ),
            
            
            #----------------------------------------------------------
            #mapa ponto
            tabItem(tabName = "marker",
                    fluidPage(tags$head(includeCSS("estilo.css")), 
                              fluidRow(
                                  box(width = 7,
                                      title = span("Mapa de pontos dos acidentes",
                                                   style = "color: white; font-size: 20px"),
                                      status = "success",
                                      solidHeader = T,
                                      leafletOutput("mapMark", height = 800)
                                      
                                  ),
                                  column(width = 5,
                                         infoBoxOutput("countAcidentesMark", width = 8),
                                         box(width = 12,
                                             status = "warning",
                                             title = span("Filtros",
                                                          style = "color: white; font-size: 20px"),
                                             fluidRow(width = 12,
                                                      column(width = 6,
                                                             dateRangeInput(
                                                                 'dateRangeMark',
                                                                 label = span("Data do acidente",
                                                                              style = "color: white; font-size: 16px"),
                                                                 start = min(crash$crash_date), end = max(crash$crash_date),
                                                                 min = min(crash$crash_date),
                                                                 max = max(crash$crash_date),
                                                                 separator = "até",
                                                                 language = "pt-BR"
                                                             )
                                                      ),
                                                      column(width = 6,
                                                             selectInput(
                                                                 "injuriesSelMark", 
                                                                 label = span("Feridos", 
                                                                              style = "color: white; font-size: 16px"),
                                                                 c("Não", "Sim"), 
                                                                 selected = c("Não", "Sim"),
                                                                 multiple = T, selectize = TRUE
                                                             )
                                                      )
                                             ),
                                             
                                             fluidRow(width = 12,
                                                      column(width = 6,
                                                             sliderInput("speedMark", 
                                                                         label = span("Limite de velocidade da via",
                                                                                      style = "color: white; font-size: 16px"), 
                                                                         min = min(crash$posted_speed_limit), 
                                                                         max = max(crash$posted_speed_limit), 
                                                                         value = c(min(crash$posted_speed_limit), max(crash$posted_speed_limit))),
                                                             
                                                             checkboxGroupInput("viaMark", 
                                                                                label = span("Tipo de via",
                                                                                             style = "color: white; font-size: 16px"),
                                                                                choices = list("Estacionamento","Mão Única","Não Dividida",
                                                                                               "Desconhecido","Dividida sem Barreira",
                                                                                               "Quatro Vias", "Dividida com Barreira",
                                                                                               "Interseção em T", "Beco", "Outro",
                                                                                               "Faixa Central da Curva","Declive",
                                                                                               "Cinco Pontos, ou mais", "Tipo de Interseção Desconhecida",
                                                                                               "Entrada de Automóveis", "Interseção em Y", "Rota de Tráfego",
                                                                                               "Interseção em L","Desvio","Não Reportado"),
                                                                                selected =  c("Estacionamento","Mão Única","Não Dividida",
                                                                                              "Desconhecido","Dividida sem Barreira",
                                                                                              "Quatro Vias", "Dividida com Barreira",
                                                                                              "Interseção em T", "Beco", "Outro",
                                                                                              "Faixa Central da Curva","Declive",
                                                                                              "Cinco Pontos, ou mais", "Tipo de Interseção Desconhecida",
                                                                                              "Entrada de Automóveis", "Interseção em Y", "Rota de Tráfego",
                                                                                              "Interseção em L","Desvio","Não Reportado"),
                                                                                inline = FALSE
                                                             )
                                                      ),
                                                      column(width = 6,
                                                             checkboxGroupInput("climaMark", 
                                                                                span("Condição do clima",
                                                                                     style = "color: white; font-size: 16px"), 
                                                                                choices = list("Limpo","Neve","Chuva",
                                                                                               "Nublado",
                                                                                               "Neblina/Fumaça/Névoa","Chuva/Chuvisco Congelante",
                                                                                               "Granizo","Nevasca",
                                                                                               "Vento Forte",
                                                                                               "Tempestade de Areia e Sujeira", 
                                                                                               "Desconhecido", "Outro"),
                                                                                selected = c("Limpo","Neve","Chuva","Desconhecido",
                                                                                             "Nublado", "Outro",
                                                                                             "Neblina/Fumaça/Névoa","Chuva/Chuvisco Congelante",
                                                                                             "Granizo","Nevasca",
                                                                                             "Vento Forte","Tempestade de Areia e Sujeira"),
                                                                                inline = FALSE
                                                             ),
                                                             checkboxGroupInput("diaMark",
                                                                                span("Dia da semana",
                                                                                     style = "color: white; font-size: 16px"),
                                                                                choices = list("Segunda-feira","Terça-feira",
                                                                                               "Quarta-feira", "Quinta-feira",
                                                                                               "Sexta-feira", "Sábado",
                                                                                               "Domingo"),
                                                                                selected = c("Segunda-feira","Terça-feira",
                                                                                             "Quarta-feira", "Quinta-feira",
                                                                                             "Sexta-feira", "Sábado",
                                                                                             "Domingo"),
                                                                                inline = FALSE
                                                             )
                                                      )
                                             )
                                         )
                                  )
                              )
                    )
            ),
            
            
            
            #-------------------------------------
            #heatmap
            tabItem(tabName = "heatmap",
                    fluidPage(
                        fluidRow(
                            box(width = 7,
                                title = span("Mapa de calor dos acidentes",
                                             style = "color: white; font-size: 20px"),
                                status = "success",
                                solidHeader = T,
                                leafletOutput("mapHeat", height = 800)
                                
                            ),
                            column(width = 5,
                                   infoBoxOutput("countAcidentesHeat", width = 8),
                                   box(width = 12,
                                       status = "warning",
                                       title = span("Filtros",
                                                    style = "color: white; font-size: 20px"),
                                       fluidRow(width = 12,
                                                column(width = 6,
                                                       dateRangeInput(
                                                           'dateRangeHeat',
                                                           label = span("Data do acidente",
                                                                        style = "color: white; font-size: 16px"),
                                                           start = min(crash$crash_date), end = max(crash$crash_date),
                                                           min = min(crash$crash_date),
                                                           max = max(crash$crash_date),
                                                           separator = "até",
                                                           language = "pt-BR"
                                                       )
                                                ),
                                                column(width = 6,
                                                       selectInput(
                                                           "injuriesSelHeat", 
                                                           label = span("Feridos", 
                                                                        style = "color: white; font-size: 16px"),
                                                           c("Não", "Sim"), 
                                                           selected = c("Não", "Sim"),
                                                           multiple = T, selectize = TRUE
                                                       )
                                                )
                                       ),
                                       
                                       fluidRow(width = 12,
                                                column(width = 6,
                                                       sliderInput("speedHeat", 
                                                                   label = span("Limite de velocidade da via",
                                                                                style = "color: white; font-size: 16px"), 
                                                                   min = min(crash$posted_speed_limit), 
                                                                   max = max(crash$posted_speed_limit), 
                                                                   value = c(min(crash$posted_speed_limit), max(crash$posted_speed_limit))),
                                                       
                                                       checkboxGroupInput("viaHeat", 
                                                                          label = span("Tipo de via",
                                                                                       style = "color: white; font-size: 16px"),
                                                                          choices = list("Estacionamento","Mão Única","Não Dividida",
                                                                                         "Desconhecido","Dividida sem Barreira",
                                                                                         "Quatro Vias", "Dividida com Barreira",
                                                                                         "Interseção em T", "Beco", "Outro",
                                                                                         "Faixa Central da Curva","Declive",
                                                                                         "Cinco Pontos, ou mais", "Tipo de Interseção Desconhecida",
                                                                                         "Entrada de Automóveis", "Interseção em Y", "Rota de Tráfego",
                                                                                         "Interseção em L","Desvio","Não Reportado"),
                                                                          selected =  c("Estacionamento","Mão Única","Não Dividida",
                                                                                        "Desconhecido","Dividida sem Barreira",
                                                                                        "Quatro Vias", "Dividida com Barreira",
                                                                                        "Interseção em T", "Beco", "Outro",
                                                                                        "Faixa Central da Curva","Declive",
                                                                                        "Cinco Pontos, ou mais", "Tipo de Interseção Desconhecida",
                                                                                        "Entrada de Automóveis", "Interseção em Y", "Rota de Tráfego",
                                                                                        "Interseção em L","Desvio","Não Reportado"),
                                                                          inline = FALSE
                                                       )
                                                ),
                                                column(width = 6,
                                                       checkboxGroupInput("climaHeat", 
                                                                          span("Condição do clima",
                                                                               style = "color: white; font-size: 16px"), 
                                                                          choices = list("Limpo","Neve","Chuva",
                                                                                         "Nublado",
                                                                                         "Neblina/Fumaça/Névoa","Chuva/Chuvisco Congelante",
                                                                                         "Granizo","Nevasca",
                                                                                         "Vento Forte",
                                                                                         "Tempestade de Areia e Sujeira", 
                                                                                         "Desconhecido", "Outro"),
                                                                          selected = c("Limpo","Neve","Chuva","Desconhecido",
                                                                                       "Nublado", "Outro",
                                                                                       "Neblina/Fumaça/Névoa","Chuva/Chuvisco Congelante",
                                                                                       "Granizo","Nevasca",
                                                                                       "Vento Forte","Tempestade de Areia e Sujeira"),
                                                                          inline = FALSE
                                                       ),
                                                       checkboxGroupInput("diaHeat",
                                                                          span("Dia da semana",
                                                                               style = "color: white; font-size: 16px"),
                                                                          choices = list("Segunda-feira","Terça-feira",
                                                                                         "Quarta-feira", "Quinta-feira",
                                                                                         "Sexta-feira", "Sábado",
                                                                                         "Domingo"),
                                                                          selected = c("Segunda-feira","Terça-feira",
                                                                                       "Quarta-feira", "Quinta-feira",
                                                                                       "Sexta-feira", "Sábado",
                                                                                       "Domingo"),
                                                                          inline = FALSE
                                                       )
                                                )
                                       )
                                   )
                            )
                        )
                    )
            ),
            #mapa animado 
            #------------------------------------
            tabItem(tabName = "map",
                    fluidPage(
                        box(width = 12,
                            sliderInput("year",span("Número de acidentes durante a última semana por hora",
                                                    style = "color: white; font-size: 16px"), 
                                        min = min(dcrash$crash_date), max = max(dcrash$crash_date), value = min(dcrash$crash_date),
                                        timeFormat = "%Y-%m-%d %H:%M", ticks = F, animate = T, step = 3600),
                            status = "danger"
                        ),
                        box(width = 12,
                            leafletOutput("mapAnim", width = "100%", height = "600"),
                            status = "success",
                            title = span("Mapa animado dos acidentes da última semana por hora",
                                         style = "color: white; font-size: 20px"),
                            solidHeader = T
                        )  
                    )
                    
            ),
            
            #Sobre n?s
            #-----------------------------------------------------------
            
            tabItem(tabName =  "us",
                    fluidRow(
                        box(width = 4,                        
                            title =  span("Franciele Lobo Pallaoro",
                                          style = "color: white; font-size: 20px"),
                            "Graduanda em Estatística",
                            br(),
                            "Universidade Federal do Rio Grande do Sul",
                            br(),
                            br(),
                            icon("github"),
                            "github.com/franpallaoro",
                            br(),
                            icon("at"),
                            "franpallaoro@gmail.com",
                            solidHeader = T,
                            status = "warning"
                        ),
                        
                        box(width = 4,                        
                            title =  span("Giulia Bagatini Carlotto",
                                          style = "color: white; font-size: 20px"),
                            "Graduanda em Estatística",
                            br(),
                            "Universidade Federal do Rio Grande do Sul",
                            br(),
                            br(),
                            icon("github"),
                            "github.com/Giulia-Carlotto",
                            br(),
                            icon("at"),
                            "gbcarlotto@gmail.com",
                            solidHeader = T,
                            status = "warning"
                        ),
                        
                        box(width = 4,                       
                            title =  span("Tainá Cabalheiro",
                                          style = "color: white; font-size: 20px"),
                            "Graduanda em Estatística",
                            br(),
                            "Universidade Federal do Rio Grande do Sul",
                            br(),
                            br(),
                            icon("github"),
                            "github.com/taina-cabalheiro",
                            br(),
                            icon("at"),
                            "cabalheirot@gmail.com",
                            solidHeader = T,
                            status = "warning"
                        )
                    )
            )
        )
        
    )
)


server <- function(input, output) {
    
    #------------------
    #data mark
    dataMark <- reactive({
        res <- crash %>% 
            filter(crash_date >= input$dateRangeMark[1] & crash_date <= input$dateRangeMark[2]) %>% 
            filter(injuries %in% input$injuriesSelMark)%>%
            filter(posted_speed_limit >= input$speedMark[1] & posted_speed_limit <= input$speedMark[2]) %>% 
            filter(trafficway_type %in% input$viaMark)%>%
            filter(crash_day_of_week %in% input$diaMark)%>%
            filter(weather_condition %in% input$climaMark)
        res
    })
    
    output$countAcidentesMark <- renderValueBox({
        valueBox(
            paste0(nrow(dataMark())), "Acidentes", icon = icon("car-crash"),
            color = "maroon"
        )
    }) 
    
    #---------------------
    #dataHeat
    dataHeat <- reactive({
        res <- crash %>% 
            filter(crash_date >= input$dateRangeHeat[1] & crash_date <= input$dateRangeHeat[2]) %>% 
            filter(injuries %in% input$injuriesSelHeat)%>%
            filter(posted_speed_limit >= input$speedHeat[1] & posted_speed_limit <= input$speedHeat[2]) %>% 
            filter(trafficway_type %in% input$viaHeat)%>%
            filter(crash_day_of_week %in% input$diaHeat)%>%
            filter(weather_condition %in% input$climaHeat)
        res
    })
    
    output$countAcidentesHeat <- renderValueBox({
        valueBox(
            paste0(nrow(dataHeat())), "Acidentes", icon = icon("car-crash"),
            color = "maroon"
        )
    })    
    
    
    
    #mapa marcadores
    #--------------------------------------------------
    output$mapMark <- renderLeaflet({
        icoLst <- awesomeIconList(
            "Não" = makeAwesomeIcon(
                icon = "user",
                iconColor = "black",
                markerColor =  "darkblue"
            ),
            "Sim" = makeAwesomeIcon(
                icon = "alert",
                iconColor = "black",
                markerColor =  "darkred"
            )
        )
        
        html_legend <- "
        <div style='position: relative; display: inline-block' class='awesome-marker-icon-darkred awesome-marker'><i class='glyphicon glyphicon-alert icon-black' 
        style='padding-left: 4.5px;'></i></div> 
            <div style='position: relative;display: inline-block;top: 15px;'> Acidente com feridos </div>
            <br>
        <div style='position: relative; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'><i class='glyphicon glyphicon-user icon-black' style='
        padding-left: 4.5px;'></i></div>
             <div style='position: relative;display: inline-block;top: 15px;'> Acidente sem feridos </div>"
        
        
        leaflet(dataMark()) %>% addTiles() %>% 
            addAwesomeMarkers(lng = ~longitude,
                              lat = ~latitude,
                              icon = ~ icoLst[injuries],
                              clusterOptions = markerClusterOptions())%>%
            addControl(html = html_legend, position = "bottomleft")%>%
            fitBounds(-87.525659, 41.634836, -87.842203, 42.040514)
    })
    
    
    #mapa calor
    #--------------------------------------------------  
    output$mapHeat <- renderLeaflet({
        
        leaflet(dataHeat()) %>% addTiles() %>% 
            addHeatmap(lng = ~longitude, lat = ~latitude,
                       blur = 20, max = 0.05, radius = 15)%>%
            fitBounds(-87.525659, 41.634836, -87.842203, 42.040514)
    })
    
    #mapa animado
    #-------------------------------------------
    output$mapAnim <- renderLeaflet({
        
        icoLst <- awesomeIconList(
            "Não" = makeAwesomeIcon(
                icon = "user",
                iconColor = "black",
                markerColor =  "darkblue"
            ),
            "Sim" = makeAwesomeIcon(
                icon = "alert",
                iconColor = "black",
                markerColor =  "darkred"
            )
        )
        
        dcrash %>%
            filter(crash_date == min(crash_date)) %>%
            leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            addAwesomeMarkers(lng = ~longitude, lat = ~latitude, 
                              icon = ~ icoLst[injuries]) %>%
            fitBounds(-87.525659, 41.634836, -87.842203, 42.040514)
        
        
    })
    
    observeEvent(input$year,{
        
        
        # adding day and night section --------------------------------------------
        
        if (hour(input$year) >= 10 & hour(input$year) < 22 ) {
            
            icoLst <- awesomeIconList(
                "Não" = makeAwesomeIcon(
                    icon = "user",
                    iconColor = "black",
                    markerColor =  "darkblue"
                ),
                "Sim" = makeAwesomeIcon(
                    icon = "alert",
                    iconColor = "black",
                    markerColor =  "darkred"
                )
            )
            
           
            
            leafletProxy("mapAnim", data= dcrash %>%
                             filter(crash_date < input$year) %>%
                             filter(crash_date == max(crash_date)) ) %>%
                clearMarkers() %>%
                addTiles() %>%
                addAwesomeMarkers(lng = ~longitude, lat = ~latitude, 
                                  icon = ~ icoLst[injuries])
        } else {
            
            icoLst <- awesomeIconList(
                "Não" = makeAwesomeIcon(
                    icon = "user",
                    iconColor = "black",
                    markerColor =  "darkblue"
                ),
                "Sim" = makeAwesomeIcon(
                    icon = "alert",
                    iconColor = "black",
                    markerColor =  "darkred"
                )
            )
            
            
            leafletProxy("mapAnim", data= dcrash %>%
                             filter(crash_date < input$year) %>%
                             filter(crash_date == max(crash_date)) ) %>%
                clearMarkers() %>%
                addProviderTiles(providers$CartoDB.DarkMatter) %>%
                addAwesomeMarkers(lng = ~longitude, lat = ~latitude, 
                                  icon = ~ icoLst[injuries])}
    })
}
shinyApp(ui,server)