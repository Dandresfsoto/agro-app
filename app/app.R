### PAQUETES ----
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(shinyBS)
library(fresh)
library(DT)
library(tidyverse)
library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(plotly)
library(readxl)
library(reshape2)
library(leaflet)
library(rlang)
library(RColorBrewer)
library(data.table)
library(FactoMineR)
library(factoextra)
library(xlsx)
library(DBI)
library(RMySQL)
library(glue)

### EXTRACCION DE DATOS ----
#setwd('E:/NEW_DATLAS')

### UI ----
ui<-dashboardPage(
  freshTheme=create_theme(
    adminlte_color(
      light_blue="#292A48",
      blue="#FFCD00", 
      navy="#292A48",
      red="#292A48",
      yellow='#FFCD00'
    ),
    adminlte_sidebar(
      dark_bg="#C2C4FF", 
      dark_hover_bg="#FFCD00", 
      dark_color="#292A48",
      dark_submenu_color='red'
    )
  ),
  
  header <- dashboardHeader(
    title = HTML(paste0(
      '<span class = "logo-lg">
            <img src = "https://drive.google.com/uc?export=view&id=1OhpHc2ApRmOX_RC0j4xvsx7Hyx_5rNtF" height="40" style="width:107%;position:left;">
                </span>',
      '<span class = "logo-mini">
            <img src= "https://drive.google.com/uc?export=view&id=1OiiwEoSN0nG7BZdOP0XZwdPix6cONidL" height="40" style="width:100%;position:center;">
                </span>'
    )),
    tags$li(class = "dropdown",
            div(class="inlay", style="height:8px;"),
            tags$img(src="https://drive.google.com/uc?export=view&id=1OkvkQOztXq2xL2KwxN1ZNpdJQUA5bp_G", style="height:35px;position:center;")
    ),

    controlbarIcon=strong(icon('fas fa-book'),'GLOSARIO')
  ), 
  
  sidebar<-dashboardSidebar(
    disable=FALSE,  collapsed=TRUE, minified=TRUE, 
    sidebarMenu(
      id='tabs', 
      div(class="inlay", style="height:3px;width:100%;background-color:#292A48;"),
      menuItem("DESCRIPTIVAS", tabName="DESCRIPTIVAS", icon=icon("fas fa-chart-area"), selected=TRUE),
      div(class="inlay", style="height:3px;width:100%;background-color:#292A48;"),
      menuItem("EXPORTACIONES", tabName="EXPORTACIONES", icon=icon("fas fa-luggage-cart")), 
      div(class="inlay", style="height:3px;width:100%;background-color:#292A48;"),
      menuItem("PROXIMIDAD", tabName="PROXIMIDAD", icon=icon("fas fa-exchange-alt")), 
      div(class="inlay", style="height:3px;width:100%;background-color:#292A48;"),
      menuItem("COMPLEJIDAD", tabName="COMPLEJIDAD", icon=icon("fas fa-chart-bar")), 
      div(class="inlay", style="height:3px;width:100%;background-color:#292A48;"),
      menuItem("RANKING", tabName="RANKING", icon=icon("fas fa-wave-square")),
      div(class="inlay", style="height:3px;width:100%;background-color:#292A48;"),
      menuItem("TIPOLOGIA", tabName="TIPOLOGIA", icon=icon("fas fa-layer-group")),
      div(class="inlay", style="height:3px;width:100%;background-color:#292A48;"),
      menuItem("EFICIENCIA", tabName="ANAL", icon=icon("fas fa-exclamation-circle"),
               HTML('<a style="font:bold 120% serif;" href=https://davidhincapie.shinyapps.io/Simul/ target="_blank">ANALISIS DE EFICIENCIA</a>'),br()),
      div(class="inlay", style="height:3px;width:100%;background-color:#292A48;")
    )
  ), 
  
  body<-dashboardBody(shinyjs::useShinyjs(), uiOutput("body")), 
  
  controlbar<-dashboardControlbar(br(), DTOutput('GLOSARIO'))
  
  )


### SERVER ----
server<-function(input, output, session){
  ### UI BODY ----
  output$body<-renderUI({
    tabItems(
      ### DESCRIPTIVAS ----
      tabItem(
        tabName="DESCRIPTIVAS", class='active', 
        fluidRow(
          tags$head(tags$style(HTML('.content-wrapper,.right-side{background-color:#292A48;},h2{font-family: "Yusei Magic", sans-serif;}'))),
          tags$head(tags$style(HTML(".shiny-split-layout>div{overflow:visible;}"))),
          box(width=4, status = 'warning', title = 'SELECCIÓN DE DATOS',
              div(id='descrip_id', style='disply:block;font: 100% ;overflow-y:scroll;height:525px', 
                  selectizeInput('BASE0','BASE DE DATOS',choices=c('INDICE DE APTITUD MUNICIPAL'='IAM','EVALUACIONES AGROPECUARIAS'='EVA')),
                  selectizeInput('DEPTO0','DEPARTAMENTO',choices=c('VALLE DEL CAUCA'), multiple=TRUE, selected='VALLE DEL CAUCA'),
                  selectizeInput('MUNICIPIO0','MUNICIPIO',choices=c(''), multiple=TRUE),
                  selectizeInput('ANO0','AÑO',choices=c('')),
                  selectizeInput('PRODU0','PRODUCTO',choices=c(''), multiple=TRUE),
                  selectizeInput('VARIABLE0','VARIABLE PRINCIPAL',choices=c('')),
                  selectizeInput('VARIABLE1','VARIABLE ADICIONAL',choices=c('')),
                  selectizeInput('PRODUCTOR0','PRODUCTOR',choices=c('TODOS','SI','NO')),
                  selectizeInput('CONFLICTO0','POST-CONFLICTO',choices=c('TODOS','SI','NO')),
                  br()
              )
          ),
          box(width=8, status = 'warning', title = 'COMPORTAMIENTO DE: ',
              plotlyOutput('GRAF_DESCRIP1') %>% withSpinner(),
              div(id='redes_id', 
                  strong('GRÁFICO DE CAJA:',style="font:bold 100% ;"),
                  HTML("<p style='color:black;font: 90% ;text-align:justify;overflow-y:scroll;height:100px'>
                  El grafico nos indica el comportamiento de los municipios pertenecientes a un departamento en cuanto a los 
                  valores obtenidos en las variables, explicando que tan similares son los municipios en cuanto a la cadena 
                  productiva, indicando el mínimo, mediana, media, máximo e identificación de municipios con comportamientos atípicos. 
                  Por ejemplo: Si observamos las variables rendimiento el grafico, y tenemos un diagrama muy amplio y con un rango muy 
                       alto quiere decir que los municipios del departamento observado tienen diferencias en su forma de producir dado 
                       que son muy heterogéneos en sus rendimientos, la identificación de datos atípicos se observa en aquellos puntos 
                       por fuera de los bigotes y esto permite plantear reflexiones de porque esa condición.</p>")
              )
          )
        ),
        fluidRow(
          box(width=4, status = 'warning', 
              div(id='descrip_id', style='disply:block;font: 100% ;overflow-y:scroll;height:400px',
                  textOutput('TEXTO_GRAFICO2'),
                  hr(),
                  tableOutput('TAB_DESPLAZA'),
                  textOutput('TIEMPO_DESPLAZA')
              )),
          box(width=8, status = 'warning', title = paste0('COMPORTAMIENTO DE: '),
              plotlyOutput('GRAF_DESCRIP2') %>% withSpinner()
          )
        ),
        fluidRow(
          box(width=12, status = 'warning', title = 'BASE DE DATOS',
              DTOutput('TAB_DESCRIP') %>% withSpinner(),
              downloadBttn('DESCARGAR_DESCRIPTIVAS','Descargar')
          )
        )
      ),
      ### EXPORTACIONES ----
       tabItem(
        tabName="EXPORTACIONES", 
        fluidRow(
          box(width = 4, status = 'warning', title = 'SELECCIÓN DE DESTINOS',
              div(id='exp_id', style='disply:block;font: 100% ;overflow-y:scroll;height:520px', 
              selectizeInput('BASE1','BASE DE DATOS',choices=c('EXPORTACIONES','IMPORTACIONES')),
              selectizeInput('DEPTO1','DEPARTAMENTO',choices=c(''), multiple=TRUE),
              selectizeInput('PAIS1','PAIS',choices=c(''), multiple=TRUE),
              selectizeInput('SECTOR1','SECTOR',choices=c(''), multiple=TRUE),
              selectizeInput('PRODU1','PRODUCTO',choices=c(''), multiple=TRUE),
              selectizeInput('POSAR1','NOMENCLATURA ARANCELARIA',choices=c(''), multiple=TRUE),
              selectizeInput('ANO1','AÑO',choices=c('2020','2019','2018')),
              br()
              )
          ),
          box(width = 8, status = 'warning', title = 'DESTINOS',
              plotlyOutput('DESTINO') %>% withSpinner(),
              div(id='redes_id',
                  strong('GRÁFICO DE BARRAS:',style="font:bold 100% ;"),
                  HTML("<p style='color:black;font: 90% ;text-align:justify;overflow-y:scroll;height:100px'>
                El diagrama de barras representa gráficamente un conjunto de valores por medio de barras con 
                       longitud proporcional a los valores representados. En este caso, para un determinado 
                       año y departamento, las barras de mayor longitud, representadas de otro color, indican 
                       hacia (exportaciones) que países se exporta más un determinado producto o desde 
                       (importaciones) que país se importa más un determinado producto.</p>")
              )
          )
        ),
        fluidRow(
          box(width = 12, status = 'warning', title = 'CATEGORIAS',
              plotlyOutput('TREEMAP') %>% withSpinner(),
              div(id='redes_id',
                  strong('GRÁFICO TREEMAP:',style="font:bold 100% ;"),
                  HTML("<p style='color:black;font: 90% ;text-align:justify;overflow-y:scroll;height:100px'>
                  El Treemap o diagrama de árbol es un método para mostrar datos jerárquicos, estructurados 
                       en forma de rectángulos. El diagrama presentado, muestra para un determinado año y 
                       departamento, cuáles sectores productivos exportaron/importaron más (en términos de 
                       dólares) y dentro de cada sector productivo, cuáles fueron los productos de mayor 
                       exportación/importación.</p>")
              )
          )
        ),
        fluidRow(
          box(width = 12, status = 'warning', title = 'BASE DE DATOS',
              DTOutput('EXPORTACIONES') %>% withSpinner(),
              downloadBttn('DESCARGAR_EXPORTACIONES','Descargar')
          )
        )
      ),
      ### COMPLEJIDAD ----
      tabItem(
        tabName="COMPLEJIDAD", 
        div(id='complej_id', style='font:bold 100% ;color:white;', 
            column(width=4, selectizeInput('DEPTO2','DEPARTAMENTO',choices=c(''), selected='')),
            column(width=4, selectizeInput('SECTOR2','SECTOR',choices=c(''), multiple=TRUE)),
            column(width=4, selectizeInput('SUBSECTOR2','SUBSECTOR',choices=c(''), multiple=TRUE)),
            column(width=4, selectizeInput('PRODU2','PRODUCTO',choices=c(''), multiple=TRUE)),
            column(width=4, selectizeInput('POSAR2','NOMENCLATURA ARANCELARIA',choices=c(''), multiple=TRUE)),
            column(width=4,
                   column(width=4, selectizeInput('ANO2','AÑO',choices=c(''))),
                   column(width=4, radioGroupButtons('RCA','RCA', choices=c('<1'='0','>=1'='1')))
            )
        ),
        fluidRow(
          box(width=12, status = 'warning', title = 'COMPLEJIDAD ICP', 
              plotlyOutput('DISTANCIA') %>% withSpinner())
        ),
        fluidRow(
          box(width = 12, status = 'warning', title = 'COMPLEJIDAD',
              DTOutput('COMPLEJIDAD') %>% withSpinner(),
              downloadBttn('DESCARGAR_COMPLEJIDAD','Descargar')
          )
        )
      ),
      ### PROXIMIDAD ----
      tabItem(
        tabName="PROXIMIDAD", 
        div(id='proximi_id', style='font:bold 100% ;color:white;',
            column(width=3, selectizeInput('PRODU3','PRODUCTO',choices=c(''))),
            column(width=3, selectizeInput('SECTOR3','SECTOR',choices=c(''), multiple=TRUE)),
            column(width=3, selectizeInput('ANO3','AÑO',choices=c(''))),
            column(width=3, sliderInput("PROXIMIDAD", "PROXIMIDAD", min=0,max=100,value=c(0,100)))
        ),
        box(width=12, status = 'warning', title = 'PROXIMIDAD ENTRE PRODUCTO Y COMPLEJIDAD',
            plotlyOutput("GRAF_PROXI") %>% withSpinner()
        ),
        box(width=12, status = 'warning', title = 'BASE DE DATOS',
            tags$head(tags$style("#TAB_PROXI{font: 100% ;text-align:left;justify-content:center;")),
            DTOutput('TAB_PROXI') %>% withSpinner(),
            downloadBttn('DESCARGAR_PROXIMIDAD','Descargar')
        )
      ),
      
      ### RANKING ----
      tabItem(
        tabName="RANKING", 
        fluidRow(
          div(id='ranking_id', style='font:bold 100% ;color:white;',
              column(width = 3, selectizeInput('DEPTO5','DEPARTAMENTO',choices=c(''))),
              column(width = 3, selectizeInput('ANO5','AÑO',choices=c('2020'))),
              br()
          )
        ),
        fluidRow(
          box(width=12, status = 'warning',  title = 'RANKING',
              plotlyOutput('GRAF_RANKI') %>% withSpinner() 
          )
        ),
        fluidRow(
          box(width=12, status = 'warning', title = 'BASE DE DATOS',
              DTOutput('TAB_RANKI') %>% withSpinner(),
              downloadBttn('DESCARGAR_RANKING','Descargar')
          )
        )
      ),
      ### TIPOLOGIA ----
      tabItem(
        tabName='TIPOLOGIA',
        fluidRow(
          box(width = 3, status = 'warning', title = 'CARGAR BASE DE DATOS', style='color:black;text-align:left;height:520px;',
              tags$div(id='data_base', 
                       fileInput(inputId="file", label="CARGAR PLANTILLA .XLSX", multiple=FALSE, accept=c(".xlsx")),
                       materialSwitch(inputId="usarPlantilla", label="USAR LA PLANTILLA", status="primary", right=TRUE),
                       materialSwitch(inputId="nombreFilas", label="NOMBRE DE FILAS", status="success", right=TRUE),
                       hr(),
                       actionBttn('ANALIZAR','ANALIZAR', color='primary'),
                       actionBttn('RESTAURAR','RESTAURAR', color='primary', size='sm')
              )
          ),
          box(width = 9, status = 'warning', title = 'BASE DE DATOS', style='text-align:right;height:520px;',
              h4(textOutput('INFODATA'), style='text-align:right;'),
              DTOutput('HEADBASE')
          )
        ),
        fluidRow(
          tabBox(width=12,
                 tabPanel(title="CLUSTER", plotlyOutput('GRAFCLUS') ),
                 tabPanel(title="DENDOGRAMA", plotOutput('GRAFDEND') ),
                 tabPanel(title="TABLA", 
                          fluidRow(
                            column(width=3, selectizeInput('filtroCluster','FILTRAR CLUSTER',choice='', multiple=TRUE))
                            ),
                            DTOutput('TABLACLUSTER'),
                            downloadBttn('DESCARGAR_TIPOLOGIA','Descargar')
                 ),
                 tabPanel(title="RESUMEN", 
                          selectizeInput('lista_cluster','Selecionar Variable',choices=''),
                          DTOutput('RESUMEN_TIPOLOGIA') )
          )
        )
      )
      ### FIN TABS ----
    )
  })
    
  ### FUNCIONES ----
  TablaBase<-function(BASE){
    DATA<-BASE %>% 
      DT::datatable(rownames=FALSE, escape=FALSE, selection='none', filter='top', extensions=c('Scroller'), style='bootstrap4', class='cell-border stripe', 
                    options=list(scrollY=300, scrollX=TRUE, scroller=TRUE, deferRender=TRUE, dom='trip'))
    return(DATA)
  }
  
  Money <- function(x,d=2){
    if(is.numeric(x)){
      y<-paste("$",format(round(x,digits=d),big.mark=","))
    }else{
      x<-as.numeric(x)
      y<-paste("$",format(round(x,digits=d),big.mark=","))
      }
    return(y)
  }
  
  Porcen <- function(x){
    if(is.numeric(x)){
      y<-paste(format(round((x*100),1),big.mark=','),"%")
    }else y<-paste(x,"%")
    return(y)
  }
  
 ### QUERY GENERALIZADA
  SQL_QUERY <- function(SELECT=NULL,FROM=NULL,WHERE=NULL){
    tryCatch({
      cnx <- dbConnect(RMySQL::MySQL(), host='datlas.cncszlsaoohv.us-east-2.rds.amazonaws.com',
                       dbname='datlas', port=3306, user='admin', password='datlasupb2021')
      if(is.null(SELECT)) SELECT='*' 
      Query<-dbGetQuery(cnx, paste0("SELECT ",SELECT," FROM ",FROM))
      if(!is.null(WHERE))  
        Query<-dbGetQuery(cnx, paste0("SELECT ",SELECT," FROM ",FROM," WHERE ",WHERE))
    },error=function(cond){
      dbDisconnect(cnx)
    },warning=function(cond){
      dbDisconnect(cnx)
    },finally={
      dbDisconnect(cnx)
    })
    return(data.frame(Query))
  }
  
  ### QUERY PARTICULAR PARA EXPORTACIONES
  SQL_QUERY_EXP <- function(SELECT=NULL,FROM=NULL,WHERE=NULL){
    tryCatch({
      if(input$tabs=='EXPORTACIONES'){
        if(input$BASE1=='EXPORTACIONES'){
          if(input$ANO1=='2020') tabla='Exportx2020'
          if(input$ANO1=='2019') tabla='Exportx2019'
          if(input$ANO1=='2018') tabla='Exportx2018'
        }
        if(input$BASE1=='IMPORTACIONES'){
          if(input$ANO1=='2020') tabla='Importx2020'
        }
      }
      cnx <- dbConnect(RMySQL::MySQL(), host='datlas.cncszlsaoohv.us-east-2.rds.amazonaws.com',
                       dbname='datlas', port=3306, user='admin', password='datlasupb2021')
      FROM<-tabla
      if(is.null(SELECT)) SELECT='*' 
      Query<-dbGetQuery(cnx, paste0("SELECT ",SELECT," FROM ",FROM))
      if(!is.null(WHERE))  
        Query<-dbGetQuery(cnx, paste0("SELECT ",SELECT," FROM ",FROM," WHERE ",WHERE))
    },error=function(cond){
      dbDisconnect(cnx)
    },warning=function(cond){
      dbDisconnect(cnx)
    },finally={
      dbDisconnect(cnx)
    })
    return(data.frame(Query))
  }
  

  
  DATA<-reactiveValues()
  
  ### GLOSARIO ----
  observe({
    DATA$GLOSARIO <- readxl::read_excel("GLOSARIO.xlsx", sheet='GLOSARIO') %>% data.frame()
  })
  ### DATABASE
  output$GLOSARIO <- DT::renderDataTable({
    if(is.null(DATA$GLOSARIO)) return()
    DATA$GLOSARIO %>% 
      DT::datatable(rownames=FALSE, selection='none', style='bootstrap4', class='cell-border stripe', extensions=c('Responsive','Scroller'),
                    options=list(scrollY=500, scrollX=10, scroller=FALSE, deferRender=TRUE, dom='ftri'))
  })
  
  ### DESCRIPTIVAS ----
  observe({
    isolate({ 
      qr_depto<-paste0("DEPARTAMENTO IN ('VALLE DEL CAUCA')")
      BASE <- SQL_QUERY(FROM='INDICE_APTITUD_MUN',WHERE=qr_depto)
      DATA$DESPLAZAMIENTO <- SQL_QUERY(FROM='DESPLAZAMIENTO') 
      updateSelectizeInput(session,'VARIABLE0', choices=c(colnames(BASE[,-c(1:6)])), server=TRUE)
      updateSelectizeInput(session,'VARIABLE1', choices=c(colnames(BASE)), server=TRUE)
      updateSelectizeInput(session,'DEPTO0', choices=c(unique(BASE$DEPARTAMENTO),'TODOS'), server=TRUE, selected='VALLE DEL CAUCA')
      updateSelectizeInput(session,'MUNICIPIO0', choices=c(unique(BASE$MUNICIPIO),'TODOS'), server=TRUE)
      updateSelectizeInput(session,'PRODU0','PRODUCTO', choices=c(unique(BASE$CULTIVO)), server=TRUE)
      updateSelectizeInput(session,'ANO0', choices=c(unique(BASE$ANO)), server=TRUE)
      DATA$BASE0 <- BASE
      DATA$TEXT_GRAF=='HIS'
    })
  })
  
  observeEvent(input$BASE0,{
    if(is.null(input$BASE0)) return()
    if(input$BASE0=='IAM'){
      BASE <- NULL
      updateSelectizeInput(session,'DEPTO0', choices=c(SQL_QUERY(SELECT='DISTINCT DEPARTAMENTO',FROM='INDICE_APTITUD_MUN'),'TODOS'), server=TRUE, selected='VALLE DEL CAUCA')
      qr_depto<-paste0("DEPARTAMENTO IN ('",paste(input$DEPTO0, collapse="','"),"')")
      BASE <- SQL_QUERY(FROM='INDICE_APTITUD_MUN',WHERE=qr_depto)
    }
    if(input$BASE0=='EVA'){
      BASE <- NULL
      updateSelectizeInput(session,'DEPTO0', choices=c(SQL_QUERY(SELECT='DISTINCT DEPARTAMENTO',FROM='EVA'),'TODOS'), server=TRUE, selected='VALLE DEL CAUCA')
      qr_depto<-paste0("DEPARTAMENTO IN ('",input$DEPTO0,"')")
      BASE <- SQL_QUERY(FROM='EVA',WHERE=qr_depto)
    }
    DATA$FILTRO <- BASE
  })
  
  observeEvent(input$DEPTO0,{
    if(is.null(input$DEPTO0)) return()
    ifelse(input$DEPTO0=='TODOS' || nchar(input$DEPTO0)==0, qr_depto<-paste0("DEPARTAMENTO IS NOT NULL"), qr_depto<-paste0("DEPARTAMENTO IN ('",paste(input$DEPTO0, collapse="','"),"')"))
    
    if(input$BASE0=='IAM')FILTRO <- SQL_QUERY(FROM='INDICE_APTITUD_MUN',WHERE=qr_depto)
    if(input$BASE0=='EVA')FILTRO <- SQL_QUERY(FROM='EVA',WHERE=qr_depto)
    
    updateSelectizeInput(session,'VARIABLE0', choices=c(colnames(FILTRO[,-c(1:6)])), server=TRUE)
    updateSelectizeInput(session,'VARIABLE1', choices=c(colnames(FILTRO)), server=TRUE)
    updateSelectizeInput(session,'MUNICIPIO0', choices=c(unique(FILTRO$MUNICIPIO),'TODOS'), server=TRUE)
    updateSelectizeInput(session,'PRODU0','PRODUCTO', choices=c(unique(FILTRO$CULTIVO)), server=TRUE)
    updateSelectizeInput(session,'ANO0', choices=c(unique(FILTRO$ANO)), server=TRUE)
    DATA$BASE0 <- FILTRO
  })
  
  filtroDescrip <- function(Data){
    ifelse(input$MUNICIPIO0!='TODOS' || nchar(input$MUNICIPIO0)==0, Data<-Data %>% dplyr::filter(MUNICIPIO %in% input$MUNICIPIO0), Data)
    ifelse(input$PRODU0!='TODOS' || nchar(input$PRODU0)==0, Data<-Data %>% dplyr::filter(CULTIVO %in% input$PRODU0), Data)
    ifelse(input$ANO0!='TODOS' || nchar(input$ANO0)==0, Data<-Data %>% dplyr::filter(ANO %in% input$ANO0), Data)
    ifelse(input$CONFLICTO0!='TODOS' || nchar(input$CONFLICTO0)==0, Data<-Data %>% dplyr::filter(POST_CONFLITO %in% input$CONFLICTO0), Data)
    ifelse(input$PRODUCTOR0!='TODOS' || nchar(input$PRODUCTOR0)==0, Data<-Data %>% dplyr::filter(PRODUCTOR %in% input$PRODUCTOR0), Data)
    return(Data)
  }
  
  output$GRAF_DESCRIP1<-plotly::renderPlotly({
    if(is.null(DATA$BASE0)) return()
    if(nchar(input$VARIABLE0)==0) return()
    #
    Data<-filtroDescrip(DATA$BASE0)
    if(nrow(Data)==0) return(NULL)
    
    if(input$BASE0=='IAM') FUENTE <- 'https://sipra.upra.gov.co'
    else FUENTE <- 'https://www.agronet.gov.co/estadistica/Paginas/home.aspx?cod=1'
    
    Data[,input$VARIABLE0] <- Data[,input$VARIABLE0] %>% as.numeric()
    
    vline <- function(x = 0, color = "red") {
      list(type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = x, x1 = x, line = list(color = color))
    }
      
    meanV0 <- mean(as.numeric(Data[,input$VARIABLE0])) %>% round(digits=3)
    
    plot_ly(Data, x=Data[,input$VARIABLE0], type='box', colors='Blues',
            text=~paste0(input$VARIABLE0,': ',get(input$VARIABLE0),'<br>', 
                         'CULTIVO: ',Data$CULTIVO,'<br>', 
                         'MUNICIPIO: ',Data$MUNICIPIO,'<br>', 
                         'DEPARTAMENTO: ',Data$DEPARTAMENTO ), 
            name=input$VARIABLE0) %>%
      layout(shapes=list(type='line', x0=meanV0, x1=meanV0, y0=0, y1=1, yref="paper", line=list(dash='dot', color='red', width=1)),
             title=list(text=paste0('DEPARTAMENTO ',input$DEPTO0,'<br>',
                                    'DISTRIBUCION DE ',input$VARIABLE0,
                                    '<br><sup>Fuente: ',FUENTE,'</sup>')))
  })
  
  output$GRAF_DESCRIP2<-renderPlotly({
    if(is.null(DATA$BASE0)) return()
    if(nchar(input$VARIABLE0)==0) return()
    if(nchar(input$VARIABLE1)==0) return()
    #
    Data<-filtroDescrip(DATA$BASE0)
    if(nrow(Data)==0) return(NULL)
    
    Data[,input$VARIABLE0] <- Data[,input$VARIABLE0] %>% as.numeric()
    Data <- Data %>% arrange(input$VARIABLE0)
    
    if(input$BASE0=='IAM') FUENTE <- 'https://sipra.upra.gov.co'
    else FUENTE <- 'https://www.agronet.gov.co/estadistica/Paginas/home.aspx?cod=1'
    
    if(input$VARIABLE1=='CULTIVO'){
      DATA$TEXT_GRAF <- 'HIS'
      if(nrow(Data)<2) showNotification(paste0('Necesita al menos 2 puntos!'), type="warning")
      x<-Data[,input$VARIABLE0]
      fit <- density(x)
      HIS <- plot_ly(x=x, type="histogram", name="HISTOGRAMA", histnorm="probability",
                     text=~paste0(input$VARIABLE0,': ',Data[,input$VARIABLE0],'<br>', 
                                  'CULTIVO: ',Data$CULTIVO,'<br>', 
                                  'MUNICIPIO: ',Data$MUNICIPIO,'<br>', 
                                  'DEPARTAMENTO: ',Data$DEPARTAMENTO ) ) %>%
        layout(title=list(text=paste0(
          'DEPARTAMENTO ',input$DEPTO0,'<br>',
          'DISTRIBUCION DE ',input$VARIABLE0,
          '<br><sup>Fuente: ',FUENTE,'</sup>')),
          xaxis=list(title=input$VARIABLE1), yaxis=list(title="FRECUENCIA"), yaxis2=list(overlaying="y", side="right"))
      
    }else{
      if(class(Data[,input$VARIABLE1])=='numeric'){
        DATA$TEXT_GRAF <- 'DIS'
        PARAMETRO <- input$VARIABLE0
        plot_ly(Data, x=~get(input$VARIABLE0), y=~get(input$VARIABLE1), name=input$VARIABLE1, type='scatter',mode='markers', 
                marker = list(size=20, opacity=0.8, color=~get(input$VARIABLE0), colorscale=list(c(0,1),c('yellow',"green")), line=list(color='rgb(231,99,250)',width=1), showscale=TRUE),
                text=~paste0(input$VARIABLE0,': ',get(input$VARIABLE0),'<br>',
                             'CULTIVO: ',Data$CULTIVO,'<br>', 
                             'MUNICIPIO: ',Data$MUNICIPIO,'<br>', 
                             'DEPARTAMENTO: ',Data$DEPARTAMENTO )
        ) %>%
          layout(title=list(text=paste0('DEPARTAMENTO ',input$DEPTO0,'<br>',input$VARIABLE0,' VS ',input$VARIABLE1,
                                        '<br><sup>Fuente: ',FUENTE,'</sup>')),
                 xaxis=list(title=input$VARIABLE0), yaxis=list(title=~MUNICIPIO))
      }else{
        DATA$TEXT_GRAF <- 'BAR'
        plot_ly(Data, x=~get(input$VARIABLE0), y=~get(input$VARIABLE1), type="bar",
                marker=list(color=~get(input$VARIABLE0), colorscale = list(c(0, 1), c('yellow',"green")), showscale = TRUE),
                text=~paste0(input$VARIABLE0,': ',get(input$VARIABLE0),'<br>',
                             'TOTAL: ',sum(get(input$VARIABLE0)),'<br>',
                             'CULTIVO: ',Data$CULTIVO,'<br>', 
                             'MUNICIPIO: ',Data$MUNICIPIO,'<br>', 
                             'DEPARTAMENTO: ',Data$DEPARTAMENTO )
        ) %>%
          layout(title=list(text=paste0('DEPARTAMENTO ',input$DEPTO0,'<br>',
                                        input$VARIABLE0,' VS ',input$VARIABLE1,
                                        '<br><sup>Fuente: ',FUENTE,'</sup>')),
                 xaxis=list(title=input$VARIABLE0), yaxis=list(title=~MUNICIPIO))
      }
    }
  })
  
  output$TAB_DESCRIP <- renderDataTable({
    if(is.null(DATA$BASE0)) return()
    
    Data<-filtroDescrip(DATA$BASE0)
    if(nrow(Data)==0) return(NULL)
    if(input$BASE0=='IAM')
      titulo <- c("Departamento",	"Cultivo",HTML("A&ntilde;o"),	'Municipio', 'Post conflito','Productor',	HTML('&Aacute;rea Sembrada (ha)'),	HTML('&Aacute;rea Cosechada (ha)'),	HTML('Producci&oacute;n (t)'),	'Rendimiento (t/ha)',	'Aptitud Alta (ha)',	'Aptitud Baja (ha)',	'Aptitud Mediab (ha)','Exclusi&oacute;n Legal (ha)',	'No Apta (ha)',	'Total (ha)',	'Aptitud Alta %', 'Aptitud Baja %',	'Aptitud Media%',	'No Apta%')
    else
      titulo <- c('Departamento','Cultivo',HTML("A&Ntilde;O"),	'Municipio', 'Post conflito','Productor', HTML('&Aacute;rea Sembrada (ha)'),	HTML('&Aacute;rea Cosechada (ha)'),	HTML('Producci&oacute;n (t)'),	'Rendimiento (t/ha)')
    
    DATA$DOWN <- Data
    colnames(Data)<-titulo
    
    Data %>% TablaBase()
  })
 
  output$DESCARGAR_DESCRIPTIVAS <- downloadHandler(
    filename = paste0("TABLA_DESCRIPTIVAS_",Sys.Date(),".xlsx"),
    content = function(file){
     xlsx::write.xlsx2(DATA$DOWN, file)
    })
  
  
  output$TAB_DESPLAZA <- renderTable({
    if(is.null(DATA$DESPLAZAMIENTO)) return()
    Data<-DATA$DESPLAZAMIENTO
    Data$P_M_C_D_M_3H <- Porcen(Data$P_M_C_D_M_3H)
    ifelse(input$DEPTO0!='TODOS', Data<-Data %>% dplyr::filter(DEPARTAMENTO %in% input$DEPTO0), Data)
    Data
  })
  
  output$TIEMPO_DESPLAZA <- renderText({
    if(input$tabs!='DESCRIPTIVAS') return()
    Data<-DATA$DESPLAZAMIENTO
    Data$P_M_C_D_M_3H <- Porcen(Data$P_M_C_D_M_3H)
    ifelse(input$DEPTO0!='TODOS', Data<-Data %>% dplyr::filter(DEPARTAMENTO %in% input$DEPTO0), Data)
    
    if(nrow(Data)>1) return()
    paste0('El porcentaje de los municipios que tienen un tiempo de desplazamiento menor a tres horas a la capital del departamento del ',input$DEPTO0,' es de: ',Data$P_M_C_D_M_3H)
    
  })
  
  output$TEXTO_GRAFICO2 <- renderText({
    if(input$tabs!='DESCRIPTIVAS') return()
    
    if(DATA$TEXT_GRAF=='HIS') TEXTO<-paste0('El Histograma nos da indicios del comportamiento de la variable en términos de donde se concentra sus valores más frecuentes, por ejemplo, tenemos la variable rendimiento cuando en el grafico observamos una barra que me demarca una altura máxima, lo que nos indica que es que en los valores que incluye el ancho de la barra es el rango de rendimientos que se presentan con mayor frecuencia en los municipios del departamento seleccionado.')
    
    if(DATA$TEXT_GRAF=='BAR') TEXTO<-paste0('El diagrama de barras representa gráficamente un conjunto de valores por medio de barras con longitud proporcional a los valores representados. Por ejemplo, para un determinado año y departamento, las barras de mayor longitud, representadas de otro color, indican hacía los valores obtenidos por los municipios en cuanto a la variable observada.')
    
    if(DATA$TEXT_GRAF=='DIS') TEXTO<-paste0('El grafico de dispersión es el cruce de dos variables que nos explica el comportamiento de ambas en simultaneo, permitiendo observar si existe algún tipo de relacione en el comportamiento de ellas, dado que la variable principal se ubica en el eje horizontal donde se entiende que los valores más altos de la variable se ubican hacia la derecha, en el mismo sentido tenemos la segunda variable sobre el eje vertical donde sus valores más altos se encuentran ubicados en la parte de arriba.')
   
    TEXTO 
  })

  
  ### EXPORTACIONES ----
  observeEvent(input$tabs,{
    if(input$tabs!='EXPORTACIONES') return()
    
    observe({
      if(input$tabs!='EXPORTACIONES') return()
      DATA$BASE1 <- NULL
      qr_depto<-paste0("NOMDEPTO IN ('ANTIOQUIA')")
      DATA$BASE1 <- SQL_QUERY(FROM='Exportx2020',WHERE=qr_depto)
    })
    
    observe({
      if(input$tabs!='EXPORTACIONES') return()
      isolate({
        DATA$BASE1$POSAR <- DATA$BASE1$POSAR %>% str_remove_all("'")
        BASE <- DATA$BASE1
        updateSelectizeInput(session,'DEPTO1', choices=c(SQL_QUERY_EXP(SELECT='DISTINCT NOMDEPTO'),'TODOS'), server=TRUE, selected='ANTIOQUIA')
        updateSelectizeInput(session,'PAIS1', choices=c(unique(BASE$NOMPAIS),'TODOS'), server=TRUE)
        updateSelectizeInput(session,'SECTOR1', choices=c(unique(BASE$SECTOR),'TODOS'), server=TRUE)
        updateSelectizeInput(session,'PRODU1', choices=c(unique(BASE$NOMPRODUCTO),'TODOS'), server=TRUE)
        updateSelectizeInput(session,'POSAR1', choices=c(unique(BASE$POSAR),'TODOS'), server=TRUE)
      })
    })
    
    observeEvent(input$BASE1,{
      if(input$tabs!='EXPORTACIONES') return()
      if(nchar(input$BASE1)==0) return()
      BASE <- NULL
      updateSelectizeInput(session,'DEPTO1', choices=c(SQL_QUERY_EXP(SELECT='DISTINCT NOMDEPTO'),'TODOS'), server=TRUE, selected='ANTIOQUIA')
      qr_depto<-paste0("NOMDEPTO IN ('ANTIOQUIA')")
      BASE <- SQL_QUERY_EXP(WHERE=qr_depto)
      BASE$POSAR <- BASE$POSAR %>% str_remove_all("'")
      DATA$FILTRO <- BASE
    })
    
    observeEvent(input$DEPTO1,{
      if(input$tabs!='EXPORTACIONES') return()
      if(is.null(input$BASE1)) return()
      if(nchar(input$BASE1)==0) return()

      ifelse(input$DEPTO1=='TODOS' || nchar(input$DEPTO1)==0, qr_depto<-paste0("NOMDEPTO IS NOT NULL"), qr_depto<-paste0("NOMDEPTO IN ('",paste(input$DEPTO1, collapse="','"),"')"))
      FILTRO <- SQL_QUERY_EXP(WHERE=qr_depto)
      
      updateSelectizeInput(session,'PAIS1', choices=c(unique(FILTRO$NOMPAIS),'TODOS'), server=TRUE)
      updateSelectizeInput(session,'SECTOR1', choices=c(unique(FILTRO$SECTOR),'TODOS'), server=TRUE)
      updateSelectizeInput(session,'PRODU1', choices=c(unique(FILTRO$NOMPRODUCTO),'TODOS'), server=TRUE)
      updateSelectizeInput(session,'POSAR1', choices=c(unique(FILTRO$POSAR),'TODOS'), server=TRUE)
      DATA$BASE1 <- FILTRO
    })
    
    filtroExport <- function(Data){
      if(nchar(input$BASE1)==0) return()
      ifelse(input$DEPTO1!='TODOS' | nchar(input$DEPTO1)==0, Data<-Data %>% dplyr::filter(NOMDEPTO %in% input$DEPTO1), Data)
      ifelse(input$PAIS1!='TODOS' | nchar(input$PAIS1)==0, Data<-Data %>% dplyr::filter(NOMPAIS %in% input$PAIS1), Data)
      ifelse(input$SECTOR1!='TODOS' || nchar(input$SECTOR1)==0, Data<-Data %>% dplyr::filter(SECTOR %in% input$SECTOR1), Data)
      ifelse(input$PRODU1!='TODOS' || nchar(input$PRODU1)==0, Data<-Data %>% dplyr::filter(NOMPRODUCTO %in% input$PRODU1), Data)
      ifelse(input$POSAR1!='TODOS' || nchar(input$POSAR1)==0, Data<-Data %>% dplyr::filter(POSAR %in% input$POSAR1), Data)
      ifelse(nchar(input$ANO1)==0, Data<-Data %>% dplyr::filter(ANO %in% input$ANO1), Data)
      return(Data)
    }
    
    output$DESTINO<-renderPlotly({
      if(input$tabs!='EXPORTACIONES') return()
      if(is.null(DATA$BASE1)) return()
      if(nchar(input$BASE1)==0) return()
      
      EXPORT<- filtroExport(DATA$BASE1)
      if(nrow(EXPORT)==0) return(NULL)
      Data<-EXPORT
      Data$FOBDOL<-as.numeric(Data$FOBDOL)
      
      plot_ly(Data, y=~NOMPAIS, x=~FOBDOL, type="bar", name=input$BASE1,
              marker=list(color=~FOBDOL, colorscale=list(c(0, 1), c('yellow',"green")), showscale = TRUE),
              text=~paste0('FOBDOL: ',Money(Data$FOBDOL),'<br>',
                           'TOTAL: ',Money(sum(Data$FOBDOL)),'<br>',
                           'PAÍS: ',Data$NOMPAIS,'<br>',
                           'SECTOR: ',Data$SECTOR,'<br>',
                           'PRODUCTO: ',Data$NOMPRODUCTO,'<br>',
                           'POSAR: ',Data$POSAR )) %>%
        layout(title=list(text=paste0(input$BASE1,' POR PAÍS ',input$PAIS1,'<br><sup>Fuente: https://sipra.upra.gov.co/</sup>')),
               xaxis=list(title=~FOBDOL), yaxis=list(title=~NOMPAIS))
    })
    
    output$TREEMAP<-renderPlotly({
      if(input$tabs!='EXPORTACIONES') return()
      if(is.null(DATA$BASE1)) return()
      if(nchar(input$BASE1)==0) return()
      
      EXPORT<- filtroExport(DATA$BASE1)
      if(nrow(EXPORT)==0) return(NULL)
      
      data0 <- EXPORT %>% dplyr::select(SECTOR,FOBDOL) %>% dplyr::group_by(SECTOR) %>% dplyr::summarise(sum(FOBDOL))
      dataf <- data.frame(data0$SECTOR, NOMPRODUCTO='', data0$`sum(FOBDOL)`)
      colnames(dataf) <- c('labels','parents','values')
      
      data <- EXPORT %>% dplyr::select(SECTOR,NOMPRODUCTO,FOBDOL) %>% dplyr::group_by(NOMPRODUCTO,SECTOR) %>% dplyr::summarise(sum(FOBDOL), .groups='drop')
      colnames(data) <- c('labels','parents','values')
      
      df1 <- rbind(dataf,data)
      df1 <- na.omit(df1)
      df1$values<-as.numeric(df1$values)
      
      plot_ly(type="treemap", labels=df1$labels, parents=df1$parents, values=df1$values, domain=list(column=0),
              text=~paste0('SECTOR: ',df1$parents,'<br>PRODUCTO: ',df1$labels,'<br>FOBDOL: ',Money(df1$values) ))
    })
    
    ### DATABASE
    output$EXPORTACIONES<-DT::renderDataTable({
      if(is.null(input$BASE1)) return()
      if(input$tabs!='EXPORTACIONES') return()
      if(nchar(input$BASE1)==0) return()
      
      Data <- filtroExport(DATA$BASE1) 
      DATA$DOWN <- Data
      if(nrow(Data)==0) return(NULL)
      Data %>% TablaBase()
    })
    
  })
  
  output$DESCARGAR_EXPORTACIONES <- downloadHandler(
    filename = paste0("TABLA_EXPORTACIONES_",Sys.Date(),".xlsx"),
    content = function(file){
      xlsx::write.xlsx2(DATA$DOWN, file)
    })
  
  
  ### COMPLEJIDAD ----

  observe({
    if(input$tabs!='COMPLEJIDAD') return()
    DATA$BASE2 <- NULL
    DATA$BASE2 <- SQL_QUERY(FROM='DISTANCIA')
  })
    
  observe({
    if(input$tabs!='COMPLEJIDAD') return()
    isolate({
      DATA$BASE2$COD_PRODUCTO <- DATA$BASE2$COD_PRODUCTO %>% str_remove_all("'")
      BASE <- DATA$BASE2
      updateSelectizeInput(session, 'DEPTO2', choices=c(sort(colnames(DATA$BASE2[,c(8:37)]))), server=TRUE, selected='VALLE_DEL_CAUCA')
      updateSelectizeInput(session, 'SECTOR2', choices=c(unique(BASE$SECTOR),'TODOS'), server=TRUE)
      updateSelectizeInput(session, 'SUBSECTOR2', choices=c(unique(BASE$SUB_SECTOR),'TODOS'), server=TRUE)
      updateSelectizeInput(session, 'PRODU2', choices=c(unique(BASE$NOMBRE_PRODUCTO),'TODOS'), server=TRUE)
      updateSelectizeInput(session, 'POSAR2', choices=c(unique(BASE$COD_PRODUCTO),'TODOS'), server=TRUE)
      updateSelectizeInput(session, 'ANO2', choices=c(unique(BASE$ANO)), server=TRUE)
    })
  })
  
    filtroComplej <- function(Data){
      if(nchar(input$DEPTO2)==0) return()
      ifelse(nchar(input$SECTOR2)==0, Data, Data<-Data %>% dplyr::filter(SECTOR %in% input$SECTOR2))
      ifelse(nchar(input$SUBSECTOR2)==0, Data, Data<-Data %>% dplyr::filter(SUB_SECTOR %in% input$SUBSECTOR2))
      ifelse(nchar(input$PRODU2)==0, Data, Data<-Data %>% dplyr::filter(NOMBRE_PRODUCTO %in% input$PRODU2))
      ifelse(nchar(input$POSAR2)==0, Data, Data<-Data %>% dplyr::filter(COD_PRODUCTO %in% input$POSAR2))
      ifelse(nchar(input$ANO2)==0, Data, Data<-Data %>% dplyr::filter(ANO %in% input$ANO2))
      ifelse(input$RCA==0, Data<-Data %>% dplyr::filter(get(paste0(input$DEPTO2,'_RCA'))<1), Data<-Data %>% dplyr::filter(get(paste0(input$DEPTO2,'_RCA'))>=1))
      return(Data)
    }
    
    output$DISTANCIA<-plotly::renderPlotly({
      if(input$tabs!='COMPLEJIDAD') return()
      if(is.null(DATA$BASE2)) return()
      if(nchar(input$DEPTO2)==0) return()
      
      meanPCI<-mean(DATA$BASE2$PCI) %>% round(digits=4)
      Data<-filtroComplej(DATA$BASE2)
      if(nrow(Data)==0) return(NULL)
      
      hline <- function(y = 0, color = "red") {
        list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = y, y1 = y, line = list(color = color))
      }
      
      plot_ly(x=Data[,input$DEPTO2], y=Data[,'PCI'], type='scatter', mode='markers', color=Data[,'SECTOR'], colors="Set3",
              marker=list(size=log(Data[,'EXPORTANCIONES']), opacity=0.8, line=list(color='black',width=0.1)),
                 text=~paste0('DEPTO: ',input$DEPTO2,'<br>SECTOR: ',Data[,'SECTOR'],'<br>SUBSECTOR: ',Data[,'SUB_SECTOR'],'<br>PRODUCTO: ',Data[,'NOMBRE_PRODUCTO'],
                              '<br>NOMENCLATURA ARANCELARIA: ',Data[,'COD_PRODUCTO'],'<br>EXPORTANCIONES: ',Data[,'EXPORTANCIONES'])) %>% 
        layout(shapes=list(type='line', x0=min(Data[,input$DEPTO2]), x1=max(Data[,input$DEPTO2]), y0=meanPCI, y1=meanPCI, line=list(dash='dot', color='red', width=1)),
          title=list(text=paste0("Potencial de los Productos para ",input$DEPTO2,"<br><sup>Fuente: DATLAS</sup>")), 
               xaxis=list(title='DISTANCIAS'), yaxis=list(title="COMPLEJIDAD (I.C.P.)")) 
    }) 
    
    
    ### DATABASE
    output$COMPLEJIDAD<-DT::renderDataTable({
      if(input$tabs!='COMPLEJIDAD') return()
      if(is.null(DATA$BASE2)) return()
      if(nchar(input$DEPTO2)==0) return()
      
      Data <- filtroComplej(DATA$BASE2) 
      if(nrow(Data)==0) return(NULL)
      Data <- Data %>% dplyr::select(COD_PRODUCTO,SECTOR,SUB_SECTOR,EXPORTANCIONES,ANO,PCI,all_of(input$DEPTO2),all_of(paste0(input$DEPTO2,'_RCA')),NOMBRE_PRODUCTO) 
      
      DATA$DOWN <- Data
      Data %>% DT::datatable(rownames=FALSE, selection='single', class='cell-border stripe', extensions=c('Responsive','Scroller'),
                             options=list(scrollY=300, scrollX=100, scroller=TRUE, deferRender=TRUE, dom='trip'))
    })
    
    output$DESCARGAR_COMPLEJIDAD <- downloadHandler(
      filename = paste0("TABLA_COMPLEJIDAD_",Sys.Date(),".xlsx"),
      content = function(file){
        xlsx::write.xlsx2(DATA$DOWN, file)
      })
    
    

  
  
  ### PROXIMIDAD ----
  observeEvent(input$tabs,{
    if(input$tabs!='PROXIMIDAD') return()
    
    observe({
      if(input$tabs!='PROXIMIDAD') return()
      DATA$BASE3 <- NULL
      DATA$BASE3 <- SQL_QUERY(FROM='PROXIMIDAD')
    })

    observe({
      if(input$tabs!='PROXIMIDAD') return()
      BASE <- DATA$BASE3
      updateSelectizeInput(session, 'PRODU3', choices=c(sort(colnames(BASE[,-c(1:7)]))), server=TRUE)
      updateSelectizeInput(session, 'SECTOR3', choices=c(unique(BASE$SECTOR)), server=TRUE)
      updateSelectizeInput(session, 'ANO3', choices=c(unique(BASE$ANO)), server=TRUE)
    })
    
    filtroProximi <- function(base){
      Data<-base %>% dplyr::select(COD_PRODUCTO,SECTOR,SUB_SECTOR,EXPORTACIONES,ANO,PCI,all_of(input$PRODU3),NOMBRE_PRODUCTO)
      Data[,input$PRODU3] <- as.numeric(Data[,input$PRODU3])*100
      ifelse(nchar(input$SECTOR3)==0, Data, Data<-Data %>% dplyr::filter(SECTOR %in% input$SECTOR3))
      ifelse(nchar(input$ANO3)==0, Data, Data<-Data %>% dplyr::filter(ANO %in% input$ANO3))
      Data <- Data %>% dplyr::filter(get(input$PRODU3)%in%c(min(input$PROXIMIDAD):max(input$PROXIMIDAD)))
      return(Data)
    }
    
    output$GRAF_PROXI<-renderPlotly({
      if(input$tabs!='PROXIMIDAD') return()
      if(is.null(DATA$BASE3)) return()
      if(nchar(input$PRODU3)==0) return()
      
      Data<-filtroProximi(DATA$BASE3)
      if(nrow(Data)==0) return(NULL)
      
      plot_ly(x=Data[,input$PRODU3], y=Data[,'PCI'], type='scatter', mode='markers', color=Data[,'SECTOR'], colors="Set3",
              marker=list(size=log(Data[,'EXPORTACIONES']), opacity=0.8, line=list(color='black',width=0.1)),
              text=~paste0('PRODUCTO BASE: ',input$PRODU3,'<br>SECTOR: ',Data[,'SECTOR'],'<br>SUBSECTOR: ',Data[,'SUB_SECTOR'],'<br>PRODUCTO: ',Data[,'NOMBRE_PRODUCTO'],
                           '<br>NOMENCLATURA ARANCELARIA: ',Data[,'COD_PRODUCTO'],'<br>EXPORTACIONES: ',Data[,'EXPORTACIONES'])) %>% 
        layout(title=list(text=paste0("Proximidades entre productos vs complejidad<br><sup>Fuente: DATLAS</sup>")), 
               xaxis=list(title=paste0('Proximidad al ',input$PRODU3)), yaxis=list(title="Complejidad (I.C.P.)"))
    })
    
    
    output$TAB_PROXI<-DT::renderDataTable({
      if(input$tabs!='PROXIMIDAD') return()
      if(is.null(DATA$BASE3)) return()
      if(nchar(input$PRODU3)==0) return()
      
      Data<-filtroProximi(DATA$BASE3)
      if(nrow(Data)==0) return(NULL)
      DATA$DOWN <- Data
      
      dplyr::arrange(Data, desc(input$PRODU3)) %>% 
        DT::datatable(rownames=FALSE, selection='single', class='cell-border stripe', extensions=c('Responsive','Scroller'),
                      options=list(scrollY=300, scrollX=100, scroller=TRUE, deferRender=TRUE, dom='trip'))
    })
    
    
    output$DESCARGAR_PROXIMIDAD <- downloadHandler(
      filename = paste0("TABLA_PROXIMIDAD_",Sys.Date(),".xlsx"),
      content = function(file){
        xlsx::write.xlsx2(DATA$DOWN, file)
      })
   
  })
  
  
  
  ### RANKING ----
  observeEvent(input$tabs,{
    if(input$tabs!='RANKING') return()
    
    observe({
      if(input$tabs!='RANKING') return()
      DATA$BASE5 <- NULL
      DATA$BASE5 <- SQL_QUERY(FROM='RANKING')
    })
    
    observe({
      if(input$tabs!='RANKING') return()
      isolate({
        BASE <- DATA$BASE5
        updateSelectizeInput(session,'DEPTO5', choices=c(unique(BASE$DEPARTAMENTO)), server=TRUE)
        DATA$BASE5 <- BASE
      })
    })
    
    output$GRAF_RANKI<-renderPlotly({
      if(input$tabs!='RANKING') return()
      if(is.null(DATA$BASE5)) return()
      if(nchar(input$DEPTO5)==0) return()
      if(nchar(input$ANO5)==0) return()
      
      Data<-DATA$BASE5
      ifelse(nchar(input$DEPTO5)==0, Data<-Data, Data<-Data %>% dplyr::filter(DEPARTAMENTO%in%input$DEPTO5))
      df_office<-Data %>% dplyr::select(-ANO, -DEPARTAMENTO)
      
      # hallar promedios por grupo
      df_office_avg <-
        df_office %>% 
        arrange(COMPONENTE, episode) %>% 
        mutate(episode_id = row_number()) %>% 
        group_by(COMPONENTE) %>% 
        mutate(
          avg = mean(RANKING),
          episode_mod = episode_id + (9 * COMPONENTE),
          mid = mean(episode_mod)
        ) %>% 
        ungroup() %>% 
        mutate(COMPONENTE = factor(FACTOR))
      
      ### 
      
      df_lines <-
        df_office_avg %>% 
        group_by(COMPONENTE) %>% 
        summarize(
          start_x = min(episode_mod) - 5,
          end_x = max(episode_mod) + 5,
          y = unique(avg)
        ) %>% 
        pivot_longer(
          cols = c(start_x, end_x),
          names_to = "type",
          values_to = "x"
        ) %>% 
        mutate(
          x_group = if_else(type == "start_x", x + .1, x - .1),
          x_group = if_else(type == "start_x" & x == min(x), x_group - .1, x_group),
          x_group = if_else(type == "end_x" & x == max(x), x_group + .1, x_group)
        )
      
      #######
      
      p<-df_office_avg %>% 
        ggplot(aes(episode_mod, RANKING)) +
        geom_hline(
          data = tibble(y = 7:10),
          aes(yintercept = y),
          color = "grey82",
          size = .5
        )
      
      p<-p + 
        geom_segment(
          aes(
            xend = episode_mod,
            yend = avg, 
            color = COMPONENTE,
            color = after_scale(colorspace::lighten(color, .2))
          )
        )
      
      p<-p + 
        geom_line(
          data = df_lines,
          aes(x, y),
          color = "grey40"
        ) +
        geom_line(
          data = df_lines,
          aes(
            x_group, 
            y, 
            color = COMPONENTE, 
            color = after_scale(colorspace::darken(color, .2))
          ),
          size = 2.5
        ) + 
        geom_point(
          aes(size = VALOR, color = COMPONENTE,label= VARIABLE)
        ) 
      
      p<-p + 
        geom_label(
          aes(
            mid, 10.12,
            label = glue::glue(" COMPONENTE {COMPONENTE} "),
            color = COMPONENTE, 
            color = after_scale(colorspace::darken(color, .2))
          ),
          fill = NA,
          family = "Special Elite",
          fontface = "bold",
          label.padding = unit(.2, "lines"),
          label.r = unit(.25, "lines"), 
          label.size = .5
        ) 
      
      
      p<-p + 
        scale_x_continuous(expand = c(.015, .015)) +
        scale_y_continuous(
          expand = c(.03, .03),
          limits = c(0.0, 10.2),
          breaks = seq(0, 10, by = .5),
          sec.axis = dup_axis(name = NULL)
        ) +
        scale_color_manual(
          values = brewer.pal(n=4, name="Paired"),
          guide = FALSE 
        ) +
        scale_size_binned(name = "valores", range = c(.3, 3)) +
        labs(
          x = NULL, 
          y = "INDICE DE COMPETIVIDAD DEL PRODUCTO",
          caption = "FUENTE DATLAS Y ICD"
        ) +
        guides(
          size = guide_bins(
            show.limits = TRUE,
            direction = "horizontal",
            title.position = "top",
            title.hjust = .5
          )
        ) +
        theme(
          legend.position = c(.5, .085),
          legend.key.width = unit(2, "lines")
        ) 
      
      ggplotly(p)
      
    })
    
    
    ### DATABASE
    output$TAB_RANKI<-DT::renderDataTable({
      if(input$tabs!='RANKING') return()
      if(is.null(DATA$BASE5)) return()
      if(nchar(input$DEPTO5)==0) return()
      if(nchar(input$ANO5)==0) return()
      
      Data<-DATA$BASE5
      ifelse(nchar(input$DEPTO5)==0, Data<-Data, Data<-Data %>% dplyr::filter(DEPARTAMENTO%in%input$DEPTO5))
      ifelse(nchar(input$ANO5)==0, Data<-Data, Data<-Data %>% dplyr::filter(ANO%in%input$ANO5))
      DATA$DOWN <- Data
      
      if(nrow(Data)==0) return(NULL)
      Data %>% TablaBase()
    })
    
    output$DESCARGAR_RANKING <- downloadHandler(
      filename = paste0("TABLA_RANKING_",Sys.Date(),".xlsx"),
      content = function(file){
        xlsx::write.xlsx2(DATA$DOWN, file)
      })
    
  })
  
  

  ### TIPOLOGIA ----
  res <- reactiveValues(FILE=NULL, HCPC=NULL, KLUSTER=NULL)
  
  RESTAURAR <- function(){
    res$FILE <- NULL
    res$HCPC <- NULL
    res$KLUSTER <- NULL
    updateMaterialSwitch(session,"nombreFilas",value=FALSE)
  }
  
  observeEvent(input$RESTAURAR,{
    RESTAURAR()
    shinyjs::reset('file')
    updateMaterialSwitch(session,'usarPlantilla',value=FALSE)
  })
  
  observe({
    inFile <- input$file
    if(is.null(inFile)){
      BASE <- SQL_QUERY(FROM='INDICES_PRUEBA')
      res$INICIAL <- data.frame(BASE, row.names=1)
      res$BASE <- res$INICIAL
    }else{
      RESTAURAR()
      ext <- tools::file_ext(inFile$name)
      if(ext=='xlsx'){
        file.rename(inFile$datapath, paste(inFile$datapath, ext, sep="."))
        res$FILE <- readxl::read_xlsx(paste(inFile$datapath, ext, sep=".")) %>% data.frame()
        showNotification(paste0('ARCHIVO CARGADO!'), type="message")
      }else{showNotification(paste0('El archivo no es .xlsx!'), type="error")}
    }
  })
  
  observeEvent(input$usarPlantilla,{
    if(input$usarPlantilla==FALSE){
      res$BASE <- res$INICIAL
    }else{
      if(is.null(res$FILE)){
        updateMaterialSwitch(session,'usarPlantilla',value=FALSE)
        showNotification(paste0('CARGA UNA PLANTILLA!'), type="warning")
      }else{
        res$BASE <- data.frame(res$FILE)
      }
    }
  })
  
  observe({
    shinyjs::toggle('usarPlantilla', condition=!is.null(res$FILE))
    shinyjs::toggle('nombreFilas', condition=!is.null(res$FILE))
  })
  
  observeEvent(input$nombreFilas,{
    if(input$nombreFilas==FALSE) return()
    if(input$usarPlantilla!=FALSE){
      if(nrow(res$FILE)!=length(unique(res$FILE[,1]))){
        updateMaterialSwitch(session,'nombreFilas',value=FALSE)
        showNotification(paste0('Hay Filas Duplicadas'), type="error")
      }else{
        if(input$nombreFilas!=TRUE) 
          res$BASE <- data.frame(res$FILE)
        else res$BASE <- data.frame(res$FILE, row.names=1)  
      }
    }else return()
  })
  
  output$INFODATA <- renderText({
    paste0('BASE DE DATOS CON: ',nrow(res$BASE),' FILAS Y ',ncol(res$BASE),' COLUMNAS.')
  })
  
  output$HEADBASE <- renderDT({
    if(is.null(res$BASE)) return()
    res$BASE %>% 
      DT::datatable(rownames=TRUE, escape=FALSE, selection='none', filter='top', extensions=c('Scroller'), style='bootstrap4', class='cell-border stripe', 
                    options=list(scrollY=300, scrollX=TRUE, scroller=TRUE, deferRender=TRUE, dom='trip'))
  })
  
  observeEvent(input$ANALIZAR,{
    if(is.null(res$BASE)) return()
    tryCatch({
      BASE <- res$BASE %>% na.omit()
      res$PCA <- PCA(BASE, ncp=3, graph=FALSE)
      res$HCPC<- HCPC(res$PCA, graph=FALSE)
      data.clust <- res$HCPC$data.clust
      res$KLUSTER <- data.frame(data.clust)
      showNotification(sprintf('ANALISIS LISTO!'), type="message")
    },error=function(cond){ 
      res$KLUSTER <- NULL
      res$HCPC <- NULL
      showNotification(sprintf('ALGO SALIO MAL!'), type="error")
    },finally={ 
    })
  })
  
  observe({
    if(is.null(res$KLUSTER)) return(NULL)
    updateSelectizeInput(session,'filtroCluster',choices=unique(res$KLUSTER$clust), server=TRUE)  
  })
  
  output$TABLACLUSTER <- renderDT({
    if(is.null(res$KLUSTER)) return()
    if(is.null(input$filtroCluster)){
      res$FiltroCluster <- data.frame(res$KLUSTER)
    }else{
      res$FiltroCluster <- data.frame(res$KLUSTER) %>% dplyr::filter(clust%in%input$filtroCluster)
    }
    res$FiltroCluster %>% DT::datatable(
      rownames=TRUE, selection='single', style='bootstrap4', extensions=c('Scroller'),
      options=list(scrollY=400, scrollX=800, scroller=TRUE, deferRender=TRUE, paging=TRUE, dom='tri')
    )
  })
  
  output$CLUSTER_AYUDA <- renderText({
    paste0('El anterior es un clustering jerárquico que permite visualizar las tipología de individuos existente donde agrupa en cada una de ellas a individuos ( municipios, ciudades o departamentos) con características similares en términos de las variables analizadas permitiendo comprender el contexto que contiene cada grupo basándose en las interpretaciones del plano realizadas en el analisis anteriormente.')
  })
  
  output$GRAFDEND <- renderPlot({
    if(is.null(res$HCPC)) return(NULL)
    fviz_dend(res$HCPC, cex=0.7, horiz=FALSE, type="rectangle",  palette="jco", rect=TRUE, rect_fill=TRUE, rect_border="jco", labels_track_height=0.8)
  })
  
  output$GRAFCLUS <- renderPlotly({
    if(is.null(res$HCPC)) return(NULL)
    fviz_cluster(res$HCPC, repel=FALSE, show.clust.cent=TRUE, palette="jco", main="CLUSTERS")  
  })
  
  output$DESCARGAR_TIPOLOGIA <- downloadHandler(
    filename = paste0("TABLA_TIPOLOGIA_",Sys.Date(),".xlsx"),
    content = function(file){
      xlsx::write.xlsx2(res$FiltroCluster, file)
    })
  
  observe({
    if(is.null(res$KLUSTER)) return()
    isolate({
      lista <- res$KLUSTER %>% dplyr::select(-clust)
      updateSelectizeInput(session, 'lista_cluster', choices=c(colnames(lista)))
    })
  })
  
  observeEvent(input$lista_cluster,{
    if(nchar(input$lista_cluster)==0) return()
    if(is.null(res$KLUSTER)) return()
    KLUSTER <- res$KLUSTER
    base <- data.frame('CLUSTER'=KLUSTER[,'clust'],'x'=KLUSTER[,input$lista_cluster])
    resumen <- data.frame(base) %>% 
      group_by(CLUSTER) %>% 
      summarise(media=mean(x) %>% round(digits=2), desvia=sd(x) %>% round(digits=2), mediana=median(x) %>% round(digits=2), minimo=min(x) %>% round(digits=2), maximo=max(x) %>% round(digits=2) )
    res$KLUSTER_RESUMEN <- data.frame(resumen) 
  })
  
  output$RESUMEN_TIPOLOGIA <- renderDataTable({
    if(is.null(res$KLUSTER_RESUMEN)) return()
    res$KLUSTER_RESUMEN %>% 
      DT::datatable(rownames=FALSE, escape=FALSE, selection='none', extensions=c('Scroller'), style='bootstrap4', class='cell-border stripe', 
                    options=list(scrollY=200, scrollX=TRUE, scroller=TRUE, deferRender=TRUE, dom='tri'))
  })
  
  
  
  
  
}

shinyApp(ui, server)

####



















