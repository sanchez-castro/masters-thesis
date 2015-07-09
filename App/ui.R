library(shiny)
library(leaflet)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Recomendación de hoteles"),
  
  sidebarPanel(
    textInput("hotel", label = "Nombre o clave del hotel", 'golden'),
    h3(p('Visualización')),
    sliderInput('num_recom',
                label = 'Número de hoteles a recomendar',
                min = 1, max = 20, value = 10),
    h3(p('Parámetros del modelo')),
    sliderInput('price_range',
                label = 'Máxima desviación del precio (hacia arriba, %)',
                min = 1, max = 300, value = 30),
    sliderInput('alpha',
                label = 'Cantidad de servicios - Perfil del hotel',
                min = 0, max = 1, value = 0.338, step = 0.001),
    sliderInput('needed_weight',
                label = 'Tamaño de la vecindad de hoteles a considerar',
                min = 1, max = 100, value = 30),
    actionButton('refresh', 'Recargar mapa'),
    width = 2
  ),
  
  mainPanel(
    h3(textOutput("name", container = span)),
    tabsetPanel(
      tabPanel('Resultados de búsqueda', dataTableOutput("search_hits")),
      tabPanel('Mapa', leafletOutput('map_plot', width = 1000, height = 620)),
      tabPanel('Recomendaciones', dataTableOutput("simples")),
      tabPanel('Acumulación de hoteles', plotOutput('acumulados', width='100%', height='100%')),
      tabPanel('Información completa', dataTableOutput("completas"))
    )
  )
))