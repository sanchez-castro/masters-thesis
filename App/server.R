library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(Matrix)
#library(ggmap)
library(leaflet)

# setwd("C:/Users/bmxddt005062/Documents/recomendacion_de_hoteles/App")
setwd("/Users/Felipe/data-science/masters-thesis/App")
options(scipen = 10) # Solo los numeros grandes en notacion cientifica


# Cargar los datos --------------------------------------------------------

# load('datos/datos_completos.Rdata')
load('datos/datos_completos_mac.Rdata')

# Funciones auxiliares ----------------------------------------------------

# Ver el tamaño de los objetos en el workspace, en MB
lh <- function(qqq){
  # Pasarle ls(). Regresa los tamanios en mb
  a <- sapply(qqq, function(x) eval(parse(text = paste0('object.size(',x,')')))) %>%
    data.frame
  names(a) <- 'b'
  a$mb <- format(round(a$b/2^20, 1), )
  a
}

# Imagen de matriz bien rotada
mat_image <- function(mat, ...){
  mat %>%
    apply(2, rev) %>%
    t %>%
    image(...)
}

# Transforma de grados a radianes
deg2rad <- function(deg) return(deg*pi/180)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
geodesic_distance <- function(long1, lat1, long2, lat2, units = 'deg') {
  if(any(is.na(c(long1,lat1,long2,lat2)))){
    return(Inf)
  } else {
    if(units == 'deg'){
      long1 <- deg2rad(long1)
      lat1 <- deg2rad(lat1)
      long2 <- deg2rad(long2)
      lat2 <- deg2rad(lat2)
    }
    R <- 6371 # Earth mean radius [km]
    delta.long <- (long2 - long1)
    delta.lat <- (lat2 - lat1)
    a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
    #   c <- 2 * asin(min(1,sqrt(a)))
    c <- 2 * asin(sapply(a, function(x) min(1,sqrt(x))))
    d = R * c
    return(d) # Distance in km
  }
}

##### Función auxiliar para graficar circulos
generate_circle <- function(center = c(0,0), radius = 1, npoints = 100){
  r = radius
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

##### Calcula el radio para generar un circulo de ciertos km de radio
calculate_radius <- function(long, lat, km, precision = 10000, maxdeg = 2){
  dtheta <- seq(0,maxdeg,l=precision)
  y <- sapply(dtheta, function(i){
    geodesic_distance(long,lat,long,lat+i,units='deg')
  })
  dtheta[y > km][1]
}

##### Función para filtrar hoteles lejanos y caros
filtra_cand <- function(clav, cand, hot, price_range=0.3, outer_fence=50, min_num_recom=10){
  #   clav <- 12
  long1 <- hot %>% filter(ID_Hotel == clav) %>% .$longitude
  lat1 <- hot %>% filter(ID_Hotel == clav) %>% .$latitude
  p1 <- hot %>% filter(ID_Hotel == clav) %>% .$Precio_Dlls
  p1 <- ifelse(is.na(p1), Inf, p1) # Si no hay precio, ignoramos el criterio haciendo que cueste infinito
  aux1 <- hot %>%
    mutate(km = ifelse(is.na(latitude) | is.na(longitude), Inf,
                       geodesic_distance(long1, lat1, longitude, latitude, units = 'deg')),
           precio = ifelse(is.na(Precio_Dlls), Inf, Precio_Dlls)) %>%
    dplyr::select(ID_Hotel, precio, km)
  precio_cond <- aux1$precio < (1 + price_range)*p1
  distancia_cond <- aux1$km <= outer_fence
  if(sum(precio_cond & distancia_cond) >= 10){ # Hay suficientes hot cerca en el rango de precios
    aux2 <- aux1 %>%
      filter(precio_cond, distancia_cond) %>%
      .$ID_Hotel
  } else if(sum(distancia_cond) >= 10){   # No hay suficientes --> Relajamos el precio
    aux2 <- aux1 %>%
      filter(distancia_cond) %>%
      .$ID_Hotel
  } else {                           # Aún no hay suficientes --> Tomamos los más cercanos
    aux2 <- aux1 %>%
      arrange(km) %>%
      head(min_num_recom) %>%
      .$ID_Hotel
  }
  data.frame(id1=clav, id2=aux2)
}

##### Función para calcular similitudes
calcula_sim <- function(candidatos, mat, mat_norm, nbatch=10){
  # candidatos es una matriz de nx2, con #candidato 1, #candidato 
  # mat es la matriz categorias-hoteles
  # nbatch es el níºmero de cachos en los que se partirí¡ la matriz para realizar la operación
  #   candidatos <- cand
  #   mat <- categ_sparse_cant
  #   nbatch <- 10
  
  candidatos <- as.matrix(candidatos)
  m <- nrow(candidatos)
  if(m == 0){
    return(NULL)
  } else if(m == 1){
    flag <- T
    m <- 2
    candidatos <- rbind(candidatos, c(candidatos[,1], 1))
  } else {
    flag = F
  }
  if(nbatch >= nrow(candidatos)) nbatch <- 1
  
  aux <- floor(seq(1, m, l=nbatch+1))
  
  distancias <- lapply(1:nbatch, function(i){
    #     print(i)
    ini <- ifelse(aux[i] == 1, 1, aux[i] + 1)
    fin <- aux[i+1]
    
    # Resta matriz cantidades
    subst <- mat[,candidatos[ini:fin,1]] - mat[,candidatos[ini:fin,2]]
    auxm <- subst
    auxm@x <- sapply(subst@x, function(x) max(x,0))
    hinge <- colSums(auxm) %>% as.numeric
    auxm@x <- sapply(subst@x, function(x) abs(x))
    abs <- colSums(auxm) %>% as.numeric
    
    # Resta matriz normalizada
    subst <- mat_norm[,candidatos[ini:fin,1]] - mat_norm[,candidatos[ini:fin,2]]
    auxm@x <- sapply(subst@x, function(x) abs(x)/2)
    diverg <- colSums(auxm) %>% as.numeric
    data.frame(diverg=diverg, diff_features=abs, hinge=hinge)
  }) %>%
    rbind_all
  output <- cbind(candidatos[,1:2], distancias)
  names(output) <- c('id1','id2','diverg','diff_features','hinge')
  
  if(flag) output <- output[1,] 
  output
}

# Parametros globales fijos -----------------------------------------------

outer_fence <- 30 # Nunca consideramos nada más lejano que 100 km
a1 <- hoteles %>%
  dplyr::select(id1=ID_Hotel, n1=Nombre_Hotel, cl1=Clav_Hotel,
                p1=Precio_Dlls, u1=Utilidad_Dlls, range1=Date_Range,
                city1=Nombre_Ciudad,
                pais1=Nombre_Pais, est1=Nombre_Estado, zip1=Codigo_Postal, stars1=Estrellas,
                dest1=Clav_Destino, ubic1=Clav_Ubicacion, long1=longitude, lat1=latitude,
                adult1=Adult_Only, allinc1=Categoria_Alimentos)
a2 <- hoteles %>%
  dplyr::select(id2=ID_Hotel, n2=Nombre_Hotel, cl2=Clav_Hotel,
                p2=Precio_Dlls, u2=Utilidad_Dlls, range2=Date_Range,
                city2=Nombre_Ciudad,
                pais2=Nombre_Pais, est2=Nombre_Estado, zip2=Codigo_Postal, stars2=Estrellas,
                dest2=Clav_Destino, ubic2=Clav_Ubicacion, long2=longitude, lat2=latitude,
                adult2=Adult_Only, allinc2=Categoria_Alimentos)

sums <- colSums(categorias_hoteles_sparse_cantidad)
mat_norm <- sparseMatrix(i = summary(categorias_hoteles_sparse_cantidad)$i,
                         j = summary(categorias_hoteles_sparse_cantidad)$j,
                         x = categorias_hoteles_sparse_cantidad@x/sums[summary(categorias_hoteles_sparse_cantidad)$j])

# Server ------------------------------------------------------------------

shinyServer(function(input, output){
  
  ##### Parámetros
  
  search_hits <- reactive({
    if(grepl('[0-9]', input$hotel)){
      clave <- gsub('[^0-9]', '', input$hotel) %>% as.numeric
      aux <- hoteles %>%
        filter(Clav_Hotel == clave)
    } else {
      a <- filter(hoteles, grepl(input$hotel, Nombre_Hotel, ignore.case = TRUE))
      b <- filter(hoteles, grepl(input$hotel, Nombre_Ciudad, ignore.case = TRUE))
      c <- filter(hoteles, grepl(input$hotel, Nombre_Estado, ignore.case = TRUE))
      d <- filter(hoteles, grepl(input$hotel, Nombre_Pais, ignore.case = TRUE))
      aux <- rbind(a,b,c,d)
    }
    aux %>%
      mutate(Precio_Dlls = round(Precio_Dlls)) %>%
      dplyr::select(Clav_Hotel, Nombre_Hotel, Nombre_Ciudad, Nombre_Estado, Nombre_Pais,
                    Precio_Dlls, Estrellas, Clav_Destino, ID_Hotel, longitude, latitude)
  })
  clav <- reactive({
    search_hits()$ID_Hotel[1]
  })
  name <- reactive({search_hits()$Nombre_Hotel[1]})
  loc <- reactive({
    if(input$mapa_manual) loc <- input$loc
    else loc <- as.character(filter(hoteles, ID_Hotel == clav())$Nombre_Ciudad)
  })
  
  ##### Similitudes

  top <- reactive({
    price_range <- input$price_range/100 #0.1 # price < (1 + price_range)
    nserv <- filter(hoteles_categorias, ID_Hotel == clav())$Cantidad %>% sum
    cand <- data.frame(id1=clav(), id2=claves_hoteles$ID_Hotel)
    cand2 <- filtra_cand(clav(), cand, hoteles, price_range = price_range,
                         outer_fence = outer_fence, min_num_recom=10) # HARDCODEADO!!!
    salida <- calcula_sim(cand2, mat = categorias_hoteles_sparse_cantidad,
                          mat_norm = mat_norm, nbatch = 1) %>%
      mutate(diverg = round(diverg, 3))
    
    alpha <- input$alpha
    
    top <- cand %>% ### Diferente al otro codigo (alla es cand2) porque aqui queremos todos los hoteles para el mapa

      left_join(a1) %>%
      left_join(a2) %>%
      left_join(salida) %>%
      mutate(km = geodesic_distance(long1,lat1,long2,lat2,units = 'deg'),
             temp = nserv,
             hinge_norm = ifelse(hinge == 0, 0,
                                 ifelse(temp == 0, 1, hinge/nserv)),
             score = alpha*(1 - diverg) + (1 - alpha)*(1 - hinge_norm),
             price = (p2 < (1 + price_range)*p1)) %>%
      dplyr::select(id1,id2,n1,n2,diverg,diff_features,hinge,hinge_norm,score,p1,p2,km,stars1,stars2,
                    allinc1,allinc2,adult1,adult2,city1,city2,ubic1,ubic2,u1,u2,dest1,dest2,est1,est2,
                    long1,lat1,long2,lat2,pais1,pais2,cl1,cl2,range1,range2,price)
    top
  })

  ##### Recomendaciones
  
  recomendaciones <- reactive({
    needed_weight <- input$needed_weight
    cand <- data.frame(id1=clav(), id2=claves_hoteles$ID_Hotel)
    
    max_km <- top() %>%
      arrange(is.na(!diverg), km) %>% ### Diferente al otro codigo porque aqui queremos todos los hoteles para el mapa
      filter(cumsum(score) < needed_weight) %>%
      .$km %>%
      max
    
    selected <- top() %>%
      filter(km <= max_km) %>%
      arrange(desc(score)) %>%
      head(input$num_recom) %>%
      mutate(recommend = T) %>%
      dplyr::select(id1,id2,recommend)
    output <- top() %>%
      left_join(selected, by = c('id1','id2')) %>%
      mutate(recommend = !is.na(recommend),
             same = (id1 == id2)) %>%
      arrange(desc(same), desc(recommend), desc(score)) %>%
      dplyr::select(-same) %>%
      mutate(rank = row_number())
    output <- output[c(ncol(output)-1, ncol(output), 1:(ncol(output)-2))]
    simples <- output %>%
      filter(recommend) %>%
      arrange(desc(score)) %>%
      dplyr::select(Rank = rank,
                    Recomendacion=n2,
                    Score = score,
                    Km = km,
                    Precio_busq=p1,
                    Precio_recom=p2,
                    Categoria_busq=stars1,
                    Categoria_recom=stars2,
                    All_Inc_busq=allinc1,
                    All_Inc_recom=allinc2)
    list(output, simples, max_km)
  })
  
  completas <- reactive(recomendaciones()[[1]])
  simples <- reactive(recomendaciones()[[2]])
  max_kms <- reactive(recomendaciones()[[3]])
  
  ##### Gráficas
  
  graficas <- reactive({
    # Parametros
    refresh <- input$refresh
    recom_shape <- 13
    
    nserv <- filter(hoteles_categorias, ID_Hotel == clav())$Cantidad %>% sum
    nserv <- ifelse(nserv == 0, 1, nserv)
    
    # Cargar datos
    max_km <- max_kms()
    daf <- completas() %>%
      mutate(candidato = !is.na(diverg),
             compatibilidad = (1 - diverg),
             servicios = ifelse(candidato, 1 - hinge/nserv, 0.3),
             op = ifelse(candidato, 0.8, 0.4),
             fillop = ifelse(candidato, 0.7, 0.2),
             near = (km < max_km),
             near2 = (km < max(outer_fence, max_km)),
             temp1 = ifelse(adult2, ', adult only', ''),
             temp2 = ifelse(candidato, paste0(rank,') '), ''),
             label = paste0(temp2, n2,' ($',round(p2),', ', round(km,1), ' km', temp1,
                            ', incl: ', allinc2, ')'))
        
    # Datos auxiliares para dibujar la cerca
    x <- daf[1,'long1']
    y <- daf[1, 'lat1']
    
    pal <- colorNumeric(
      na.color = 'blue',
      palette = c('red','yellow','green'),
      domain = c(0,1)
    )
    map_plot <- daf %>%
      leaflet() %>%
      addTiles() %>%
      # Legends
      addLegend(position = 'topright', title = 'Servicios', opacity = 0,
                colors = rep('black',3), labels = c('Chico = faltan muchos servicios',
                                                    'Mediano = faltan algunos servicios',
                                                    'Grande = tiene casi todos los servicios')) %>%
      addLegend(position = 'topright', colors = c(pal(seq(0,1,l=5)),'blue'), labels = c(seq(0,1,l=5),'Filtrados'),
                title = 'Compatibilidad', opacity = 1) %>%
      # Cerca exterior
      addCircles(lng = x, lat = y, radius = max(outer_fence, max_km)*1000, stroke = T, fillOpacity = 0.05,
                 color='black', weight = 1, opacity = 1, group='Cerca exterior') %>%
      # Cerca interior
      addCircles(lng = x, lat = y, radius = max_km*1000, stroke = T, fillOpacity = 0.05,
                 color='black', weight = 2, opacity = 1, group='Cerca interior') %>%
      # Los demás
      addCircleMarkers(lng = ~long2, lat = ~lat2, radius = ~(30*servicios),
                       color = ~pal(compatibilidad), stroke = F, fill = TRUE, 
                       fillOpacity = ~fillop, popup = ~label,
                       data = filter(daf, !candidato), group='Filtrados') %>%
      # Candidatos no recomendados
      addCircleMarkers(lng = ~long2, lat = ~lat2, radius = ~(30*servicios),
                       color = ~pal(compatibilidad), stroke = F, fill = TRUE, 
                       fillOpacity = ~fillop, popup = ~label,
                       data = filter(daf, (candidato & !recommend) | id2 == clav()), group='Candidatos no recomendados') %>%
      # Recomendados
      addCircleMarkers(lng = ~long2, lat = ~lat2, radius = ~(30*servicios),
                       color = ~pal(compatibilidad), stroke = F, fill = TRUE, 
                       fillOpacity = ~fillop, popup = ~label,
                       data = filter(daf, recommend), group='Recomendados') %>%
      # Resaltar recomendados
      addCircleMarkers(lng = ~long2, lat = ~lat2, radius = ~(30*servicios), opacity=1,
                       color = 'black', fill=FALSE, stroke=T, weight=1,
                       data = filter(daf, recommend), group='Resaltar Recomendados') %>%
      # Hotel original
      addMarkers(lng = ~long2, lat = ~lat2, popup=~label, data = daf[1,]) %>%
      addLayersControl(
        overlayGroups = c('Cerca exterior','Cerca interior','Filtrados',
                          'Candidatos no recomendados','Recomendados','Resaltar Recomendados'),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = x, lat = y, zoom = 11)
    map_plot
  })
  
  acumulados <- reactive({
    needed_weight <- input$needed_weight
    
    d2 <- completas() %>%
      arrange(km) %>%
      filter(!is.na(score)) %>%
      head(100) %>%
      mutate(peso_acumulado = cumsum(score),
             hoteles_acumulados = row_number()) %>%
      dplyr::select(km, score, hoteles_acumulados, peso_acumulado) %>%
      gather(variable, valor, hoteles_acumulados:peso_acumulado) %>%
      mutate(needed_weight = needed_weight)
    
    gr1 <- ggplot(d2) +
      geom_line(aes(km, valor)) +
      geom_line(aes(km, needed_weight), color='red', linetype='dashed') +
      facet_wrap(~ variable) +
      labs(y='')
        
    gr1
  })
    
  ##### Output
  
  output$hotel <- renderText(input$hotel)
  output$name <- renderText(paste(name(), clav()))
  output$search_hits <- renderDataTable(search_hits())
  output$completas <- renderDataTable(options = list(pageLength = 20), completas())
  output$simples <- renderDataTable(options = list(pageLength = 20), simples())
  output$map_plot <- renderLeaflet(graficas())
  output$acumulados <- renderPlot(acumulados(), width = 1000, height = 625)
  
})















