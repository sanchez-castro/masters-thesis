library(ggplot2)
library(grid)
library(ggthemes)
library(dplyr)

# Auxiliares --------------------------------------------------------------

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

## Similitud
aux <- data.frame(x = c('BEACH','BUSINESS','ENTERTAINMENT','CONVENIENCE'),
                  y = c(0.1,6,10,8),
                  Hotel = rep('Original', 4))
ggplot(aux) +
  geom_bar(aes(x,y,fill=Hotel), stat='identity', position='dodge', width=0.5) +
  scale_fill_manual(values = 'black') +
  labs(title='Características del hotel original', x='', y='Número de servicios por categoría') +
  theme(axis.text.x=element_text(angle=30, hjust=1))

## Similitud (servicios)
x <- data.frame(x = c('BEACH','BUSINESS','ENTERTAINMENT','CONVENIENCE'),
                y = c(0.1,6,10,8),
                Hotel = rep('Original', 4))
y <- data.frame(x = c('BEACH','BUSINESS','ENTERTAINMENT','CONVENIENCE'),
                y = c(4,5,9,9),
                Hotel = rep('Alternativa 1', 4))
z <- data.frame(x = c('BEACH','BUSINESS','ENTERTAINMENT','CONVENIENCE'),
                y = c(0.1,3,4,4),
                Hotel = rep('Alternativa 2', 4))
aux <- rbind(
  cbind(x,div='Perfil similar', hinge='Servicios suficientes'),
  cbind(y,div='Perfil similar', hinge='Servicios suficientes'),
  cbind(x,div='Perfil similar', hinge='Servicios insuficientes'),
  cbind(z,div='Perfil similar', hinge='Servicios insuficientes'))

ggplot(aux) +
  geom_bar(aes(x,y,fill=Hotel), stat='identity', position='dodge', width=0.5) +
  scale_fill_manual(values = c('black', rgb(0.1,0.8,0.3), rgb(0.7,0.1,0.7))) +
  facet_wrap( ~ hinge) +
  labs(title='Efecto de la cantidad de servicios', x='', y='Número de servicios por categoría') +
  theme(axis.text.x=element_text(angle=30, hjust=1))



## Similitud (perfil)

x <- data.frame(x = c('BEACH','BUSINESS','ENTERTAINMENT','CONVENIENCE'),
                y = c(0.1,6,10,8),
                Hotel = rep('Original', 4))
y <- data.frame(x = c('BEACH','BUSINESS','ENTERTAINMENT','CONVENIENCE'),
                y = c(1,3,6,5),
                Hotel = rep('Alternativa 1', 4))
v <- data.frame(x = c('BEACH','BUSINESS','ENTERTAINMENT','CONVENIENCE'),
                y = c(10,10,10,10),
                Hotel = rep('Alternativa 3', 4))
aux <- rbind(
  cbind(x,div='Perfil similar', hinge='Servicios suficientes'),
  cbind(y,div='Perfil similar', hinge='Servicios suficientes'),
  cbind(x,div='Perfil diferente', hinge='Servicios suficientes'),
  cbind(v,div='Perfil diferente', hinge='Servicios suficientes'))

ggplot(aux) +
  geom_bar(aes(x,y,fill=Hotel), stat='identity', position='dodge', width=0.5) +
  scale_fill_manual(values = c('black', rgb(0.1,0.8,0.3), rgb(0.1,0.3,0.8))) +
  facet_wrap(~ div) +
  labs(title='Efecto de los perfiles', x='', y='Número de servicios por categoría') +
  theme(axis.text.x=element_text(angle=30, hjust=1))




## Similitud (servicios + perfil)


x <- data.frame(x = c('BEACH','BUSINESS','ENTERTAINMENT','CONVENIENCE'),
                y = c(0.1,6,10,8),
                Hotel = rep('Original', 4))
y <- data.frame(x = c('BEACH','BUSINESS','ENTERTAINMENT','CONVENIENCE'),
                y = c(1,5,9,9),
                Hotel = rep('Alternativa 1', 4))
z <- data.frame(x = c('BEACH','BUSINESS','ENTERTAINMENT','CONVENIENCE'),
                y = c(0.1,3,4,4),
                Hotel = rep('Alternativa 2', 4))
v <- data.frame(x = c('BEACH','BUSINESS','ENTERTAINMENT','CONVENIENCE'),
                y = c(10,10,10,10),
                Hotel = rep('Alternativa 3', 4))
w <- data.frame(x = c('BEACH','BUSINESS','ENTERTAINMENT','CONVENIENCE'),
                y = c(10,9,2,4),
                Hotel = rep('Alternativa 4', 4))

aux <- rbind(
  cbind(x,div='Perfil similar', hinge='Servicios suficientes'),
  cbind(y,div='Perfil similar', hinge='Servicios suficientes'),
  cbind(x,div='Perfil similar', hinge='Servicios insuficientes'),
  cbind(z,div='Perfil similar', hinge='Servicios insuficientes'),
  cbind(x,div='Perfil diferente', hinge='Servicios suficientes'),
  cbind(v,div='Perfil diferente', hinge='Servicios suficientes'),
  cbind(x,div='Perfil diferente', hinge='Servicios insuficientes'),
  cbind(w,div='Perfil diferente', hinge='Servicios insuficientes'))

ggplot(aux) +
  geom_bar(aes(x,y,fill=Hotel), stat='identity', position='dodge', width=0.5) +
  scale_fill_manual(values = c('black', rgb(0.1,0.8,0.3),
                               rgb(0.7,0.1,0.7), rgb(0.1,0.3,0.8), rgb(1,0.2,0.1))) +
  facet_grid(div ~ hinge) +
  labs(title='Características combinadas: perfil + servicios', x='', y='Número de servicios por categoría') +
  theme(axis.text.x=element_text(angle=30, hjust=1))

## Elección de alpha
set.seed(1234)
n <- 50
x <- rbeta(n, shape1 = 8, shape2 = 2)
qplot(x)
y <- 3*x - 2 + rnorm(n, 0, 0.1)
d <- data.frame(x,y) %>%
  filter(x > 0, x < 1, y > 0, y < 1)
mx <- mean(x)
my <- mean(y)
arr <- data.frame(x = rep(mx, 3),
                  y = rep(my, 3),
                  xend = c(0.75, 1, 1),
                  yend = c(0.9, 1, my + (1-mx)),
                  col = c('blue','red','black'))
plot_alpha <- ggplot(d) +
  geom_point(aes(x,y)) +
  geom_segment(data=arr,
               mapping=aes(x=x, xend=xend, y=y, yend=yend),
               arrow = arrow(length = unit(0.5,'cm'))) +
  geom_point(aes(x=mx, y=my), color = 'red', size = 3) +
  geom_text(aes(x=mx, y=my, label='(mean(x), mean(y))'), hjust=1.1) +
  xlim(0,1) +
  ylim(0,1) +
  coord_equal() +
  labs(title = 'Dirección de avance de la similitud\npara distintos valores de alpha',
       x = 'Similitud de servicios',
       y = 'Similitud de perfil')
plot_alpha

ggsave(filename = 'tesis/imagenes/alpha.pdf',
       plot = plot_alpha,
       width = 8, height = 8)


# Distancia: criterio dinámico --------------------------------------------

set.seed(1234)
x1 <- data.frame(x=c(0,rnorm(29),1.5,-1),
                 y=c(0,rnorm(29),1.2,-1.3),
                 s=c(5,2*runif(29),4,4),
                 col=c('original', rep('opciones malas',29),
                       'opciones buenas', 'opciones buenas'),
                 id='Pocos hoteles similares cercanos')

x2 <- data.frame(x=c(0,rnorm(30)),
                 y=c(0,rnorm(30)),
                 s=c(5,2*runif(10), 3 + 2*runif(20)),
                 col=c('original', rep('opciones malas',10),
                       rep('opciones buenas', 20)),
                 id='Muchos hoteles similares cercanos')
x3 <- rbind(x1,x2)
c1 <- circleFun(center = c(0,0), diameter = 2, npoints = 100)
c2 <- circleFun(center = c(0,0), diameter = 4, npoints = 100)
c3 <- rbind(cbind(c1, id='Muchos hoteles similares cercanos'),
            cbind(c2, id='Pocos hoteles similares cercanos'))

plot_distdin <- ggplot(x3) +
  geom_point(aes(x,y,size=s,color=col)) +
  geom_point(aes(x,y,size=s,alpha=(s>2)), shape=1, color='black') +
  geom_path(data=c3, aes(x,y), linetype='dashed') +
  scale_size_continuous(guide=FALSE) +
  scale_alpha_discrete(guide=FALSE) +
  guides(color=guide_legend(title=NULL)) +
  labs(x='Longitud', y='Latitud') +
  coord_equal() +
  facet_wrap(~ id)

ggsave(filename = 'tesis/imagenes/distdin.pdf',
       plot = plot_distdin,
       width = 8, height = 4)


# Preparar información para Gephi -----------------------------------------

library(dplyr)
library(igraph)

load('tesis/datos/datos_gephi.Rdata')
hoteles <- read.csv('tesis/datos/Hoteles.csv', header = T, quote = '"',
                    stringsAsFactors = F) %>%
  dplyr::select(-X) %>%
  arrange(Clav_Hotel)

dim(recomendados)
head(recomendados)

h1 <- hoteles %>%
  rename(cl1=Clav_Hotel, n1=Nombre_Hotel, d1=Clav_Destino, cld1=Nombre_Destino)
h2 <- hoteles %>%
  rename(cl2=Clav_Hotel, n2=Nombre_Hotel, d2=Clav_Destino, cld2=Nombre_Destino)

red_df <- recomendados %>%
  filter(rank <= 5) %>%
  left_join(h1) %>%
  left_join(h2) %>%
  dplyr::select(from=cl1, to=cl2, km, score)
dim(red_df)
head(red_df)

red <- graph.data.frame(red_df, directed = T)
vert <- data.frame(Clav_Hotel = as.numeric(get.vertex.attribute(red, name = 'name')))
vert <- vert %>%
  left_join(hoteles, by=c('Clav_Hotel'))
red <- set.vertex.attribute(red, name = 'Nombre_Hotel', value = vert$Nombre_Hotel)
red <- set.vertex.attribute(red, name = 'Clav_Destino', value = vert$Clav_Destino)
red <- set.vertex.attribute(red, name = 'Nombre_Destino', value = vert$Nombre_Destino)
get.vertex.attribute(red, name = 'name')
get.vertex.attribute(red, name = 'Nombre_Hotel')
get.vertex.attribute(red, name = 'Clav_Destino')
get.vertex.attribute(red, name = 'Nombre_Destino')

write.graph(red, file="tesis/datos/red2.graphml", format="graphml")










