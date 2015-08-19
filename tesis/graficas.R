library(ggplot2)

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
