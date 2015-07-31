
saveAsGEXF = function(g, filepath="converted_graph.gexf")
  
{
  
  require(igraph)
  
  require(rgexf)
  
  # gexf nodes require two column data frame (id, label)
  
  # check if the input vertices has label already present
  
  # if not, just have the ids themselves as the label
  
  if(is.null(V(g)$label))
    
    V(g)$label <- as.character(V(g)$name)
  
  # similarily if edges does not have weight, add default 1 weight
  
  if(is.null(E(g)$weight))
    
    E(g)$weight <- rep.int(1, ecount(g))
  
  nodes <- data.frame(cbind(V(g), V(g)$label))
  
  edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))
  
  # combine all node attributes into a matrix (and take care of & for xml)
  
  vAttrNames <- setdiff(list.vertex.attributes(g), "label")
  
  nodesAtt <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&",get.vertex.attribute(g, attr))),
                         
                         stringsAsFactors = FALSE)
  
  # combine all edge attributes into a matrix (and take care of & for xml)
  
  eAttrNames <- setdiff(list.edge.attributes(g), "weight")
  
  edgesAtt <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&",get.edge.attribute(g, attr))),
                         
                         stringsAsFactors = FALSE)
  
  # generate the gexf object
  
  output <- write.gexf(nodes, edges,
                       
                       edgesWeight=E(g)$weight,
                       
                       edgesAtt = edgesAtt,
                       
                       nodesAtt = nodesAtt)
  
  print(output, filepath, replace=T)
  
}


detach('package:dplyr', unload=T)
library(igraph)
library(dplyr)


head(out)
dim(out)
head(recomendados)

############################

hoteles_nombres <- sqlQuery(con,
"
SELECT Clav_Hotel, Nombre_Hotel, Clav_Destino
FROM Hoteles WITH(NOLOCK)
")
a1 <- hoteles_nombres[1:2]
names(a1) <- c('cl1','n1')
a2 <- a1
names(a2) <- c('cl2','n2')
a3 <- hoteles_nombres
names(a3) <- c('cl1','n1','d1')

library(networkD3)

aux <- recomendados %>%
  filter(id1 != id2) %>%
  inner_join(a3) %>%
  filter(d1 == 16) %>% # 2 = cancun, 16 playa del carmen
  group_by(cl1) %>%
  filter(row_number() <= 5)

ids1 <- aux %>%
  group_by(cl1) %>%
  summarise %>%
  mutate(Source = row_number() - 1)
ids2 <- ids1 %>%
  rename(cl2=cl1, Target=Source)

links <-  aux %>%
  inner_join(ids1) %>%
  inner_join(ids2) %>%
  mutate(Value = score) %>%
  dplyr::select(Source, Target, Value)
#links$Value %>% hist

nodes <- links %>%
  group_by(Target) %>%
  summarise(Size=n()^2) %>%
  right_join(ids2) %>%
  left_join(a2) %>%
  mutate(Size = ifelse(is.na(Size), 0.5, Size) + 5,
         Group = 1) %>%
  dplyr::select(Name = n2, Group, Size)

forceNetwork(Nodesize = 'Size',
             fontSize = 20,
             Links = links,
             Nodes = nodes,
             Source = 'Source',
             Target = 'Target',
             Value = 'Value',
             NodeID = 'Name',
             Group = 'Group',
             charge = -15000,
             zoom = T)
