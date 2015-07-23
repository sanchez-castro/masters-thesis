
# Correr modelo.R y después...

# Versión provisional para subir ya ---------------------------------------

max_num_recom <- 20
out <- r$recomendados %>%
  left_join(a1[c('id1','cl1','n1')], by='id1') %>%
  left_join(a2[c('id2','cl2','n2')], by='id2') %>%
  dplyr::select(cl1,cl2,n1,n2,p1,p2,km,rank,score,diverg,hinge,diff_features) %>%
  mutate(within_price = (p2 < p1 * (1 + 0.3))) %>%
  group_by(cl1) %>%
  mutate(nrec = sum(within_price)) %>%
  filter((nrec >= max_num_recom & within_price) | (nrec < max_num_recom)) %>%
  top_n(n = max_num_recom, rank) %>%
  arrange(cl1, rank)

out_info <- out %>%
  group_by(cl1) %>%
  tally
qplot(n, data=out_info)

out[1001:2000,] %>%
  View

out %>%
  filter(cl1 == 9) %>%
  head(10)

out_final <- out %>%
  dplyr::select(Clav_Hotel_Buscado=cl1, Clav_Hotel_Recomendado=cl2) %>%
  mutate(Rank = row_number())

out_final %>%
  tally %>%
  .$n %>%
  mean
dim(out_final)
out_final$Clav_Hotel_Buscado %>% unique %>% length
# write.csv(out_final,
#           file = 'salida/recomendaciones_implementacion_rapida.csv',
#           row.names = FALSE,
#           quote = FALSE,
#           fileEncoding = 'utf-8')
