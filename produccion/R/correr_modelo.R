source('produccion/R/funciones.R')

system.time({
  r <- recomendar(
    hot = hoteles,
    hot_cat = hoteles_categorias,
    mat = categorias_hoteles_sparse_cantidad,
    mat_norm = categorias_hoteles_sparse_prob,
    alpha = alpha,
    needed_weight = needed_weight,
    price_range = price_range,
    num_recom = max_num_recom,
    min_num_recom = min_num_recom,
    outer_fence = outer_fence,
    recommend = recommend,
    verbose = verbose)
})


info <- r$info
recomendados <- r$recomendados

rm(r)
gc()