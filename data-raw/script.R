
df = vroom::vroom('data-raw/_old/base_final_classificada_2019.txt')

# APENAS EMPRESA, PJ
df = df |>
  dplyr::filter(TIP_DOC_DEST == "J") |>
  dplyr::select(NUM_DOC_DEST, CNAE_DEST, NUM_NF, NUM_ITEM, DSC_PRODUTO,
                NUM_QTD_COMPRA, VAL_UN_COMPRA, grupo_cod, SEGMENTO)

df |> dplyr::glimpse()
df |> colnames()
df |> dplyr::distinct(NUM_DOC_DEST) |> nrow()  # 10332 EMPRESAS
df |> dplyr::distinct(CNAE_DEST) |> nrow()  # 233 CNAES
df |> dplyr::filter(is.na(CNAE_DEST))

table(df$SEGMENTO)

 
df = df |> dplyr::mutate(
  CNAE_DEST = as.character(CNAE_DEST),
  VAL_UN_COMPRA = stringr::str_replace(VAL_UN_COMPRA, ",","."),
  VAL_UN_COMPRA = as.double(VAL_UN_COMPRA)
)

 
features = df |>
  dplyr::group_by(NUM_DOC_DEST) |>
  dplyr::mutate(
    N_COMPRAS = dplyr::n(),
    VAL_UNIT_MEDIO = sum(VAL_UN_COMPRA) / N_COMPRAS,
    VOLUME_COMPRA_MEDIO = sum(NUM_QTD_COMPRA) / N_COMPRAS,
    CESTA_PROD_DIFER = dplyr::n_distinct(DSC_PRODUTO),
    GRUPO_PROD_DIFER = dplyr::n_distinct(grupo_cod)
  ) |> dplyr::select(
    NUM_DOC_DEST, CNAE_DEST, N_COMPRAS, VAL_UNIT_MEDIO, 
    VOLUME_COMPRA_MEDIO, CESTA_PROD_DIFER, GRUPO_PROD_DIFER, SEGMENTO
  ) |> dplyr::distinct_all()

table(features$SEGMENTO)

rm(df) 


# Reclassificando a base por segmento:

features = features |>
  dplyr::mutate(
    target = dplyr::case_when(
      CNAE_DEST == "4711301" ~ "I",
      CNAE_DEST == "4711302" ~ "I",
      CNAE_DEST == "4723700" ~ "I",
      CNAE_DEST == "4712100" ~ "II",
      CNAE_DEST == "1091100" ~ "III",
      CNAE_DEST == "1091102" ~ "III",
      CNAE_DEST == "4721101" ~ "III",
      CNAE_DEST == "4721102" ~ "III",
      CNAE_DEST == "5611201" ~ "III",
      CNAE_DEST == "5611202" ~ "III",
      CNAE_DEST == "5611203" ~ "III",
      CNAE_DEST == "5620101" ~ "III",
      CNAE_DEST == "5620103" ~ "III",
      CNAE_DEST == "5620104" ~ "III",
      TRUE ~ "unclassified"
    )
  )

table(features$target)

features = features |> dplyr::filter(target != 'unclassified')

features |> dplyr::rename(N_TRANS = N_COMPRAS) |> 
  readr::write_csv('data/features.csv')


# 
# # Base classificada utilizando a base da RFB (Banco SQL)
# rfb = readr::read_csv("data-raw/RFB_labeled_dataset.csv", 
#                       col_types = readr::cols(SEGMENTO = readr::col_character(), 
#                                               cnae_fiscal = readr::col_character()))
# 
# rfb |> dplyr::glimpse()
# 
# 
# dplyr::glimpse(features)
# table(features$seg)
# features |> dplyr::distinct(NUM_DOC_DEST) # 7,900
# 
# df = features |>
#   dplyr::select(
#     -N_COMPRAS, -VAL_UNIT_MEDIO, -VOLUME_COMPRA_MEDIO,
#     -CESTA_PROD_DIFER_1, -CESTA_PROD_DIFER_2, -GRUPO_PROD_DIFER_1,
#     -SEGMENTO
#   )
# 
# df |> readr::write_rds('dataset-nf.rds')
# 
# features1 = features |> 
#   dplyr::select(NUM_DOC_DEST, N_COMPRAS, VAL_UNIT_MEDIO,VOLUME_COMPRA_MEDIO,
#                 CESTA_PROD_DIFER_1, CESTA_PROD_DIFER_2, GRUPO_PROD_DIFER_1,seg) |> 
#   dplyr::distinct_all() |> 
#   dplyr::filter(seg!='unclassified')
# 
# unclassified = features |> 
#   dplyr::select(NUM_DOC_DEST, N_COMPRAS, VAL_UNIT_MEDIO,VOLUME_COMPRA_MEDIO,
#                 CESTA_PROD_DIFER_1, CESTA_PROD_DIFER_2, GRUPO_PROD_DIFER_1,seg) |> 
#   dplyr::distinct_all() |> 
#   dplyr::filter(seg=='unclassified')
# 
# unclassified = unclassified[sample(nrow(unclassified), size=177), ]
# 
# features1 = dplyr::bind_rows(features1, unclassified)

# 
# dplyr::glimpse(features1)
# table(features$seg)
# 
# 
# dataset = dplyr::bind_rows(seg1, seg2, seg3) |> 
#   dplyr::rename(N_TRANS = N_COMPRAS,
#                 GRUPO_PROD_DIFER = GRUPO_PROD_DIFER_1,
#                 CESTA_PROD_DIFER = CESTA_PROD_DIFER_1) |> 
#   dplyr::select(-CESTA_PROD_DIFER_2)
# 
# dplyr::glimpse(dataset)
# table(dataset$seg)

# seg1 = features |> dplyr::filter(seg=="I")
# seg2 = features |> dplyr::filter(seg=="II")
# seg3 = features |> dplyr::filter(seg=="III")
# 
# 
# size = nrow(seg1)
# seg2 = seg2[sample(nrow(seg2), size=size), ]
# seg3 = seg3[sample(nrow(seg3), size=size), ]


# dataset |> readr::write_csv('data/dataset.csv')

# START -------------------------------------------------------------------

dataset = readr::read_csv('data/dataset.csv')

dataset |> dplyr::select(-GRUPO_PROD_DIFER) |> 
  readr::write_csv('data/dataset.csv')
