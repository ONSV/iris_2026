tar_load(indicadores)
indicadores <- indicadores %>% mutate(ano = 2024)

saveRDS(indicadores, file = "/Users/anabeatrizmarques/Documents/onsv/iris/iris_2026/export/tabela_indicadores_2024.rds")
