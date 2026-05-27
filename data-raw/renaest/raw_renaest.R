library(arrow)

# SUBSTITUIR O CAMINHO DE ACORDO COM O USUÁRIO QUE ESTIVER EXECUTANDO
# O ARQUIVO ORIGINAL É MUITO GRANDE PARA INSERIR EM UM REPOSITÓRIO 
path_acidentes <- "~/Downloads/RENAEST_DABERTOS_20240712/Acidentes_DadosAbertos_20240712.csv"
path_vitimas <- "~/Downloads/RENAEST_DABERTOS_20240712/Vitimas_DadosAbertos_20240712.csv"

renaest_acidentes <- read_csv2_arrow(path_acidentes)
renaest_vitimas <- read_csv2_arrow(path_vitimas)

acidentes_2023 <- renaest_acidentes |> 
  filter(ano_acidente == 2023)

vitimas_2023 <- renaest_vitimas |> 
  filter(ano_acidente == 2023)

write_parquet(acidentes_2023, "data-raw/renaest/renaest_acidentes_2023.parquet")
write_parquet(vitimas_2023, "data-raw/renaest/renaest_vitimas_2023.parquet")
