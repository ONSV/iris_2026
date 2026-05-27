library(arrow)

# SUBSTITUIR O CAMINHO DE ACORDO COM O USUÁRIO QUE ESTIVER EXECUTANDO
# O ARQUIVO ORIGINAL É MUITO GRANDE PARA INSERIR EM UM REPOSITÓRIO 
path_acidentes <- "~/Downloads/renaest_dabertos_20250412/Acidentes_DadosAbertos_20250412.csv"
path_vitimas <- "~/Downloads/renaest_dabertos_20250412/Vitimas_DadosAbertos_20250412.csv"

renaest_acidentes <- read_csv2_arrow(path_acidentes)
renaest_vitimas <- read_csv2_arrow(path_vitimas)

acidentes_2024 <- renaest_acidentes |> 
  filter(ano_acidente == 2024)

vitimas_2024 <- renaest_vitimas |> 
  filter(ano_acidente == 2024)

write_parquet(acidentes_2024, "data-raw/renaest/renaest_acidentes_2024.parquet")
write_parquet(vitimas_2024, "data-raw/renaest/renaest_vitimas_2024.parquet")
