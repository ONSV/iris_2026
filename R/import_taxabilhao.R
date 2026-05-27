create_taxabilhao <- function() {
  tibble(
    nome_uf = c(
      "São Paulo", "Distrito Federal", "Rio de Janeiro", "Rio Grande do Sul",
      "Santa Catarina", "Amapá", "Amazonas", "Roraima", "Minas Gerais",
      "Rondônia", "Goiás", "Rio Grande do Norte", "Paraná", "Acre",
      "Mato Grosso", "Mato Grosso do Sul", "Espírito Santo", "Ceará", "Pará",
      "Tocantins", "Pernambuco", "Bahia", "Sergipe", "Maranhão", "Paraíba",
      "Alagoas", "Piauí"
    ),
    taxa_bilhao = c(
      19.83, 20.00, 28.83, 25.56, 26.29, 20.95, 29.25, 26.85, 28.76, 32.48, 
      33.80, 33.02, 33.41, 31.96, 35.73, 36.70, 38.91, 43.09, 44.31, 42.48, 
      50.57, 46.72, 47.48, 54.13, 54.66, 73.75, 71.12
    )
  )
}
