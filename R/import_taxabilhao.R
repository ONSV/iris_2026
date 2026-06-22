create_taxabilhao <- function() {
  tibble(
    nome_uf = c(
        "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", 
        "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão", 
        "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará", 
        "Paraíba", "Paraná", "Pernambuco", "Piauí", "Rio de Janeiro", 
        "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", 
        "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"
    ),
    taxa_bilhao = c(
        25.95, 62.39, 25.82, 26.96, 48.33, 37.13, 14.64, 40.23,
        28.90, 48.43, 30.18, 28.13, 23.37, 39.97, 46.17, 29.61, 47.92,
        67.03, 20.11, 28.93, 23.35, 38.14, 29.04, 22.13, 15.57, 50.99, 
        43.91
    )
  )
}
