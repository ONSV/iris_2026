make_tbl_ind_class <- function() {
    tibble(
        pilar = c(
            "Pilar I", "Pilar II", "Pilar III", "Pilar IV", "Pilar V",
            "Pilar VI", "-"
        ),
        descricao = c(
            "Gestão na Segurança do Trânsito", 
            "Vias Seguras", 
            "Segurança Veicular", 
            "Educação para o Trânsito", 
            "Vigilância, Promoção da Saúde e Atendimento às Vítimas no Trânsito", 
            "Normalização e Fiscalização",
            "Resultado final"
        ),
        indicadores = c(
            "I.1, I.2, I.3, I.4", 
            "II.1, II.2, II.3, II.4, II.5", 
            "III.1, III.2, III.3, III.4", 
            "IV.1, IV.2, IV.3, IV.4, IV.5, IV.6, IV.7", 
            "V.1, V.2, V.3, V.4", 
            "VI.1, VI.2, VI.3, VI.4, VI.5, VI.6, VI.7",
            "0.1, 0.2, 0.3"
        )
    )
}

make_desc_ind_tbl <- function() {
    tibble(
        indicador = c(
            "I.1", "I.2", "I.3", "I.4",
            "II.1", "II.2", "II.3", "II.4", "II.5",
            "III.1", "III.2", "III.3", "III.4",
            "IV.1", "IV.2", "IV.3", "IV.4", "IV.5", "IV.6", "IV.7",
            "V.1", "V.2", "V.3", "V.4",
            "VI.1", "VI.2", "VI.3", "VI.4", "VI.5", "VI.6", "VI.7",
            "0.1", "0.2", "0.3"
        ),
        descricao = c(
            # Pilar I
            "Percentual de municípios integrados ao SNT",
            "Percentual de campos não informados nas bases do RENAEST",
            "Produto interno bruto per capita",
            "Disponibilidade de informações no portal do Detran",
            # Pilar II
            "Percentual de extensão de rodovias em condições péssimas e ruins na categoria Estado Geral",
            "Percentual de extensão de rodovias em condições péssimas e ruins na categoria Geometria",
            "Percentual de extensão de rodovias em condições péssimas e ruins na categoria Pavimento",
            "Percentual de extensão de rodovias em condições péssimas e ruins na categoria Sinalização",
            "Percentual de extensão de rodovias federais com pistas duplas",
            # Pilar III
            "Percentual de frota de motocicletas",
            "Percentual da frota com idade igual ou superior a 10 anos",
            "Percentual mínimo de veículos com Airbag/ABS na frota",
            "Percentual mínimo de veículos com ISOFIX na frota",
            # Pilar IV
            "Relação entre condutores habilitados a conduzir motocicletas e frota de motocicletas",
            "Taxa de infrações por consumo de bebidas alcoólicas a cada 10 mil veículos",
            "Taxa de infrações por excesso de velocidade a cada 10 mil veículos",
            "Taxa de infrações por não utilizar cinto de segurança a cada 10 mil veículos",
            "Taxa de infrações cometidas por não utilizar capacete a cada 10 mil motocicletas",
            "Taxa de infrações por utilizar celular na direção a cada 10 mil veículos",
            "Taxa de infrações cometidas por não utilizar dispositivos de retenção a cada 10 mil veículos",
            # Pilar V
            "Taxa de profissionais que atuam na área da saúde per capita",
            "Taxa de leitos totais per capita",
            "Taxa de leitos SUS per capita",
            "Taxa de leitos não SUS per capita",
            # Pilar VI
            "Taxa de infrações por frota",
            "Taxa de câmeras de segurança em geral em relacao à frota",
            "Taxa de câmeras de segurança em rodovias em relação à frota",
            "Taxa de câmeras de segurança em vias urbanas em relação à frota",
            "Taxa de câmeras de segurança em relação à extensão de rodovias federais pavimentadas",
            "Taxa de infrações de velocidade por câmera de segurança",
            "Taxa de câmeras de segurança em capitais em relação à extensão de vias",
            # "Taxa de pontos de monitoramento de velocidade em geral em relação à frota",
            # "Taxa de pontos de monitoramento de velocidade em rodovias em relação à frota",
            # "Taxa de pontos de monitoramento de velocidade em vias urbanas em relação à frota",
            # "Taxa de pontos de monitoramento de velocidade em relação à extensão de rodovias federais pavimentadas",
            # "Taxa de pontos de monitoramento de velocidade em capitais em relação à extensão de vias",
            # Resultado
            "Índice de óbitos por veículos",
            "Índice de óbitos por habitantes",
            "Índice de óbitos por bilhão de quilômetros percorridos"
        ),
        unidade = c(
            # Pilar 1
            "%",
            "%",
            "R$ 1.000 / pop.",
            "Nota",
            # Pilar 2
            "%",
            "%",
            "%",
            "%",
            "%",
            # Pilar 3
            "%",
            "%",
            "%",
            "%",
            # Pilar 4
            "n / 100 motocicletas",
            "n / 10 mil veic.",
            "n / 10 mil veic.",
            "n / 10 mil veic.",
            "n / 10 mil veic.",
            "n / 10 mil veic.",
            "n / 10 mil veic.",
            # Pilar 5
            "n / 1000 hab.",
            "n / 1000 hab.",
            "n / 1000 hab.",
            "n / 1000 hab.",
            # Pilar 6
            "n / 10 mil veic.",
            "n / 10 mil veic.",
            "n / 10 mil veic.",
            "n / 10 mil veic.",
            "n / 100 km",
            "n / n",
            "n / 100 km",
            #"n / 10 mil veic.",
            #"n / 10 mil veic.",
            #"n / 10 mil veic.",
            #"n / 100 km",
            #"n / 100 km",
            # Resultados
            "n / 10 mil veic.",
            "n / 100 mil hab.",
            "n / bi. km"
        ),
        fonte = c(
            # Pilar 1
            "SNT (2024)",
            "RENAEST (2023)",
            "IBGE (2021)",
            "ONSV (2024)",
            # Pilar 2
            "CNT (2023)",
            "CNT (2023)",
            "CNT (2023)",
            "CNT (2023)",
            "DNIT (2023)",
            # Pilar 3
            "RENAVAM (2023)",
            "RENAVAM (2023)",
            "RENAVAM (2023)",
            "RENAVAM (2023)",
            # Pilar 4
            "RENACH (2023); RENAVAM (2023)",
            "RENAINF (2023); RENAVAM (2023)",
            "RENAINF (2023); RENAVAM (2023)",
            "RENAINF (2023); RENAVAM (2023)",
            "RENAINF (2023); RENAVAM (2023)",
            "RENAINF (2023); RENAVAM (2023)",
            "RENAINF (2023); RENAVAM (2023)",
            # Pilar 5
            "DATASUS (2023); IBGE (2023)",
            "DATASUS (2023); IBGE (2023)",
            "DATASUS (2023); IBGE (2023)",
            "DATASUS (2023); IBGE (2023)",
            # Pilar 6
            "RENAINF (2023); RENAVAM (2023)",
            "INMETRO (2023); RENAVAM (2023)",
            "INMETRO (2023); RENAVAM (2023)",
            "INMETRO (2023); RENAVAM (2023)",
            "INMETRO (2023); DNIT (2023)",
            "RENAINF (2023); INMETRO (2023)",
            "INMETRO (2023); OSM (2024)",
            # "INMETRO (2023); RENAVAM (2023)",
            # "INMETRO (2023); RENAVAM (2023)",
            # "INMETRO (2023); RENAVAM (2023)",
            # "INMETRO (2023); DNIT (2023)",
            # "INMETRO (2023); OSM (2024)",
            # Resultado
            "DATASUS (2022); RENAVAM (2022)",
            "DATASUS (2022); IBGE (2022)",
            "ONSV (2023)"
        )
    )
}

make_gt_desc <- function(tbl_desc, ind_input) {
    tbl_desc |> 
        filter(str_extract(indicador, "^[^.]+\\.") == ind_input) |> 
        gt() |> 
        tab_options(table.font.size = "10pt") |> 
        cols_label(
            indicador = "Ind.",
            descricao = "Descrição",
            unidade = "Unidade",
            fonte = "Fonte"
        ) |> 
        cols_width(
            indicador ~ px(40),
            descricao ~ px(290),
            unidade ~ px(110),
            fonte ~ px(135)
        )
}

make_fontes_tbl <- function() {
    tibble(
        orgao = c(
            "CNT", "DATASUS-SIM", "DATASUS-CNES", "IBGE", "IBGE", "INMETRO", 
            "ONSV", "ONSV", "OSM", "RENAEST", "RENACH", "RENAINF", "RENAVAM",
            "SNT"
        ),
        informacao = c(
            "Pesquisa CNT de Rodovias",
            "Declarações de óbito",
            "Qtde. de profissionais e leitos",
            "População projetada",
            "Produto interno bruto",
            "Câmeras de segurança",
            "Nota do desempenho do Detran",
            "Óbitos por bi. de km percorrido",
            "Extensão de vias",
            "Quantidade de campos não-informados",
            "Condutores habilitados",
            "Notificações de penalidade",
            "Frota de veículos por tipo e idade",
            "Qtde. de municípios integrados ao SNT"
        ),
        ano_utilizado = c(
            "2023", "2023", "2023", "2021 a 2023", "2021", "2023", "2024",
            "2020", "2024", "2023", "2023", "2023", "2010 a 2023", "2024"
        )
    )
}

make_gt_cnt <- function(cnt_df_2023) {
    cnt_df_2023 |> 
        select(-total, -ano) |> 
        gt() |> 
        cols_label(
            uf = "UF",
            otimo = "Ótimo",
            bom = "Bom",
            regular = "Regular",
            ruim = "Ruim", 
            pessimo = "Péssimo"
        ) |> 
        tab_spanner(label = "Extensão (km)", columns = otimo:pessimo) |> 
        fmt_number(decimals = 0, sep_mark = ".", dec_mark = ",") |> 
        tab_options(table.font.size = "10pt")
}

make_codinf_tbl <- function() {
    tibble(
        cod_inf = c(
            5169, 5185, 5193, 6262, 6270, 6289, 6297, 6300, 6319, 6327, 6335,
            6343, 6351, 6360, 6378, 6386, 6394, 7030, 7048, 7072, 7137, 7366,
            7455, 7463, 7471
        ),
        fator_risco = c(
            "Álcool", "Cinto", "Retenção", "Velocidade", "Velocidade",
            "Velocidade", "Velocidade", "Velocidade", "Velocidade",
            "Velocidade", "Velocidade", "Velocidade", "Velocidade",
            "Velocidade", "Velocidade", "Velocidade", "Velocidade", "Capacete",
            "Capacete", "Fator Humano", "Fator Humano", "Celular", "Velocidade",
            "Velocidade", "Velocidade"
        ),
        descricao = c(
            "Dirigir sob a influência de álcool e dirigir sob a influência de qualquer substância psicoativa que deter dependência",
            "Deixar o condutor de usar o cinto segurança e deixar o passageiro de usar o cinto segurança",
            "Transportar criança sem observância das normas de segurança estabelecidas p/ CTB",
            "Deixar de reduzir a velocidade quando se aproximar de passeata, aglomeração, desfile /etc.",
            "Deixar de reduzir a velocidade onde o trânsito esteja sendo controlado pelo agente",
            "Deixar de reduzir a velocidade do veículo ao aproximar-se da guia da calçada - Deixar de reduzir a velocidade do veículo ao aproximar-se do acostamento",
            "Deixar de reduzir velocidade do veículo ao aproximar-se interseção não sinalizada",
            "Deixar reduzir velocidade nas vias rurais cuja faixa domínio não esteja cercada",
            "Deixar de reduzir a velocidade nos trechos em curva de pequeno raio",
            "Deixar de reduzir velocidade ao se aproximar de local sinalizado com advertência de obras/trabalhadores",
            "Deixar de reduzir a velocidade sob chuva/neblina/cerração/ventos fortes",
            "Deixar de reduzir a velocidade quando houver má visibilidade",
            "Deixar de reduzir a velocidade quando o pavimento se apresentar escorregadio, defeituoso ou avariado.",
            "Deixar de reduzir a velocidade à aproximação de animais na pista",
            "Deixar de reduzir a velocidade de forma compatível com a segurança, em declive",
            "Deixar de reduzir a velocidade de forma compatível com segurança ao ultrapassar ciclista",
            "Deixar de reduzir a velocidade nas proximidades de escolas, hospitais, estação de embarque/desembarque passageiros e onde haja intensa movimentação de pedestres",
            "Conduzir motocicleta, motoneta e ciclomotor sem capacete de segurança e/ou sem vestuário aprovado pelo Contran",
            "Conduzir motocicleta, motoneta e ciclomotor transportando passageiro s/ capacete e/ou transportando passageiro fora do assento",
            "Conduzir motocicleta/motoneta/ciclomotor transportando criança menor de 7 anos, ou sem condição de cuidar própria segurança",
            "Conduzir ciclomotor transportando criança s/ condição de cuidar própria segurança",
            "Dirigir o veículo utilizando-se de fones nos ouvidos conectados a aparelhagem sonora ou utilizando-se de telefone celular",
            "Transitar em velocidade superior à máxima permitida em até 20%",
            "Transitar em velocidade superior à máxima permitida em mais de 20% até 50%",
            "Transitar em velocidade superior à máxima permitida em mais de 50%"
        )
    )
}

make_gt_pilar2 <- function(df_cnt, ind_label) {
    df_cnt |> 
        mutate(indicador = (pessimo + ruim) / total) |> 
        select(-total, -ano) |> 
        arrange(-indicador) |> 
        gt() |> 
        cols_label(
            uf = "UF",
            otimo = "Ótimo",
            bom = "Bom",
            regular = "Regular",
            ruim = "Ruim",
            pessimo = "Péssimo",
            indicador = ind_label
        ) |> 
        fmt_number(
            otimo:pessimo, 
            decimals = 0, 
            dec_mark = ",", 
            sep_mark = "."
        ) |> 
        fmt_percent(indicador, decimals = 2, dec_mark = ",", sep_mark = ".") |> 
        tab_spanner(columns = otimo:pessimo, label = "Extensão (km)") |> 
        tab_options(table.font.size = "10pt") |> 
        data_color(
            columns = indicador,
            palette = "Blues"
        )
}

make_pilar5_tbl <- function(df_joined, var) {
    var <- sym(var)
    df_joined |> 
        mutate(indicador = {{var}} / populacao * 1000) |> 
        select(nome_uf, {{var}}, populacao, indicador) |> 
        arrange(-indicador)
}

make_pilar5_gt <- function(tbl_pilar5, n_label, ind_label) {
    tbl_pilar5 |> 
        gt() |> 
        cols_label(
            nome_uf = "UF",
            populacao = "População",
            indicador = ind_label
        ) |> 
        cols_label_with(starts_with("n_"), \(x) n_label) |> 
        fmt_number(decimals = 0, sep_mark = ".", dec_mark = ",") |> 
        fmt_number(indicador, decimals = 2, sep_mark = ".", dec_mark = ",") |> 
        tab_options(table.font.size = "10pt") |> 
        data_color(
            columns = indicador,
            palette = "Blues"
        )
}

make_pilar6_cameras_tbl <- function(df_joined, var) {
    var <- sym(var)
    df_joined |> 
        mutate(valor = {{var}} / frota_total * 10000) |> 
        select(sigla_uf, {{var}}, frota_total, valor) |> 
        arrange(-valor)
}

make_pilar6_cameras_gt <- function(tbl_pilar6, n_label, ind_label) {
    tbl_pilar6 |> 
        gt() |> 
        cols_label(
            sigla_uf = "UF",
            frota_total = "Frota de veículos",
            valor = ind_label
        ) |> 
        cols_label_with(starts_with("cameras_"), \(x) n_label) |> 
        fmt_number(decimals = 0, sep_mark = ".", dec_mark = ",") |> 
        fmt_number(valor, decimals = 2, sep_mark = ".", dec_mark = ",") |> 
        tab_options(table.font.size = "10pt") |> 
        data_color(
            columns = valor,
            palette = "Blues"
        )
}