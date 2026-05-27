targets_import <- list(
  # import_datasus_sim.R
  tar_target(datasus_sim, import_datasus_sim(roadtrafficdeaths::rtdeaths)),
  # import_datasus_cnes.R
  tar_target(
    file_patterns, 
    c("leitos_nsus", "leitos_sus", "totais", "profissionais")
  ),
  tar_target(file_paths, map(file_patterns, extract_cnes_path)),
  tar_target(list_raw_df, map(file_paths, read_cnes_files)),
  tar_target(list_clean_df, map(list_raw_df, arrange_cnes_df)),
  tar_target(datasus_cnes, join_cnes_df(list_clean_df)),
  # import_cnes.R
  tar_target(
    cnt_path,
    list.files("data-raw/cnt", full.names = TRUE, pattern = "pesquisa")
  ),
  tar_target(raw_pdf_list, map(cnt_path, pdf_text)),
  tar_target(table_list, create_table_list()),
  tar_target(
    page_number_2022,
    map(table_list$ano_2022, grep, x = raw_pdf_list[[1]])
  ),
  tar_target(
    page_number_2023,
    map(table_list$ano_2023, grep, x = raw_pdf_list[[2]])
  ),
  tar_target(
    list_raw_tables_2022,
    map(
      page_number_2022,
      get_raw_table,
      raw_text = raw_pdf_list[[1]]
    )
  ),
  tar_target(
    list_raw_tables_2023,
    map(
      page_number_2023,
      get_raw_table,
      raw_text = raw_pdf_list[[2]]
    )
  ),
  tar_target(list_df_2022, map(list_raw_tables_2022, cnt_transform_to_df)),
  tar_target(list_df_2023, map(list_raw_tables_2023, cnt_transform_to_df)),
  tar_target(
    cnt,
    list(
      tabelas_2022 = map(list_df_2022, arrange_cnt_df, ano = 2022),
      tabelas_2023 = map(list_df_2023, arrange_cnt_df, ano = 2023)
    )
  ),
  # import_cameras.R
  tar_target(df_cameras, read_excel("data-raw/cameras/cameras.xlsx")),
  tar_target(cameras, arrange_cameras_df(df_cameras)),
  tar_target(pontos_fiscalizacao, calc_pontos_fiscalizacao(df_cameras)),
  tar_target(pontos_fisc_uf, calc_pontos_fisc_uf(pontos_fiscalizacao)),
  tar_target(pontos_fisc_mun, calc_pontos_fisc_mun(pontos_fiscalizacao, osm)),
  # import_detrans.R
  tar_target(detrans, make_df_detrans()),
  # import_dnit.R
  tar_target(
    dnit_path_list, 
    list.files("data-raw/dnit", pattern = "SNV", full.names = TRUE)
  ),
  tar_target(list_raw_dnit, map(dnit_path_list, read_dnit_df)),
  tar_target(
    dnit, 
    map2(list_raw_dnit, seq(2021, 2024, 1), arrange_dnit_df) |> 
      reduce(bind_rows)
  ),
  # import_ibge_pib.R
  tar_target(
    raw_ibge_pib_df,
    read_excel("data-raw/ibge_pib/ibge_pib.xlsx", skip = 3, n_max = 27)
  ),
  tar_target(ibge_pib, clean_ibge_pib_df(raw_ibge_pib_df)),
  # import_ibge_pop.R
  tar_target(
    raw_ibge_pop,
    read_csv2(
      "data-raw/ibge_pop/ibge_pop.csv",
      locale = locale(encoding = "latin1"), 
      skip = 3, 
      n_max = 27
    )
  ),
  tar_target(ibge_pop, arrange_ibge_pop(raw_ibge_pop)),
  # import_renach.R
  tar_target(renach, arrange_renach(driversbr::drivers)),
  # import_renaest.R
  tar_target(
    renaest_path,
    list.files("data-raw/renaest", pattern = "parquet", full.names = TRUE)
  ),
  tar_target(renaest_list, map(renaest_path, read_parquet)),
  tar_target(renaest, join_renaest(renaest_list, arrange_renaest)),
  # import_renainf.R
  tar_target(renainf_url, "https://www.gov.br/transportes/pt-br/assuntos/transito/conteudo-Senatran/estatisticas-quantidade-de-infracoes-denatran"),
  tar_target(renainf_raw, download_renainf_2023(renainf_url)),
  tar_target(renainf, arrange_renainf(renainf_raw)),
  # import_renavam.R
  tar_target(renavam_2011_2023, read_renavam_2011_2023(fleetbr::fleetbr)),
  tar_target(
    path_old_renavam,
    list.files("data-raw/renavam", full.names = TRUE, pattern = "tipo")
  ),
  tar_target(
    list_old_renavam,
    map(
      path_old_renavam,
      read_excel,
      sheet = 2,
      skip = 2,
      n_max = 33
    )
  ),
  tar_target(
    old_renavam,
    map2(list_old_renavam, c(2009, 2010), arrange_old_renavam) |> 
      reduce(bind_rows)
  ),
  tar_target(renavam, bind_rows(renavam_2011_2023, old_renavam)),
  tar_target(
    df_renavam_idade, 
    read_excel("data-raw/renavam/renavam_idade_2023.xlsx")
  ),
  tar_target(renavam_idade, arrange_renavam_idade(df_renavam_idade)),
  # import_snt.R
  tar_target(df_snt, read_snt("https://www.gov.br/transportes/pt-br/assuntos/transito/conteudo-Senatran/municipalizacao-senatran")),
  tar_target(
    raw_municipios, 
    read_csv("data-raw/snt/ibge_municipios.csv", skip = 6)
  ),
  tar_target(municipios, arrange_municipios(raw_municipios)),
  tar_target(snt, left_join(df_snt, municipios)),
  # import_taxabilhao.R
  tar_target(taxabilhao, create_taxabilhao()),
  # import_uf.R
  tar_target(uf, create_uf(raw_municipios)),
  tar_target(sf_uf, read_state()),
  # import_osm.R
  tar_target(
      osm, 
      map(
          list.files("data-raw/osm", full.names = TRUE, pattern = "geojson"), 
          calc_osm_dist
      ) |> 
          reduce(bind_rows)
  )
)