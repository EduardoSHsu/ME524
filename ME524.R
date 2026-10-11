library(tidyverse)

brasileirao <- read_csv("brasileirao_2026.csv")

# Jogos já realizados

jogos_realizados <- brasileirao %>%
  filter(
    !is.na(gols_mandante),
    !is.na(gols_visitante)
  )

# Estimativa dos parâmetros

parametros_mandante <- jogos_realizados %>%
  group_by(time_mandante) %>%
  summarise(
    theta_m = mean(gols_mandante),
    phi_m   = mean(gols_visitante),
    .groups = "drop"
  ) %>%
  rename(time = time_mandante)


parametros_visitante <- jogos_realizados %>%
  group_by(time_visitante) %>%
  summarise(
    theta_v = mean(gols_visitante),
    phi_v   = mean(gols_mandante),
    .groups = "drop"
  ) %>%
  rename(time = time_visitante)


# parâmetros de todos os times
parametros <- parametros_mandante %>%
  full_join(parametros_visitante, by = "time")

# TABELA DE CLASSIFICAÇÃO ATUAL

tabela_atual <- bind_rows(
  jogos_realizados %>%
    transmute(
      time = time_mandante,
      pontos = case_when(
        gols_mandante > gols_visitante ~ 3,
        gols_mandante == gols_visitante ~ 1,
        TRUE ~ 0
      ),
      vitorias = as.integer(gols_mandante > gols_visitante),
      empates = as.integer(gols_mandante == gols_visitante),
      derrotas = as.integer(gols_mandante < gols_visitante),
      saldo_gols = gols_mandante - gols_visitante,
      gols_pro = gols_mandante,
      gols_contra = gols_visitante
    ),
  jogos_realizados %>%
    transmute(
      time = time_visitante,
      pontos = case_when(
        gols_visitante > gols_mandante ~ 3,
        gols_visitante == gols_mandante ~ 1,
        TRUE ~ 0
      ),
      vitorias = as.integer(gols_visitante > gols_mandante),
      empates = as.integer(gols_mandante == gols_visitante),
      derrotas = as.integer(gols_visitante < gols_mandante),
      saldo_gols = gols_visitante - gols_mandante,
      gols_pro = gols_visitante,
      gols_contra = gols_mandante
    )) %>%
  group_by(time) %>%
  summarise(
    pontos = sum(pontos),
    vitorias = sum(vitorias),
    empates = sum(empates),
    derrotas = sum(derrotas),
    saldo_gols = sum(saldo_gols),
    gols_pro = sum(gols_pro),
    gols_contra = sum(gols_contra),
    .groups = "drop"
  ) %>%
  arrange(desc(pontos))


tabela_atual


# função para os jogos restantes

preparar_jogos_restantes <- function(dados, parametros) {
  
  dados %>%
    filter(
      is.na(gols_mandante),
      is.na(gols_visitante)
    ) %>%
    left_join(
      parametros %>%
        select(time, theta_m, phi_m) %>%
        rename(
          theta_m_jogo = theta_m,
          phi_m_jogo = phi_m
        ),
      by = c("time_mandante" = "time")
    ) %>%
    left_join(
      parametros %>%
        select(time, theta_v, phi_v) %>%
        rename(
          theta_v_jogo = theta_v,
          phi_v_jogo = phi_v
        ),
      by = c("time_visitante" = "time")
    ) %>%
    mutate(
      lambda_m_jogo = (theta_m_jogo + phi_v_jogo) / 2,
      lambda_v_jogo = (theta_v_jogo + phi_m_jogo) / 2
    )
}


# jogos restantes modelo original

jogos_restantes_original <- preparar_jogos_restantes(
  brasileirao,
  parametros
)


# conferir parâmetros

jogos_restantes_original %>%
  select(
    rodada,
    time_mandante,
    time_visitante,
    lambda_m_jogo,
    lambda_v_jogo
  )

# simulação do campeonato inteiro 

simular_campeonato <- function(jogos_restantes, classificacao_atual) {
  resultados <- jogos_restantes %>%
    mutate(
      gols_mandante = rpois(n(), lambda_m_jogo),
      gols_visitante = rpois(n(), lambda_v_jogo)
    ) %>%
    mutate(
      pontos_m = case_when(
        gols_mandante > gols_visitante ~ 3,
        gols_mandante == gols_visitante ~ 1,
        TRUE ~ 0
      ),
      pontos_v = case_when(
        gols_visitante > gols_mandante ~ 3,
        gols_visitante == gols_mandante ~ 1,
        TRUE ~ 0
      ),
      vitoria_m = as.integer(gols_mandante > gols_visitante),
      vitoria_v = as.integer(gols_visitante > gols_mandante),
      saldo_m = gols_mandante - gols_visitante,
      saldo_v = gols_visitante - gols_mandante
    )
  resultados_mandantes <- resultados %>%
    transmute(
      time = time_mandante,
      pontos = pontos_m,
      vitorias = vitoria_m,
      saldo_gols = saldo_m,
      gols_pro = gols_mandante
    )
  resultados_visitantes <- resultados %>%
    transmute(
      time = time_visitante,
      pontos = pontos_v,
      vitorias = vitoria_v,
      saldo_gols = saldo_v,
      gols_pro = gols_visitante
    )
  novos_resultados <- bind_rows(
    resultados_mandantes,
    resultados_visitantes
  ) %>%
    group_by(time) %>%
    summarise(
      pontos_novos = sum(pontos),
      vitorias_novas = sum(vitorias),
      saldo_novo = sum(saldo_gols),
      gols_pro_novos = sum(gols_pro),
      .groups = "drop"
    )
  classificacao_final <- classificacao_atual %>%
    left_join(novos_resultados, by = "time") %>%
    mutate(
      pontos_novos = coalesce(pontos_novos, 0),
      vitorias_novas = coalesce(vitorias_novas, 0),
      saldo_novo = coalesce(saldo_novo, 0),
      gols_pro_novos = coalesce(gols_pro_novos, 0),
      
      pontos = pontos + pontos_novos,
      vitorias = vitorias + vitorias_novas,
      saldo_gols = saldo_gols + saldo_novo,
      gols_pro = gols_pro + gols_pro_novos
    ) %>%
    arrange(
      desc(pontos),
      desc(vitorias),
      desc(saldo_gols),
      desc(gols_pro)
    ) %>%
    mutate(colocacao = row_number())
  list(
    classificacao_final = classificacao_final,
    resultados = resultados
  )
}



# Monte Carlo para o modelo original

set.seed(204239)

B <- 10000

resultados_mc_original <- map_dfr(seq_len(B), function(b) {
  
  simular_campeonato(
    jogos_restantes = jogos_restantes_original,
    classificacao_atual = tabela_atual
  )$classificacao_final %>%
    mutate(simulacao = b)
  
})



# resumo das simulações: calcular todas as perguntas, armazenando assim menos info

resumir_simulacoes <- function(resultados) {
  prob_campeao <- resultados %>% #1
    group_by(time) %>%
    summarise(
      probabilidade_campeao = mean(colocacao == 1),
      .groups = "drop"
    ) %>%
    arrange(desc(probabilidade_campeao))
  prob_rebaixamento <- resultados %>% #2
    group_by(time) %>%
    summarise(
      probabilidade_rebaixamento = mean(colocacao >= 17),
      .groups = "drop"
    ) %>%
    arrange(desc(probabilidade_rebaixamento))
  prob_desempate <- resultados %>% #3
    group_by(simulacao) %>%
    summarise(
      empate = sum(pontos == max(pontos)) > 1,
      .groups = "drop"
    ) %>%
    summarise(
      probabilidade_desempate = mean(empate) #4
    )
  pontos_campeao <- resultados %>%
    filter(colocacao == 1)
  valor_esperado_pontos <- pontos_campeao %>%
    summarise(
      media = mean(pontos),
      mediana = median(pontos),
      desvio_padrao = sd(pontos),
      IC_2.5 = quantile(pontos, 0.025),
      IC_97.5 = quantile(pontos, 0.975)
    )
  list(
    prob_campeao = prob_campeao,
    prob_rebaixamento = prob_rebaixamento,
    prob_desempate = prob_desempate,
    pontos_campeao = pontos_campeao,
    valor_esperado_pontos = valor_esperado_pontos
  )
}


# calculo do erro padrao para o modelo original

B <- 10000

erro_mc <- resultados_mc_original %>%
  group_by(time) %>%
  summarise(
    prob_campeao = mean(colocacao == 1),
    prob_rebaixamento = mean(colocacao >= 17),
    prob_g4 = mean(colocacao <= 4),
    ep_mc_campeao = sqrt(
      prob_campeao * (1 - prob_campeao) / n()
    ),
    ep_mc_rebaixamento = sqrt(
      prob_rebaixamento * (1 - prob_rebaixamento) / n()
    ),
    ep_mc_g4 = sqrt(
      prob_g4 * (1 - prob_g4) / n()
    ),
    pontos_esperados = mean(pontos),
    ep_mc_pontos = sd(pontos) / sqrt(n()),
    posicao_media = mean(colocacao),
    ep_mc_posicao = sd(colocacao) / sqrt(n()),
    .groups = "drop"
  )

erro_mc

# resumo do modelo original

resumo_original <- resumir_simulacoes(resultados_mc_original)

resumo_original$prob_campeao
resumo_original$prob_rebaixamento
resumo_original$prob_desempate
resumo_original$valor_esperado_pontos

pontos_campeao <- resumo_original$pontos_campeao

# pontos para 90% de prob de ser campeao # 5


prob_campeao_pontos_original <- resultados_mc_original %>%
  group_by(pontos) %>%
  summarise(
    probabilidade = mean(colocacao == 1),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(pontos)


pontos_90_original <- prob_campeao_pontos_original %>%
  filter(probabilidade >= 0.90) %>%
  slice_min(pontos, n = 1)

# pontos para 95% de prob de n ser rebaixado # 6

prob_nao_rebaixamento_original <- resultados_mc_original %>%
  group_by(pontos) %>%
  summarise(
    probabilidade = mean(colocacao <= 16),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(pontos)

pontos_95_original <- prob_nao_rebaixamento_original %>%
  filter(probabilidade >= 0.95) %>%
  slice_min(pontos, n = 1)



########### bootstrap

set.seed(932402)

#função bootstrap

bootstrap_media <- function(x, B = 5000) {
  replicate(B, mean(sample(x, 
                           size = length(x),
                           replace = TRUE),
                    na.rm = TRUE))
}


# agrupando dados

dados_bootstrap <- bind_rows(brasileirao %>%
                               transmute(time = time_mandante,
                                         parametro = "theta_m",
                                         gols = gols_mandante),
                             brasileirao %>%
                               transmute(time = time_mandante,
                                         parametro = "phi_m",
                                         gols = gols_visitante),
                             brasileirao %>%
                               transmute(time = time_visitante,
                                         parametro = "theta_v",
                                         gols = gols_visitante),
                             brasileirao %>%
                               transmute(time = time_visitante,
                                         parametro = "phi_v",
                                         gols = gols_mandante))



#bootstrap dos quatro estimadores

bootstrap <- dados_bootstrap %>%
  filter(!is.na(gols)) %>%
  group_by(time, parametro) %>%
  summarise(bootstrap = list(bootstrap_media(gols, B = 5000)),
            .groups = "drop") %>%
  mutate(media = sapply(bootstrap, mean),
         IC_inferior = sapply(bootstrap,
                              quantile,
                              probs = 0.025),
         IC_superior = sapply(bootstrap,
                              quantile,
                              probs = 0.975)) %>%
  select(time, parametro, media, 
         IC_inferior, IC_superior)

# grafico bootstrap

bootstrap %>% 
  ggplot(aes(x = reorder(time, media), y = media,
             ymin = IC_inferior,ymax = IC_superior)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  facet_wrap(~ parametro, #scales = "free_x", 
             labeller = as_labeller(
               c(
                 theta_m = "theta[m]",
                 theta_v = "theta[v]",
                 phi_m   = "phi[m]",
                 phi_v   = "phi[v]"
               ),
               label_parsed
             )
  ) +
  coord_flip() + # mudar de lado
  labs(x = "Equipe",y = "Média de gols") +
  theme_bw(base_size = 16)

###########



# Distribuição das posições finais


distribuicao_posicoes <- resultados_mc_original %>%
  count(time, colocacao, name = "frequencia") %>%
  complete(
    time,
    colocacao = 1:20,
    fill = list(frequencia = 0)
  ) %>%
  group_by(time) %>%
  mutate(
    probabilidade = frequencia / sum(frequencia)
  ) %>%
  ungroup()

ordem_times <- resultados_mc_original %>%
  group_by(time) %>%
  summarise(
    posicao_media = mean(colocacao),
    .groups = "drop"
  ) %>%
  arrange(desc(posicao_media)) %>%
  pull(time)

distribuicao_posicoes <- distribuicao_posicoes %>%
  mutate(
    time = factor(time, levels = ordem_times)
  )


# Heatmap das posições

ggplot(
  distribuicao_posicoes,
  aes(
    x = colocacao,
    y = time,
    fill = probabilidade
  )
) +
  geom_tile(color = "white", linewidth = 0.15) +
  scale_x_continuous(breaks = 1:20) +
  scale_fill_viridis_c(
    labels = scales::percent,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  labs(
    x = "Posição final",
    y = "Equipe",
    fill = "Probabilidade",
    title = "Distribuição das posições finais"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())



# distribuição da pontuação

ggplot(
  resultados_mc_original,
  aes(
    x = reorder(time, pontos, FUN = median),
    y = pontos
  )
) +
  geom_boxplot(outlier.alpha = 0.1) +
  coord_flip() +
  labs(
    x = "Equipe",
    y = "Pontos finais",
    title = "Distribuição dos pontos finais por equipe"
  ) +
  theme_bw()



# probabilidade por faixas (campeao, g4, meio, z4)


prob_faixas <- resultados_mc_original %>%
  group_by(time) %>%
  summarise(
    Campeao = mean(colocacao == 1),
    G4 = mean(colocacao <= 4),
    Meio_tabela = mean(colocacao >= 5 & colocacao <= 16),
    Z4 = mean(colocacao >= 17),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Campeao, G4, Meio_tabela, Z4),
    names_to = "faixa",
    values_to = "probabilidade"
  )


ggplot(
  prob_faixas,
  aes(
    x = reorder(time, probabilidade),
    y = probabilidade,
    fill = faixa
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Equipe",
    y = "Probabilidade",
    fill = "Faixa",
    title = "Probabilidade de terminar em diferentes faixas"
  ) +
  theme_bw()



# posição media e ic 95%


resumo_posicoes <- resultados_mc_original %>%
  group_by(time) %>%
  summarise(
    media = mean(colocacao),
    mediana = median(colocacao),
    inferior = quantile(colocacao, 0.025),
    superior = quantile(colocacao, 0.975),
    .groups = "drop"
  )


ggplot(
  resumo_posicoes,
  aes(
    x = reorder(time, media),
    y = media,
    ymin = inferior,
    ymax = superior
  )
) +
  geom_pointrange() +
  coord_flip() +
  scale_y_reverse(breaks = 1:20) +
  labs(
    x = "Equipe",
    y = "Posição final",
    title = "Posição esperada e intervalo central de 95%"
  ) +
  theme_bw()



# distribuição dos pontos do campeão


ggplot(pontos_campeao, aes(x = pontos)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 1,
    boundary = 0
  ) +
  geom_density(linewidth = 1) +
  geom_vline(
    xintercept = mean(pontos_campeao$pontos),
    linetype = "dashed"
  ) +
  labs(
    x = "Pontos do campeão",
    y = "Densidade",
    title = "Distribuição dos pontos do campeão"
  ) +
  theme_bw()



# perfil ofensivo e defensivo


parametros %>%
  ggplot(
    aes(
      x = (theta_m + theta_v) / 2,
      y = (phi_m + phi_v) / 2,
      label = time
    )
  ) +
  geom_point() +
  geom_text(
    nudge_y = 0.03,
    check_overlap = TRUE
  ) +
  labs(
    x = "Média estimada de gols marcados",
    y = "Média estimada de gols sofridos",
    title = "Perfil ofensivo e defensivo das equipes"
  ) +
  theme_bw()



# resumo por equipe


resumo_equipes <- resultados_mc_original %>%
  group_by(time) %>%
  summarise(
    pontos_esperados = mean(pontos),
    desvio_pontos = sd(pontos),
    posicao_media = mean(colocacao),
    posicao_mediana = median(colocacao),
    prob_g4 = mean(colocacao <= 4),
    prob_z4 = mean(colocacao >= 17),
    prob_campeao = mean(colocacao == 1),
    prob_ultima_posicao = mean(colocacao == 20),
    .groups = "drop"
  )


resumo_equipes

# Resumo do campeonato

resumo_campeonato <- resultados_mc_original %>%
  group_by(simulacao) %>%
  summarise(
    pontos_campeao = max(pontos),
    pontos_vice = sort(pontos, decreasing = TRUE)[2],
    diferenca_campeao_vice = pontos_campeao - pontos_vice,
    empate_na_lideranca = sum(pontos == pontos_campeao) > 1,
    .groups = "drop"
  ) %>%
  summarise(
    media_pontos_campeao = mean(pontos_campeao),
    mediana_pontos_campeao = median(pontos_campeao),
    media_diferenca = mean(diferenca_campeao_vice),
    prob_empate_lideranca = mean(empate_na_lideranca)
  )


resumo_campeonato

# probabilidade de ser campeao por pontos

ggplot(
  prob_campeao_pontos_original,
  aes(x = pontos, y = probabilidade)
) +
  geom_line(linewidth = 1.2, color = "#2166AC") +
  geom_point(size = 2, color = "#2166AC") +
  geom_hline(
    yintercept = 0.90,
    linetype = "dashed",
    color = "red"
  ) +
  geom_vline(
    xintercept = pontos_90_original$pontos,
    linetype = "dashed",
    color = "red"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    x = "Pontos finais",
    y = "Probabilidade de ser campeão",
    title = "Probabilidade de ser campeão em função da pontuação"
  ) +
  theme_bw()

# probabilidade de nao ser rebaixado

ggplot(
  prob_nao_rebaixamento_original,
  aes(x = pontos, y = probabilidade)
) +
  geom_line(linewidth = 1.2, color = "#1B7837") +
  geom_point(size = 2, color = "#1B7837") +
  geom_hline(
    yintercept = 0.95,
    linetype = "dashed",
    color = "red"
  ) +
  geom_vline(
    xintercept = pontos_95_original$pontos,
    linetype = "dashed",
    color = "red"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    x = "Pontos finais",
    y = "Probabilidade de não ser rebaixado",
    title = "Probabilidade de não ser rebaixado em função da pontuação"
  ) +
  theme_bw()


# extra: modelo com decaimento temporal

decaimento <- 0.90

rodada_atual <- max(jogos_realizados$rodada)


jogos_ponderados <- jogos_realizados %>%
  mutate(
    peso = decaimento^(rodada_atual - rodada)
  )


# parâmetros ponderados
parametros_mandante_pond <- jogos_ponderados %>%
  group_by(time_mandante) %>%
  summarise(
    theta_m = weighted.mean(gols_mandante, peso),
    phi_m = weighted.mean(gols_visitante, peso),
    .groups = "drop"
  ) %>%
  rename(time = time_mandante)


parametros_visitante_pond <- jogos_ponderados %>%
  group_by(time_visitante) %>%
  summarise(
    theta_v = weighted.mean(gols_visitante, peso),
    phi_v = weighted.mean(gols_mandante, peso),
    .groups = "drop"
  ) %>%
  rename(time = time_visitante)


parametros_ponderado <- parametros_mandante_pond %>%
  full_join(parametros_visitante_pond, by = "time")


# jogos restantes utilizando os parâmetros ponderados
jogos_restantes_ponderado <- preparar_jogos_restantes(
  brasileirao,
  parametros_ponderado
)


# conferência dos lambdas
jogos_restantes_ponderado %>%
  select(
    rodada,
    time_mandante,
    time_visitante,
    lambda_m_jogo,
    lambda_v_jogo
  )

# simulação pelo modelo com decaimento (mesma seed)

set.seed(204239)

resultados_mc_ponderado <- map_dfr(seq_len(B), function(b) {
  
  simular_campeonato(
    jogos_restantes = jogos_restantes_ponderado,
    classificacao_atual = tabela_atual
  )$classificacao_final %>%
    mutate(simulacao = b)
  
})


# comparando os dois modelos

probabilidade_original <- resultados_mc_original %>%
  group_by(time) %>%
  summarise(
    probabilidade_original = mean(colocacao == 1),
    .groups = "drop"
  )


probabilidade_ponderado <- resultados_mc_ponderado %>%
  group_by(time) %>%
  summarise(
    probabilidade_ponderado = mean(colocacao == 1),
    .groups = "drop"
  )


# Probabilidade de título
comparacao_campeao <- probabilidade_original %>%
  full_join(
    probabilidade_ponderado,
    by = "time"
  )


comparacao_campeao

comparacao_campeao %>%
  pivot_longer(
    cols = c(
      probabilidade_original,
      probabilidade_ponderado
    ),
    names_to = "modelo",
    values_to = "probabilidade"
  ) %>%
  ggplot(
    aes(
      x = reorder(time, probabilidade),
      y = probabilidade,
      fill = modelo
    )
  ) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Equipe",
    y = "Probabilidade de ser campeão",
    fill = "Modelo",
    title = "Comparação da probabilidade de título"
  ) +
  theme_bw()


# Probabilidade de rebaixamento
comparacao_z4 <- resultados_mc_original %>%
  group_by(time) %>%
  summarise(
    probabilidade_original = mean(colocacao >= 17),
    .groups = "drop"
  ) %>%
  left_join(
    resultados_mc_ponderado %>%
      group_by(time) %>%
      summarise(
        probabilidade_ponderado = mean(colocacao >= 17),
        .groups = "drop"
      ),
    by = "time"
  )


comparacao_z4


# resumo final - modelo ponderado

resumo_ponderado <- resumir_simulacoes(
  resultados_mc_ponderado
)


resumo_ponderado$prob_campeao
resumo_ponderado$prob_rebaixamento
resumo_ponderado$prob_desempate
resumo_ponderado$valor_esperado_pontos


# cálculo do erro - modelo com decaimento

# Numero de simulacoes
B <- 10000

# Erro-padrao de Monte Carlo das probabilidades e medias
erro_mc_ponderado <- resultados_mc_ponderado %>%
  group_by(time) %>%
  summarise(
    prob_campeao = mean(colocacao == 1),
    prob_rebaixamento = mean(colocacao >= 17),
    prob_g4 = mean(colocacao <= 4),
    ep_mc_campeao = sqrt(
      prob_campeao * (1 - prob_campeao) / n()
    ),
    ep_mc_rebaixamento = sqrt(
      prob_rebaixamento * (1 - prob_rebaixamento) / n()
    ),
    ep_mc_g4 = sqrt(
      prob_g4 * (1 - prob_g4) / n()
    ),
    pontos_esperados = mean(pontos),
    ep_mc_pontos = sd(pontos) / sqrt(n()),
    posicao_media = mean(colocacao),
    ep_mc_posicao = sd(colocacao) / sqrt(n()),
    .groups = "drop"
  )

erro_mc_ponderado
