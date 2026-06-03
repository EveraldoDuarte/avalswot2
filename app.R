library(shiny)
library(shinydashboard)
library(shinyWidgets) 
library(googlesheets4)
library(tidyverse)
library(DT)

# 1. FUNÇÕES AUXILIARES
id_sanitizado <- function(texto) {
  str_replace_all(texto, "[^[:alnum:]]", "_")
}

# CONFIGURAÇÃO DO GOOGLE SHEETS
ID_PLANILHA <- "1hFzF0azeTfZruFFTzX8mWMtNIG5fKVsVr3QNEt-vDSk"
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "everaldo.duarte@reitria.ifpe.edu.br"
)

gs4_auth(email = "everaldo.duarte@reitoria.ifpe.edu.br", cache = ".secrets")

# Itens resumidos para as tabelas
forcas <- c("F1: Qualidade do Ensino",
            "F2: Qualificação dos Servidores",
            "F3: Inserção Social e Capilaridade Territorial ",
            "F4: Políticas Acadêmicas e de Inclusão",
            "F5: Gestão Institucional e Cultura de Inovação")
fraquezas <- c("P1: Infraestrutura Insuficiente",
               "P2: Carência de Pessoal",
               "P3: Evasão Estudantil",
               "P4: Processos Burocráticos",
               "P5: Comunicação Interna")
oportunidades <- c("O1: Parcerias Institucionais",
                   "O2: Inovação Tecnológica",
                   "O3: Demandas do Mercado de Trabalho",
                   "O4: Expansão e Verticalização de Cursos",
                   "O5: Políticas Públicas Favoráveis")
ameacas <- c("A1: Restrições Orçamentárias",
             "A2: Instabilidade Política",
             "A3: Desvalorização da Educação Pública",
             "A4: Mudanças na Legislação e Reforma Administrativa",
             "A5: Saúde Mental e Cenário Pós-Pandêmico")

# Estilo CSS Customizado (Opção 2 de destaque forte)
css_customizado <- "
  .tabela-swot { width: 100%; margin-bottom: 20px; border-collapse: separate; border-spacing: 5px; table-layout: fixed; }
  .tabela-swot th { background-color: #34495e; color: white; text-align: center; padding: 10px; border-radius: 4px; font-size: 12px; word-wrap: break-word; }
  .tabela-swot td { padding: 6px; background-color: #f8f9fa; border-radius: 4px; vertical-align: middle; }
  .titulo-linha { font-weight: bold; color: #2c3e50; font-size: 12px; word-wrap: break-word; }
  
  .tabela-swot .btn-group-sm>.btn, .tabela-swot .btn-sm {
    padding: 5px 8px !important;
    font-size: 12px !important;
    background-color: #f3f4f6 !important;
    color: #4b5563 !important;
    border-color: #d1d5db !important;
  }

  /* REGRAS DE DESTAQUE ATIVO (OPÇÃO 2) */
  .btn-success.active, .btn-success:active {
    background-color: #15803d !important; color: white !important; border-color: #166534 !important;
    box-shadow: 0 0 8px rgba(22, 101, 52, 0.6) !important; font-weight: bold !important;
  }
  .btn-info.active, .btn-info:active {
    background-color: #1d4ed8 !important; color: white !important; border-color: #1e40af !important;
    box-shadow: 0 0 8px rgba(30, 64, 175, 0.6) !important; font-weight: bold !important;
  }
  .btn-warning.active, .btn-warning:active {
    background-color: #ea580c !important; color: white !important; border-color: #c2410c !important;
    box-shadow: 0 0 8px rgba(194, 65, 12, 0.6) !important; font-weight: bold !important;
  }
  .btn-danger.active, .btn-danger:active {
    background-color: #be123c !important; color: white !important; border-color: #9f1239 !important;
    box-shadow: 0 0 8px rgba(159, 18, 57, 0.6) !important; font-weight: bold !important;
  }
"

ui <- dashboardPage(
  dashboardHeader(title = "SWOT Cruzada - PDI27-31"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML(css_customizado))), 
    
    fluidRow(
      box(
        title = "⚡ Painel de Avaliação Estratégica", width = 12, status = "primary", solidHeader = TRUE,
        h3("Olá! Chegou a hora de avaliar a relação entre os itens de nossa Matriz SWOT."),
        h4 ("Abaixo, analise o impacto e a relevância de cada cruzamento, atribuindo uma nota de 0 a 5. Ao final você terá uma visão consolidada do seu posicionamento estratégico."),
        HTML("<strong>Critérios de Pontuação (0 a 5)</strong><br>
     <strong>0 – Sem impacto / Irrelevante:</strong> O fator interno não tem nenhuma relação com o fator externo. O cruzamento não gera nenhuma ação útil.<br>
     <strong>1 – Impacto muito baixo:</strong> A relação existe, mas é quase imperceptível ou irrelevante para o futuro do negócio.<br>
     <strong>2 – Impacto baixo:</strong> Há uma conexão fraca. A força ajuda pouco na oportunidade, ou a fraqueza sofre pouca influência da ameaça.<br>
     <strong>3 – Impacto moderado:</strong> Relação média. O cruzamento exige atenção, mas não é uma prioridade imediata de investimento ou correção.<br>
     <strong>4 – Impacto alto:</strong> Conexão direta e forte. Ação altamente recomendada, pois gera grande vantagem competitiva ou mitiga um risco claro.<br>
     <strong>5 – Impacto muito alto / Crítico:</strong> Relação vital. O cruzamento é crucial para a sobrevivência ou para o crescimento imediato da empresa (prioridade máxima)")
        
      )
        
    ),
    
    tabBox(
      title = "Matrizes de Cruzamento", width = 12, id = "abas_swot",
      
      # ABA 1: FORÇAS X OPORTUNIDADES
      tabPanel(
        "🟢 Forças x Oportunidades",
        h4("Estratégia Ofensiva: Como usar nossas Forças para impulsionar as Oportunidades?"), hr(),
        tags$table(class = "tabela-swot",
                   tags$tr(tags$th(style = "width: 20%;", "Fatores"), lapply(oportunidades, function(o) tags$th(style = "width: 16%;", o))),
                   lapply(forcas, function(f) {
                     tags$tr(
                       tags$td(class = "titulo-linha", f),
                       lapply(oportunidades, function(o) {
                         tags$td(align = "center", radioGroupButtons(inputId = paste0("cruz_", id_sanitizado(f), "__", id_sanitizado(o)), choices = 0:5, selected = 0, status = "success", size = "sm"))
                       })
                     )
                   })
        )
      ),
      
      # ABA 2: FORÇAS X AMEAÇAS
      tabPanel(
        "🔵 Forças x Ameaças",
        h4("Estratégia de Confronto: Como usar nossas Forças para mitigar as Ameaças?"), hr(),
        tags$table(class = "tabela-swot",
                   tags$tr(tags$th(style = "width: 20%;", "Fatores"), lapply(ameacas, function(a) tags$th(style = "width: 16%;", a))),
                   lapply(forcas, function(f) {
                     tags$tr(
                       tags$td(class = "titulo-linha", f),
                       lapply(ameacas, function(a) {
                         tags$td(align = "center", radioGroupButtons(inputId = paste0("cruz_", id_sanitizado(f), "__", id_sanitizado(a)), choices = 0:5, selected = 0, status = "info", size = "sm"))
                       })
                     )
                   })
        )
      ),
      
      # ABA 3: FRAQUEZAS X OPORTUNIDADES
      tabPanel(
        "🟠 Fraquezas x Oportunidades",
        h4("Estratégia de Reforço: Como minimizar as Fraquezas aproveitando as Oportunidades?"), hr(),
        tags$table(class = "tabela-swot",
                   tags$tr(tags$th(style = "width: 20%;", "Fatores"), lapply(oportunidades, function(o) tags$th(style = "width: 16%;", o))),
                   lapply(fraquezas, function(p) {
                     tags$tr(
                       tags$td(class = "titulo-linha", p),
                       lapply(oportunidades, function(o) {
                         tags$td(align = "center", radioGroupButtons(inputId = paste0("cruz_", id_sanitizado(p), "__", id_sanitizado(o)), choices = 0:5, selected = 0, status = "warning", size = "sm"))
                       })
                     )
                   })
        )
      ),
      
      # ABA 4: FRAQUEZAS X AMEAÇAS
      tabPanel(
        "🔴 Fraquezas x Ameaças",
        h4("Estratégia de Defesa: Como reduzir as Fraquezas para evitar que as Ameaças nos destruam?"), hr(),
        tags$table(class = "tabela-swot",
                   tags$tr(tags$th(style = "width: 20%;", "Fatores"), lapply(ameacas, function(a) tags$th(style = "width: 16%;", a))),
                   lapply(fraquezas, function(p) {
                     tags$tr(
                       tags$td(class = "titulo-linha", p),
                       lapply(ameacas, function(a) {
                         tags$td(align = "center", radioGroupButtons(inputId = paste0("cruz_", id_sanitizado(p), "__", id_sanitizado(a)), choices = 0:5, selected = 0, status = "danger", size = "sm"))
                       })
                     )
                   })
        )
      ),
      
      # ABA 5: GRÁFICO DE POSICIONAMENTO ESTRATÉGICO
      tabPanel(
        "📊 Posicionamento Estratégico",
        fluidRow(
          column(width = 8,
                 box(title = "Gráfico de Quadrantes SWOT", width = NULL, status = "primary", solidHeader = TRUE,
                     plotOutput("grafico_posicionamento", height = "450px")
                 )
          ),
          column(width = 4,
                 box(title = "Situação Atual", width = NULL, status = "warning", solidHeader = TRUE,
                     uiOutput("card_diagnostico")
                 )
          )
        ),
        fluidRow(
          box(width = 12, align = "center", style = "background-color: transparent; border: none;",
              actionButton("btn_enviar", "Gravar Matriz Completa", class = "btn-success btn-lg", 
                           style = "padding: 15px 40px; font-size: 18px; font-weight: bold; border-radius: 8px;"))
        ),
        fluidRow(
         box( p(
            "Metodologia do cálculo para o posicionamento estratégico: ",
            a("Clique aqui para acessar", 
              href = "metodo_calculo.pdf", 
              target = "_blank")
          ))
        )
      ),
      # Tabela das notas das avaliações 
      tabPanel(
        "Tabela de avaliação completa",
        fluidRow(
          box(
            title = "Resultados da Matriz TOWS", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            # Função na UI que reserva o espaço para a tabela
            DTOutput("tabela_tows") 
          )
        )
      )
    ),
    
    fluidRow(
      box(width = 12, align = "center", style = "background-color: transparent; border: none;",
          p(""))
    )
  )
)

server <- function(input, output, session) {
  
  # 1. CAPTURA REATIVA DOS DADOS DA MATRIZ (Gráfico atualiza em tempo real)
  dados_reativos <- reactive({
    ler_inputs <- function(origem_vetor, destino_vetor) {
      expand.grid(Origem = origem_vetor, Destino = destino_vetor, stringsAsFactors = FALSE) %>%
        rowwise() %>%
        mutate(
          id = paste0("cruz_", id_sanitizado(Origem), "__", id_sanitizado(Destino)),
          Nota = if(is.null(input[[id]])) 0 else as.numeric(input[[id]])
        ) %>%
        ungroup()
    }
    
    bind_rows(
      ler_inputs(forcas, oportunidades),
      ler_inputs(forcas, ameacas),
      ler_inputs(fraquezas, oportunidades),
      ler_inputs(fraquezas, ameacas)
    )
  })
  
  # 2. CÁLCULO E RENDERIZAÇÃO DO GRÁFICO DE PLOTAGEM
  output$grafico_posicionamento <- renderPlot({
    res <- dados_reativos()
    
    # Filtros de ids por matriz
    ids_fxo <- paste0("cruz_", id_sanitizado(expand.grid(forcas, oportunidades)$Var1), "__", id_sanitizado(expand.grid(forcas, oportunidades)$Var2))
    ids_fxa <- paste0("cruz_", id_sanitizado(expand.grid(forcas, ameacas)$Var1), "__", id_sanitizado(expand.grid(forcas, ameacas)$Var2))
    ids_pxo <- paste0("cruz_", id_sanitizado(expand.grid(fraquezas, oportunidades)$Var1), "__", id_sanitizado(expand.grid(fraquezas, oportunidades)$Var2))
    ids_pxa <- paste0("cruz_", id_sanitizado(expand.grid(fraquezas, ameacas)$Var1), "__", id_sanitizado(expand.grid(fraquezas, ameacas)$Var2))
    
    m_fxo <- mean(res$Nota[res$id %in% ids_fxo])
    m_fxa <- mean(res$Nota[res$id %in% ids_fxa])
    m_pxo <- mean(res$Nota[res$id %in% ids_pxo])
    m_pxa <- mean(res$Nota[res$id %in% ids_pxa])
    
    # Coordenadas vetoriais matemáticas do posicionamento (-5 a 5)
    eixo_x <- m_fxo + m_fxa - m_pxo - m_pxa
    eixo_y <- m_fxo + m_pxo - m_fxa - m_pxa
    
    ggplot() +
      geom_vline(xintercept = 0, color = "#7f8c8d", linetype = "dashed", size = 1) +
      geom_hline(yintercept = 0, color = "#7f8c8d", linetype = "dashed", size = 1) +
      
      annotate("text", x = 2.5, y = 2.5, label = "OFENSIVA\n(Desenvolvimento)", color = "#15803d", fontface = "bold", size = 4) +
      annotate("text", x = 2.5, y = -2.5, label = "CONFRONTO\n(Manutenção)", color = "#1d4ed8", fontface = "bold", size = 4) +
      annotate("text", x = -2.5, y = 2.5, label = "REFORÇO\n(Crescimento)", color = "#ea580c", fontface = "bold", size = 4) +
      annotate("text", x = -2.5, y = -2.5, label = "DEFESA\n(Sobrevivência)", color = "#be123c", fontface = "bold", size = 4) +
      
      geom_point(aes(x = eixo_x, y = eixo_y), color = "#2c3e50", size = 6) +
      geom_segment(aes(x = 0, y = 0, xend = eixo_x, yend = eixo_y), color = "#2c3e50", arrow = arrow(length = unit(0.3, "cm")), size = 1.2) +
      
      xlim(-5, 5) + ylim(-5, 5) +
      labs(x = "◀ Fraquezas --- Ambiente Interno --- Forças ▶", 
           y = "◀ Ameaças --- Ambiente Externo --- Oportunidades ▶",
           title = "Vetor de Macroposicionamento Estratégico") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 10, color = "#34495e"),
        axis.title.y = element_text(face = "bold", size = 10, color = "#34495e")
      )
  })
  
  # 3. TEXTO INFORMATIVO DE DIAGNÓSTICO DO QUADRANTE
  output$card_diagnostico <- renderUI({
    res <- dados_reativos()
    
    ids_fxo <- paste0("cruz_", id_sanitizado(expand.grid(forcas, oportunidades)$Var1), "__", id_sanitizado(expand.grid(forcas, oportunidades)$Var2))
    ids_fxa <- paste0("cruz_", id_sanitizado(expand.grid(forcas, ameacas)$Var1), "__", id_sanitizado(expand.grid(forcas, ameacas)$Var2))
    ids_pxo <- paste0("cruz_", id_sanitizado(expand.grid(fraquezas, oportunidades)$Var1), "__", id_sanitizado(expand.grid(fraquezas, oportunidades)$Var2))
    ids_pxa <- paste0("cruz_", id_sanitizado(expand.grid(fraquezas, ameacas)$Var1), "__", id_sanitizado(expand.grid(fraquezas, ameacas)$Var2))
    
    m_fxo <- mean(res$Nota[res$id %in% ids_fxo])
    m_fxa <- mean(res$Nota[res$id %in% ids_fxa])
    m_pxo <- mean(res$Nota[res$id %in% ids_pxo])
    m_pxa <- mean(res$Nota[res$id %in% ids_pxa])
    
    eixo_x <- m_fxo + m_fxa - m_pxo - m_pxa
    eixo_y <- m_fxo + m_pxo - m_fxa - m_pxa
    
    quadrante <- case_when(
      eixo_x >= 0 & eixo_y >= 0 ~ "Estratégia Ofensiva (Aproveitar forças internas para alavancar oportunidades externas).",
      eixo_x >= 0 & eixo_y < 0  ~ "Estratégia de Confronto (Utilizar forças do ambiente interno para mitigar ameaças do mercado).",
      eixo_x < 0  & eixo_y >= 0 ~ "Estratégia de Reforço (Minimizar debilidades internas tirando vantagem das janelas de oportunidade).",
      TRUE                      ~ "Estratégia de Defesa (Reduzir vulnerabilidades para evitar que ameaças causem danos críticos)."
    )
    
    tagList(
      h4("Postura Indicada:", style = "font-weight: bold;"),
      p(quadrante, style = "font-size: 26px; color: #2c3e50;"),
      hr(),
      p(tags$b("Pontuação Ambiente Interno (X): "), round(eixo_x, 2)),
      p(tags$b("Pontuação Ambiente Externo (Y): "), round(eixo_y, 2))
    )
  })
  
  # 4. ENVIO DOS DADOS BRUTOS PARA O GOOGLE SHEETS
  observeEvent(input$btn_enviar, {
    withProgress(message = 'Armazenando dados no Google Sheets...', value = 0.5, {
      
      timestamp_atual <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      gerar_df <- function(origem_vetor, destino_vetor, label_cruzamento) {
        expand.grid(Origem = origem_vetor, Destino = destino_vetor, stringsAsFactors = FALSE) %>%
          rowwise() %>%
          mutate(
            Data_Hora = timestamp_atual,
            Tipo_Cruzamento = label_cruzamento,
            Fator_Origem = Origem,
            Fator_Destino = Destino,
            id = paste0("cruz_", id_sanitizado(Origem), "__", id_sanitizado(Destino)),
            Nota = as.numeric(input[[id]])
          ) %>%
          ungroup() %>%
          select(Data_Hora, Tipo_Cruzamento, Fator_Origem, Fator_Destino, Nota)
      }
      
      dados_respostas <- bind_rows(
        gerar_df(forcas, oportunidades, "Forças x Oportunidades"),
        gerar_df(forcas, ameacas, "Forças x Ameaças"),
        gerar_df(fraquezas, oportunidades, "Fraquezas x Oportunidades"),
        gerar_df(fraquezas, ameacas, "Fraquezas x Ameaças")
      )
      
      sheet_append(ss = ID_PLANILHA, data = dados_respostas)
    })
    
    # Função no Server que renderiza o seu Data Frame
    output$tabela_tows <- renderDT({
      datatable(
        dados_respostas,
        options = list(
          pageLength = 5,       # Quantas linhas mostra por página
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json') # Traduz para Português
        )
      )
    })
    
    showModal(modalDialog(
      title = "Sucesso!", "Sua matriz SWOT cruzada foi gravada com sucesso!",
      easyClose = TRUE, footer = modalButton("Fechar")
    ))
  })
}

shinyApp(ui = ui, server = server)
