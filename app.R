library(shiny)
library(shinydashboard)
library(shinyWidgets) 
library(googlesheets4)
library(tidyverse)

# 1. FUNÇÃO AUXILIAR
id_sanitizado <- function(texto) {
  str_replace_all(texto, "[^[:alnum:]]", "_")
}

# CONFIGURAÇÃO DO GOOGLE SHEETS
ID_PLANILHA <- "1hFzF0azeTfZruFFTzX8mWMtNIG5fKVsVr3QNEt-vDSk"
#gs4_deauth() 
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "everaldo.duarte@reitria.ifpe.edu.br" # Coloque seu e-mail aqui
)

gs4_auth(email = "everaldo.duarte@reitoria.ifpe.edu.br", cache = ".secrets")

# Itens resumidos para caber perfeitamente na tabela visual
forcas <- c("F1: Qualidade do Ensino", "F2: Qualificação dos Servidores", "F3: Equipe", "F4: Tecnologia", "F5: Finanças")
fraquezas <- c("P1: Fornecedores", "P2: TI Obsoleta", "P3: Marketing", "P4: Turn-over", "P5: Digital")
oportunidades <- c("O1: Novos Mercados", "O2: Novas Techs", "O3: Leis", "O4: Demanda", "O5: Parcerias")
ameacas <- c("A1: Concorrência", "A2: Economia", "A3: Mão de Obra", "A4: Consumidor", "A5: Impostos")

# Estilo CSS Customizado Corrigido
css_customizado <- "
  .tabela-swot { width: 100%; margin-bottom: 20px; border-collapse: separate; border-spacing: 5px; }
  .tabela-swot th { background-color: #34495e; color: white; text-align: center; padding: 10px; border-radius: 4px; font-size: 13px; }
  .tabela-swot td { padding: 8px; background-color: #f8f9fa; border-radius: 4px; vertical-align: middle; }
  .titulo-linha { font-weight: bold; color: #2c3e50; min-width: 120px; }
"

# 2. INTERFACE DO USUÁRIO (UI)
ui <- dashboardPage(
  dashboardHeader(title = "SWOT Cruzada Premium"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML(css_customizado))), 
    
    fluidRow(
      box(
        title = "⚡ Painel de Avaliação Estratégica", width = 12, status = "primary", solidHeader = TRUE,
        p("Selecione o impacto de cruzamento diretamente nos blocos de notas de 0 a 5."),
        p("A interface em formato de matriz permite que você compare suas respostas horizontal e verticalmente de forma rápida.")
      )
    ),
    
    tabBox(
      title = "Matrizes de Cruzamento", width = 12, id = "abas_swot",
      
      # ABA 1: FORÇAS X OPORTUNIDADES
      tabPanel(
        "🟢 Forças x Oportunidades",
        h4("Estratégia Ofensiva: Como usar nossas Forças para impulsionar as Oportunidades?"), hr(),
        tags$table(class = "tabela-swot",
                   tags$tr(
                     tags$th("Fatores"), lapply(oportunidades, tags$th)
                   ),
                   lapply(forcas, function(f) {
                     tags$tr(
                       tags$td(class = "titulo-linha", f),
                       lapply(oportunidades, function(o) {
                         tags$td(align = "center",
                                 radioGroupButtons(
                                   inputId = paste0("cruz_", id_sanitizado(f), "__", id_sanitizado(o)),
                                   choices = 0:5, selected = 0, status = "success", size = "sm"
                                 )
                         )
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
                   tags$tr(
                     tags$th("Fatores"), lapply(ameacas, tags$th)
                   ),
                   lapply(forcas, function(f) {
                     tags$tr(
                       tags$td(class = "titulo-linha", f),
                       lapply(ameacas, function(a) {
                         tags$td(align = "center",
                                 radioGroupButtons(
                                   inputId = paste0("cruz_", id_sanitizado(f), "__", id_sanitizado(a)),
                                   choices = 0:5, selected = 0, status = "info", size = "sm"
                                 )
                         )
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
                   tags$tr(
                     tags$th("Fatores"), lapply(oportunidades, tags$th)
                   ),
                   lapply(fraquezas, function(p) {
                     tags$tr(
                       tags$td(class = "titulo-linha", p),
                       lapply(oportunidades, function(o) {
                         tags$td(align = "center",
                                 radioGroupButtons(
                                   inputId = paste0("cruz_", id_sanitizado(p), "__", id_sanitizado(o)),
                                   choices = 0:5, selected = 0, status = "warning", size = "sm"
                                 )
                         )
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
                   tags$tr(
                     tags$th("Fatores"), lapply(ameacas, tags$th)
                   ),
                   lapply(fraquezas, function(p) {
                     tags$tr(
                       tags$td(class = "titulo-linha", p),
                       lapply(ameacas, function(a) {
                         tags$td(align = "center",
                                 radioGroupButtons(
                                   inputId = paste0("cruz_", id_sanitizado(p), "__", id_sanitizado(a)),
                                   choices = 0:5, selected = 0, status = "danger", size = "sm"
                                 )
                         )
                       })
                     )
                   })
        )
      )
    ),
    
    # Botão de salvar
    fluidRow(
      box(
        width = 12, align = "center", style = "background-color: transparent; border: none;",
        actionButton("btn_enviar", "Gravar Matriz Completa", class = "btn-success btn-lg", 
                     style = "padding: 15px 40px; font-size: 18px; font-weight: bold; border-radius: 8px;")
      )
    )
  )
)

# 3. SERVIDOR (SERVER)
server <- function(input, output, session) {
  
  observeEvent(input$btn_enviar, {
    withProgress(message = 'Armazenando dados no Google Sheets...', value = 0.5, {
      
      dados_respostas <- data.frame()
      timestamp_atual <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      processar_cruzamento <- function(vetor_origem, vetor_destino, tipo_cruzamento) {
        df_temp <- data.frame()
        for(origem in vetor_origem) {
          for(destino in vetor_destino) {
            id_input <- paste0("cruz_", id_sanitizado(origem), "__", id_sanitizado(destino))
            nota_atual <- input[[id_input]]
            
            linha <- data.frame(
              Data_Hora = timestamp_atual,
              Tipo_Cruzamento = tipo_cruzamento,
              Fator_Origem = origem,
              Fator_Destino = destino,
              Nota = as.numeric(nota_atual),
              stringsAsFactors = FALSE
            )
            df_temp <- rbind(df_temp, linha)
          }
        }
        return(df_temp)
      }
      
      df_fxo <- processar_cruzamento(forcas, oportunidades, "Forças x Oportunidades")
      df_fxa <- processar_cruzamento(forcas, ameacas, "Forças x Ameaças")
      df_pxo <- processar_cruzamento(fraquezas, oportunidades, "Fraquezas x Oportunidades")
      df_pxa <- processar_cruzamento(fraquezas, ameacas, "Fraquezas x Ameaças")
      
      dados_respostas <- rbind(df_fxo, df_fxa, df_pxo, df_pxa)
      
      tryCatch({
        sheet_append(ss = ID_PLANILHA, data = dados_respostas)
        
        showModal(modalDialog(
          title = "✨ Sucesso absoluto!",
          "Sua avaliação foi salva. Os dados foram computados na nuvem.",
          easyClose = TRUE, footer = modalButton("Ok, fechar")
        ))
        
        all_inputs <- names(reactiveValuesToList(input))
        cruz_inputs <- all_inputs[str_detect(all_inputs, "^cruz_")]
        for(inp in cruz_inputs) {
          updateRadioGroupButtons(session, inp, selected = 0)
        }
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "⚠️ Falha no salvamento",
          paste("Erro de conexão com o Google Drive:", e$message),
          easyClose = TRUE, footer = modalButton("Voltar e tentar novamente")
        ))
      })
      
    })
  })
}

shinyApp(ui = ui, server = server)