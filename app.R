# Aplicação Shiny: Espécies brasileiras ameaçadas de extinção
# Autor: Carlos Eduardo de Siqueira

# Carregar pacotes necessários
library(shiny)
library(magrittr)
library(flora)

# Setup inicial
# Tradução da tabela
pt_BR <- "//cdn.datatables.net/plug-ins/2.3.2/i18n/pt-BR.json"

# Carrega as listas de espécies ameaçadas do sul do brasil
ameacadasPR <- readr::read_rds("listas/ameacadasPR.rds")
ameacadasSC <- readr::read_rds("listas/ameacadasSC.rds")
ameacadasRS <- readr::read_rds("listas/ameacadasRS.rds")
mma23_sc <- readr::read_rds("listas/fauna_ameacada.rds")
status_cnc <- readr::read_rds("listas/status_cnc.rds")
port443 <- readr::read_rds("listas/port443.rds")

# Interface do Usuário (UI)
ui <- fluidPage(
  title = "Espécies brasileiras ameaçadas de extinção",

  # Cabeçalho com informações do autor
  fluidRow(
    column(
      12,
      h1("Espécies brasileiras ameaçadas de extinção"),
      p(
        "Autor: Carlos Eduardo de Siqueira ",
        a(
          href = "mailto:carlossiqueira@ima.sc.gov.br?subject=Dúvida%20App%20Espécies%20brasileiras%20ameaçadas%20de%20extinção",
          img(src = "email28.png", alt = "envie um email")
        ),
        a(
          href = "https://github.com/cadubio/ameacaR",
          img(src = "GitHub-Mark-32px.png", height = "28px", alt = "Github")
        )
      )
    )
  ),

  # Seção "O que é?"
  fluidRow(
    column(
      12,
      h2("O que é?"),
      p(
        "Aqui você pode consultar quais espécies de plantas nativas do Brasil são consideradas ameaçadas de extinção."
      ),
      p(
        "A ",
        a(
          href = "https://www.oeco.org.br/dicionario-ambiental/27904-entenda-a-classificacao-da-lista-vermelha-da-iucn/",
          "categoria de ameaça"
        ),
        " de cada táxon será verificada em nível nacional, com consultas à ",
        a(
          href = "https://in.gov.br/en/web/dou/-/portaria-mma-n-148-de-7-de-junho-de-2022-406272733",
          "Portaria n. 148/2022 do Ministério do Meio Ambiente (MMA22)"
        ),
        ", ",
        a(
          href = "http://dados.gov.br/dataset/portaria_443",
          "Portaria n. 443/2014 do Ministério do Meio Ambiente (MMA14)"
        ),
        " e ao ",
        a(
          href = "http://cncflora.jbrj.gov.br/portal",
          "Centro Nacional de Conservação da Flora (CNCFlora)"
        ),
        ". A consulta também é realizada em nível estadual, pela ",
        a(
          href = "https://www.sde.sc.gov.br/index.php/biblioteca/consema/legislacao/resolucoes/325-resolucao-consema-no-512014-1/file",
          "Resolução CONSEMA n. 51/2014 (SC)"
        ),
        " - Santa Catarina, pela ",
        a(
          href = "https://www.sociedadechaua.org/publicacoes",
          "Lista Vermelha da Flora Ameaçada no Estado do Paraná (PR)"
        ),
        " e pelo ",
        a(
          href = "http://www.mcn.fzb.rs.gov.br/conteudo/4816/?Homologada_a_nova_Lista_da_Flora_Ga%C3%BAcha_Amea%C3%A7ada_de_Extin%C3%A7%C3%A3o",
          "Decreto n. 51.259/2014 (RS)"
        ),
        " - Rio Grande do Sul."
      ),
      p(
        "Os nomes científicos, gêneros ou espécies, são validados conforme ",
        a(href = "http://floradobrasil.jbrj.gov.br/", "Flora do Brasil 2020"),
        ", através do pacote R ",
        a(href = "http://www.github.com/gustavobio/flora", "FLORA"),
        "."
      ),
      p(
        "Para mais detalhes sobre as a validação dos nomes científicos das listas estaduais veja ",
        a(
          href = "https://github.com/cadubio/ameacaR#notas-sobres-as-listas-estaduais",
          "Notas sobres as listas estaduais"
        ),
        "."
      )
    )
  ),

  # Seção "Como usar?"
  fluidRow(
    column(
      12,
      h3("Como usar?"),
      p("A pesquisa pode feita de ", strong("duas maneiras diferentes"), "."),
      tags$ol(
        tags$li(
          "Você pode ",
          strong("colar"),
          " ou ",
          strong("digitar"),
          " os nomes científicos, gêneros ou espécies, no campo à esquerda, ou"
        ),
        tags$li(
          "Você pode clicar em ",
          strong("enviar planilha"),
          " para submeter uma planilha com os nomes científicos. Após o envio, escolha a coluna da planilha que contém os nomes científicos a serem verificados, e clique no botão \"verificar ameaçadas\". Pode-se clicar em \"ver coluna\" para uma amostra com as 6 primeiras linhas da coluna selecionada."
        )
      ),
      p("Veja em ", strong("Tutoriais"), " os vídeos de como usar.")
    )
  ),

  # Painéis principais
  tabsetPanel(
    # Painel FLORA - Pesquisar por Espécie
    tabPanel(
      "FLORA - Pesquisar por Espécie",
      br(),
      fluidPage(
        fluidRow(
          column(
            width = 5,
            textAreaInput(
              inputId = "nomesColados",
              label = "Cole ou digite um nome científico por linha",
              value = "",
              height = "200px",
              placeholder = "Exemplo:\nOcotea catharinensis\nAcianthera langeana\nBegonia fruticosa\nXylopia aromatica\nGuatteria australis\n..."
            )
          ),
          column(
            width = 4,
            fileInput(
              inputId = "planilha",
              label = "",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                ".csv",
                ".xlsx"
              ),
              buttonLabel = "Enviar planilha",
              placeholder = "⬅ clicar"
            ),
            uiOutput("sel_colunas")
          ),
          column(
            width = 3,
            tableOutput("observaColuna")
          )
        ),
        hr(),
        fluidRow(column(
          width = 12,
          DT::dataTableOutput("dados_colados"),
          DT::dataTableOutput("dados_recebidos")
        ))
      )
    ),

    # Painel FLORA - Pesquisar por Gênero
    tabPanel(
      "FLORA - Pesquisar por Gênero",
      br(),
      fluidPage(
        fluidRow(
          column(
            width = 12,
            textAreaInput(
              inputId = "genero",
              label = "Digite abaixo um gênero por linha",
              value = "",
              height = "200px",
              placeholder = "Exemplo:\nOcotea\nAcianthera\nBegonia\nXylopia\nGuatteria\n..."
            )
          )
        ),
        hr(),
        fluidRow(column(
          width = 12,
          DT::dataTableOutput("genero_out")
        ))
      )
    ),

    # Painel FAUNA - Pesquisar por Espécie
    tabPanel(
      "FAUNA - Pesquisar por Espécie",
      br(),
      fluidPage(
        fluidRow(
          column(
            width = 12,
            textAreaInput(
              inputId = "fauna_spp",
              label = "Cole ou digite um nome científico por linha",
              value = "",
              height = "200px",
              placeholder = "Exemplo:\nCavia intermedia\nThalasseus maximu\nLeopardus guttulus\nAmazona vinacea\nVitreorana uranoscopa\n..."
            )
          )
        ),
        hr(),
        fluidRow(column(
          width = 12,
          DT::dataTableOutput("fauna_output")
        ))
      )
    ),

    # Painel Tutoriais
    tabPanel(
      "Tutoriais",
      fluidPage(
        fluidRow(column(
          12,
          br(),
          h2(
            "Como usar - Digitando os nomes científicos (gêneros ou espécies)"
          ),
          tags$div(tags$video(
            id = "video_digitando",
            type = "video/webm",
            src = "digitando.webm",
            controls = "controls"
          ))
        )),
        fluidRow(column(
          12,
          br(),
          h2("Como usar - Colando os nomes científicos"),
          tags$div(tags$video(
            id = "video_colar",
            type = "video/webm",
            src = "colando.webm",
            controls = "controls"
          ))
        )),
        fluidRow(column(
          12,
          br(),
          h2("Como usar - Enviando uma planilha com nomes científicos"),
          tags$div(tags$video(
            id = "video_enviar",
            type = "video/webm",
            src = "enviar.webm",
            controls = "controls"
          ))
        ))
      )
    )
  )
)

# Servidor (Server)
server <- function(input, output, session) {
  ###### Nomes Colados ----------------------
  output$dados_colados <- DT::renderDataTable(
    server = FALSE,
    DT::datatable(
      {
        # Aguarda até que *nomes* não esteja vazio
        req(stringr::str_count(input$nomesColados, "[^\\s]+") >= 2)

        # Pega os nomes do campo textAreaInput e transforma em uma tibble.
        # Os nomes colados, um por linha, estarão separados por "\n".
        # Extrai-se os nomes e armazena no objeto *nomes*.
        nomes <-
          stringr::str_extract_all(
            input$nomesColados,
            stringr::regex("\\b.*(?=\\n)"),
            simplify = TRUE
          ) %>%
          purrr::map_df(tibble::as_tibble_col)

        req("value" %in% colnames(nomes))

        nomes2 <- nomes %>%
          dplyr::filter(value != "") %>%
          dplyr::mutate(
            value = stringr::str_replace(
              value,
              pattern = "(\\saff(?=\\s)|\\ss?sp\\b|\\scf(?=\\s))",
              replacement = "\\1."
            )
          ) %>%
          dplyr::mutate(
            value = stringr::str_replace(
              value,
              pattern = "(\\.\\.)",
              replacement = "."
            )
          ) %>%
          dplyr::mutate(
            value = dplyr::case_when(
              stringr::str_detect(value, "\\svar\\.?|\\ssubsp\\.?|\\sssp\\.?") ~
                stringr::word(value, 1, 4),
              stringr::str_detect(value, "\\scf\\.?\\b|\\saff\\.?\\b") ~
                stringr::word(value, 1, 3),
              stringr::str_detect(value, "[A-z]+\\s[a-z]+\\b") ~
                stringr::word(value, 1, 2),
              TRUE ~ stringr::word(value, 1)
            )
          ) %>%
          dplyr::distinct(value)

        # Pega os nomes em Flora do Brasil 2020
        nomes_BFG <- get.taxa(
          nomes2$value,
          drop = c("threat.status.cnc", "threat.status.mma")
        )

        # O campo textAreaInput está reativo, qualquer palavra inserida
        # será enviada a função `get.taxa`, que retornará uma linha vazia (`NA`)
        # para cada nome que não exista em Flora do Brasil 2020. Para se evitar
        # o retorno de uma tabela vazia, é verificada para que pelo menos uma
        # linha da tabela não seja vazia.
        req(TRUE %in% !is.na(nomes_BFG[seq_len(dim(nomes_BFG)[1]), c(2, 11)]))

        # Se há linhas sem NA, os nomes colados são comparados com os nomes
        # das listas de espécies ameaçadas do sul do Brasil.
        comparaTudo <- purrr::map2_df(
          list(dplyr::filter(nomes_BFG, !is.na(search.str))),
          list(
            ameacadasPR,
            ameacadasSC,
            ameacadasRS,
            port443,
            status_cnc
          ),
          dplyr::inner_join,
          by = c("search.str" = "nome.BFG")
        ) %>%
          dplyr::group_by(original.search) %>%
          dplyr::summarise(dplyr::across(
            .cols = c(pr, sc, rs, threat.status.mma14, threat.status.cnc),
            ~ toString(na.omit(.x))
          )) %>%
          dplyr::transmute(dplyr::across(
            .cols = dplyr::everything(),
            list(~ dplyr::na_if(., ""))
          )) %>%
          dplyr::mutate(dplyr::across(
            .cols = -original.search_1,
            ~ stringr::str_replace(., "([A-Z]{2})(,\\s[A-Z]{2})+", "\\1")
          )) %>%
          dplyr::rename_with(~ gsub("_1", "", .x, fixed = TRUE))

        # Dividiu-se em dois objetos (comparaTudo e compara2)
        # apenas para facilitar o entendimento.
        compara2 <-
          dplyr::left_join(
            nomes_BFG,
            comparaTudo,
            by = "original.search"
          ) %>%
          dplyr::distinct(original.search, .keep_all = TRUE) %>%
          dplyr::mutate(
            nome_correto = dplyr::case_when(
              notes == "was misspelled" ~ "grafia incorreta",
              notes == "replaced synonym" ~ "sinônimo",
              notes == "not found" ~ "não encontrado",
              notes == "was misspelled|replaced synonym" ~
                "grafia incorreta, sinônimo",
              TRUE ~ "correto"
            )
          ) %>%
          dplyr::mutate(
            nome_aceito = dplyr::if_else(
              nome_correto == "correto",
              "✔",
              search.str
            ),
            dplyr::across(
              .cols = original.search,
              .fns = \(sp) stringr::str_to_sentence(sp)
            )
          ) %>%
          dplyr::select(
            familia = family,
            especie = original.search,
            cncflora = threat.status.cnc,
            mma14 = threat.status.mma14,
            mma22 = threat.status.mma2022,
            pr,
            sc,
            rs,
            nome_correto,
            nome_aceito
          )

        compara2
      },
      caption = "Nomes científicos colados",
      extensions = c(
        "Buttons" # add download buttons
      ),
      options = list(
        language = list(
          url = pt_BR
        ),
        dom = "Btip",
        buttons = list(
          list(
            extend = "copy",
            exportOptions = list(modifiers = list(page = "all"))
          ),
          list(
            extend = "excel",
            exportOptions = list(modifiers = list(page = "all"))
          ),
          list(
            extend = "pdf",
            exportOptions = list(modifiers = list(page = "all"))
          )
        )
      ),
      colnames = c(
        "Família" = "familia",
        "Espécie" = "especie",
        "CNCFlora" = "cncflora",
        "MMA14" = "mma14",
        "MMA22" = "mma22",
        "PR" = "pr",
        "SC" = "sc",
        "RS" = "rs",
        "Nome Correto?" = "nome_correto",
        "Nome Aceito" = "nome_aceito"
      )
    )
  )

  ###### Nomes da Planilha  --------------
  # O arquivo recebido será filtrado pela extensão, sendo apenas permitido
  # csv, xls  ou xlsx.
  arquivo_recebido <- reactive({
    inFile <- input$planilha

    ifelse(
      is.null(inFile),
      tabela <- data.frame(),
      tabela <- {
        if (
          tolower(tools::file_ext(inFile$datapath)) == "xlsx" |
            tolower(tools::file_ext(inFile$datapath)) == "xls"
        ) {
          readxl::read_excel(
            inFile$datapath,
            col_names = TRUE,
            col_types = "text"
          )
        } else {
          data.table::fread(
            inFile$datapath,
            encoding = "Latin-1",
            fill = TRUE
          ) %>%
            janitor::clean_names()
        }
      }
    )
    tabela
  })

  # Se a planilha é enviada os botões são exibidos
  observeEvent(input$planilha, {
    output$sel_colunas <-
      renderUI({
        tagList(
          selectInput(
            inputId = "colunas",
            label = "Escolha a coluna com os nomes científicos",
            choices = names(arquivo_recebido())
          ),
          actionButton(
            "verifica",
            label = "verificar ameaçadas",
            class = "btn-primary btn-sm",
            icon = icon("clipboard-list")
          ),
          actionButton(
            "btn_observaCol",
            label = "ver coluna",
            class = "btn-secundary btn-sm",
            icon = icon("eye")
          )
        )
      })
  })

  # Se o botão "verifica" é pressionado os nomes são verificados
  observeEvent(input$verifica, {
    # Coluna selecionada pelo usuário, que contém os nomes científicos
    coluna_selecionada <- input$colunas

    # Filtra os nomes nomes científicos para remover os nomes dos autores.
    # Verifica se as abreviações sp., aff., var. e subsp. contém os pontos finais,
    # se não tem, adiciona o ponto.
    # Se o nome contém var. ou subsp. pega as quatro primeiras palavras.
    # Se o nome contém cf. ou aff. pega as três primeiras palavras.
    nomes <- arquivo_recebido() %>%
      dplyr::filter(!!coluna_selecionada != "") %>%
      dplyr::mutate(
        !!coluna_selecionada := stringr::str_replace(
          .data[[coluna_selecionada]],
          pattern = "(\\saff(?=\\s)|\\ss?sp\\b|\\scf(?=\\s))",
          replacement = "\\1."
        )
      ) %>%
      # Essa próxima linha pode ser removida
      # se a regex da linha anterior for melhor trabalhada
      dplyr::mutate(
        !!coluna_selecionada := stringr::str_replace(
          .data[[coluna_selecionada]],
          pattern = "(\\.\\.)",
          replacement = "."
        )
      ) %>%
      dplyr::mutate(
        value = dplyr::case_when(
          stringr::str_detect(
            .data[[coluna_selecionada]],
            "\\svar\\.?|\\ssubsp\\.?|\\sssp\\.?"
          ) ~
            stringr::word(
              .data[[coluna_selecionada]],
              1,
              4
            ),
          stringr::str_detect(
            .data[[coluna_selecionada]],
            "\\scf\\.?\\b|\\saff\\.?\\b"
          ) ~
            stringr::word(
              .data[[coluna_selecionada]],
              1,
              3
            ),
          stringr::str_detect(
            .data[[coluna_selecionada]],
            "[A-z]+\\s[a-z]+\\b"
          ) ~
            stringr::word(
              .data[[coluna_selecionada]],
              1,
              2
            ),
          TRUE ~ stringr::word(.data[[coluna_selecionada]], 1)
        )
      ) %>%
      dplyr::distinct(value)

    # Busca as informações do nomes em Flora do Brasil 2020
    nomes_BFG <- flora::get.taxa(
      nomes$value,
      drop = c("threat.status.cnc", "threat.status.mma")
    )

    # Verifica as espécies ameaçadas
    comparaTudo <- purrr::map2_df(
      list(dplyr::filter(nomes_BFG, !is.na(search.str))),
      list(
        ameacadasPR,
        ameacadasSC,
        ameacadasRS,
        port443,
        status_cnc
      ),
      dplyr::inner_join,
      by = c("search.str" = "nome.BFG")
    ) %>%
      dplyr::group_by(original.search) %>%
      dplyr::summarise(dplyr::across(
        .cols = c(pr, sc, rs, threat.status.mma14, threat.status.cnc),
        ~ toString(na.omit(.x))
      )) %>%
      dplyr::transmute(dplyr::across(
        .cols = dplyr::everything(),
        list(~ dplyr::na_if(., ""))
      )) %>%
      dplyr::mutate(dplyr::across(
        .cols = -original.search_1,
        ~ stringr::str_replace(., "([A-Z]{2})(,\\s[A-Z]{2})+", "\\1")
      )) %>%
      dplyr::rename_with(~ gsub("_1", "", .x, fixed = TRUE))

    # Dividiu-se em dois objetos (comparaTudo e compara2)
    # apenas para facilitar o entendimento.
    compara2 <-
      dplyr::left_join(
        nomes_BFG,
        comparaTudo,
        by = "original.search"
      ) %>%
      dplyr::distinct(original.search, .keep_all = TRUE) %>%
      dplyr::mutate(
        nome_correto = dplyr::case_when(
          notes == "was misspelled" ~ "grafia incorreta",
          notes == "replaced synonym" ~ "sinônimo",
          notes == "not found" ~ "não encontrado",
          notes == "was misspelled|replaced synonym" ~
            "grafia incorreta, sinônimo",
          TRUE ~ "correto"
        )
      ) %>%
      dplyr::mutate(
        nome_aceito = dplyr::if_else(
          nome_correto == "correto",
          "✔",
          search.str
        ),
        dplyr::across(
          .cols = original.search,
          .fns = \(sp) stringr::str_to_sentence(sp)
        )
      ) %>%
      dplyr::select(
        familia = family,
        especie = original.search,
        cncflora = threat.status.cnc,
        mma14 = threat.status.mma14,
        mma22 = threat.status.mma2022,
        pr,
        sc,
        rs,
        nome_correto,
        nome_aceito
      )

    output$dados_recebidos <- DT::renderDataTable(
      server = FALSE,
      DT::datatable(
        {
          compara2
        },
        caption = "Nomes científicos da planilha",
        extensions = c(
          "Buttons" # add download buttons
        ),
        options = list(
          language = list(
            url = pt_BR
          ),
          dom = "Btip",
          buttons = list(
            list(
              extend = "copy",
              exportOptions = list(modifiers = list(page = "all"))
            ),
            list(
              extend = "excel",
              exportOptions = list(modifiers = list(page = "all"))
            ),
            list(
              extend = "pdf",
              exportOptions = list(modifiers = list(page = "all"))
            )
          )
        ),
        colnames = c(
          "Família" = "familia",
          "Espécie" = "especie",
          "CNCFlora" = "cncflora",
          "MMA14" = "mma14",
          "MMA22" = "mma22",
          "PR" = "pr",
          "SC" = "sc",
          "RS" = "rs",
          "Nome Correto?" = "nome_correto",
          "Nome Aceito" = "nome_aceito"
        )
      )
    )
  })

  # Pesquisa por gênero -------------
  output$genero_out <- DT::renderDataTable(
    server = FALSE,
    DT::datatable(
      {
        coalesce_by_column <- function(df) {
          return(dplyr::coalesce(!!!as.list(df)))
        }

        generos <-
          stringr::str_split(
            stringr::str_to_sentence(input$genero),
            stringr::boundary("word")
          )[[1]] %>%
          paste(collapse = "|")

        # Aguarda até que *nomes* não esteja vazio
        req(input$genero)

        dplyr::bind_rows(ameacadasRS, ameacadasPR, ameacadasSC) %>%
          dplyr::select(-1) %>%
          dplyr::group_by(nome.BFG) %>%
          dplyr::summarise(
            dplyr::across(
              .cols = dplyr::everything(),
              .fns = coalesce_by_column
            )
          ) %>%
          dplyr::filter(stringr::str_detect(nome.BFG, generos)) %>%
          dplyr::rename("Espécie" = nome.BFG, PR = pr, RS = rs, SC = sc)
      },
      extensions = c(
        "Buttons" # add download buttons
      ),
      options = list(
        language = list(
          url = pt_BR
        ),
        dom = "Btip",
        buttons = list(
          list(
            extend = "copy",
            exportOptions = list(modifiers = list(page = "all"))
          ),
          list(
            extend = "excel",
            exportOptions = list(modifiers = list(page = "all"))
          ),
          list(
            extend = "pdf",
            exportOptions = list(modifiers = list(page = "all"))
          )
        )
      )
    )
  )

  observeEvent(input$btn_observaCol, {
    coluna_selecionada <- input$colunas
    coluna <- arquivo_recebido() %>%
      dplyr::select(coluna_selecionada) %>%
      head()
    output$observaColuna <- renderTable(coluna)
  })

  output$fauna_output <- DT::renderDataTable(
    server = FALSE,
    DT::datatable(
      {
        # Aguarda até que *nomes* não esteja vazio
        req(stringr::str_count(input$fauna_spp, "[^\\s]+") >= 2)

        nomes_fauna <- tibble::tibble(
          name = stringr::str_split_1(input$fauna_spp, "[\n]+"),
          kingdon = "Animalia"
        ) |>
          dplyr::filter(name != "")

        nomes_gbif <-
          rgbif::name_backbone_checklist(nomes_fauna, verbose = TRUE) %>%
          dplyr::filter(!is_alternative)

        fauna_ameacada <-
          dplyr::left_join(
            nomes_gbif,
            mma23_sc,
            by = dplyr::join_by("canonicalName" == "especie")
          ) %>%
          dplyr::select(
            grupo,
            canonicalName,
            mma14,
            mma23,
            SC,
            matchType,
            status,
            verbatim_name,
            species
          ) %>%
          dplyr::mutate(
            nome_correto = dplyr::case_when(
              matchType == "EXACT" & status == "ACCEPTED" ~ "✔",
              matchType == "EXACT" & status == "SYNONYM" ~ "sinônimo",
              matchType == "EXACT" & status == "DOUBTFUL" ~ "duvidoso!",
              matchType == "FUZZY" ~ "grafia incorreta",
            ),
            nome_aceito = species
          ) %>%
          dplyr::select(
            grupo,
            verbatim_name,
            mma14,
            mma23,
            SC,
            nome_correto,
            nome_aceito
          )

        fauna_ameacada
      },
      caption = "Fauna ameaçada",
      extensions = c(
        "Buttons" # add download buttons
      ),
      options = list(
        language = list(
          url = pt_BR
        ),
        dom = "Btip",
        buttons = list(
          list(
            extend = "copy",
            exportOptions = list(modifiers = list(page = "all"))
          ),
          list(
            extend = "excel",
            exportOptions = list(modifiers = list(page = "all"))
          ),
          list(
            extend = "pdf",
            exportOptions = list(modifiers = list(page = "all"))
          )
        )
      ),
      colnames = c(
        "Grupo" = "grupo",
        "Espécie" = "verbatim_name",
        "MMA14" = "mma14",
        "MMA22" = "mma23",
        "SC" = "SC",
        "Nome Correto?" = "nome_correto",
        "Nome Aceito" = "nome_aceito"
      )
    )
  )
}

# Executar a aplicação
shinyApp(ui = ui, server = server)
