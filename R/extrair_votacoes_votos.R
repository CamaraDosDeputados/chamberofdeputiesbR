#' Extrai informações básicas sobre como cada parlamentar votou em uma votação
#' nominal e aberta.
#'
#' Esta função consulta a API de Dados Abertos da CD e extrai os dados básicos
#' dos votos ocorridos nas votações em órgãos da Câmara dos Deputados, por meio
#' do \emph{id} da \emph{votação}.
#'
#' @name extrair_votacoes_votos
#' @param url_base A base da URL do JSON que contém os dados das votações.
#' @param id Identificador único da votação.
#' @return Dataframe contendo os dados básicos das votações.
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' df_votacoes_votos <- extrair_votacoes_votos(id = "994689-78")

library(jsonlite)

extrair_votacoes_votos <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/votacoes",
    id) {

  # Constrói a URL final do endpoint
  url_endpoint <- paste0(url_base, "/", id, "/", "votos")

  tryCatch({

    # Faz a requisição inicial à API e extrai os dados
    doc_json <- fromJSON(url_endpoint)
    dados <- doc_json$dados

    # Verifica e substitui colunas que são NULL por NA
    dados <- lapply(dados, function(x) if (is.null(x)) NA else x)

    # Converte os dados em um data.frame
    dados <- as.data.frame(dados)

    return(dados)

  }, error = function(e) {
    # Em caso de erro, exibe a mensagem de erro
    message("Falha ao acessar a API: ", e$message)
    return(NULL)
  })
}

# Teste

df_votacoes_votos <- extrair_votacoes_votos(id = "994689-78")
