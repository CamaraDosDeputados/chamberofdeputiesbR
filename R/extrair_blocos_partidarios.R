#' Extrai os dados básicos dos \emph{blocos partidários} de uma \emph{Legislatura}
#' específica.
#'
#' Esta função consulta a API de Dados Abertos da CD e extrai os dados básicos
#' dos \emph{blocos partidários} de uma \emph{Legislatura} específica.
#'
#' @name extrair_blocos_partidarios
#' @param url_base A URL do JSON que contém os dados básicos de blocos partidários.
#' Nas atividades parlamentares, os partidos podem se juntar em blocos partidários.
#' Quando associados, os partidos passam a trabalhar como se fossem um "grande partido",
#' com um só líder e um mesmo conjunto de vice-líderes. Os blocos só podem existir
#' até o fim da legislatura em que foram criador. Se forem passados números de
#' legislaturas com o parâmetro idLegislatura, são listados os blocos formados e
#' extintos nessas legislaturas.
#' @param idLegislatura Identificador único do bloco partidário.
#' @param itens Número de itens retornado por página.
#' @param ordem Ordem de classificação.
#' @param ordenarPor Critério de classificação.
#' @return Um dataframe contendo os dados básicos do bloco.
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' extrair_blocos(idLegislatura = 56)

library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)

extrair_blocos <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/blocos",
    idLegislatura = NULL,
    itens = 15,
    ordem = "ASC",
    ordenarPor = "id")

{

  # Cria uma lista de parâmetros da query
  query_params <- list(
    idLegislatura = idLegislatura,
    itens = itens,
    ordem = ordem,
    ordenarPor = ordenarPor)

  # Remove os parâmetros que são NULL
  query_params <- query_params[!sapply(query_params, is.null)]

  # Constrói a string da query
  query_string <- paste0("?", paste0(names(query_params), "=", query_params,
                                     collapse = "&"))

  # Constrói a URL final do endpoint
  url_endpoint <- paste0(url_base, query_string)

  tryCatch({
    # Faz a requisição inicial à API
    doc <- fromJSON(url_endpoint)

    # Verifica se há links de paginação
    if (!is.null(doc$links)) {
      links <- doc$links
      last_link <- as.character(links[links$rel == "last", ]$href)

      # Determina o número de páginas
      n_paginas <- as.numeric(substr(last_link, str_locate(last_link, "pagina=")[2] + 1, str_locate(last_link, "&itens")[1] - 1))

      # Itera sobre as páginas e junta os resultados usando map_dfr
      dados <- map_dfr(1:n_paginas, function(pagina) {
        url_endpoint_pag <- paste0(url_endpoint, "&pagina=", pagina)
        doc_pag <- fromJSON(url_endpoint_pag)
        return(doc_pag$dados)
      })
    } else {
      # Se não houver paginação, retorna os dados diretamente
      dados <- doc$dados
    }

    # Retorna os dados como um tibble
    return(tibble::as_tibble(dados))
  }, error = function(e) {
    # Em caso de erro, exibe a mensagem de erro
    message("Falha ao acessar a API: ", e$message)
    return(NULL)
  })
}

# Testes
df_blocos <- extrair_blocos(idLegislatura = 56)
