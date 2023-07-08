#' Cria um gráfico de estatísticas lexicais produzido no IRaMuTeQ.
#'
#' @param corpus.path Local onde está salvo o corpus
#' @param corpus.name O nome do corpus
#' @param save.plots Lógico.
#' @param plot.path Local para salvar o gráfico
#' @param stat Padrão é 1. Se você criou mais de uma estatística lexical no IRaMuTeQ indique o número referente. Por exemplo: stat = 3 (se foram criadas três estatísticas e você quer o gráfico da terceira)
#' @param st O número de segmentos de textos.
#'
#' @return Um gráfico
#' @export
#'
#' @examples
plot.iramuteq_stat = function(corpus.path, corpus.name, save.plots = 'TRUE',
                              plot.path = NULL, stat = 1, st){

  stat_lex <- read.csv(paste(corpus.path, corpus.name, '_stat_', stat, '/total.csv', sep = ''),
                       sep = ';', header = FALSE) |>
    rownames_to_column()

  glob <- read.delim(
    paste(corpus.path, corpus.name, '_stat_1/glob.txt', sep = ''),
    header = T,
    row.names = NULL,
    sep = ':') |>
    select(-row.names) |>
    mutate(Resumo = Resumo |> str_split_i('\\(', 1))

  stat_plot <- stat_lex |>
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(x = log(as.integer(rowname)) * 1000,
          y = log(V2) * 1000),
      color = 'blue',
      alpha = 0.8
      ) +
    ggplot2::labs(
        y = 'Log(Frequência)',
        x = 'Log(Posição)'
      ) +
    ggplot2::annotate(
        'rect',
        xmin = 7000, xmax = 8600,
        ymin = 7600, ymax = 10000,
        fill = 'lightblue', alpha = .5
      ) +
    ggplot2::geom_text(aes(7100, 9600,
                    label = paste('Textos: ', glob[1,1], sep = ''), hjust = 0, vjust = 0)) +
    ggplot2::geom_text(aes(7100, 9200,
                    label = paste('Segmentos de Textos: ', st, sep = ''), hjust = 0, vjust = 0)) +
    ggplot2::geom_text(aes(7100, 8800,
                    label = paste('Ocorrências: ', glob[2,1], sep = ''), hjust = 0, vjust = 0)) +
    ggplot2::geom_text(aes(7100, 8400,
                    label = paste('Formas: ', glob[3,1], sep = ''), hjust = 0, vjust = 0)) +
    ggplot2::geom_text(aes(7100, 8000,
                    label = paste('Hapax: ', glob[4,1], sep = ''), hjust = 0, vjust = 0)) +
      # decJ_tema +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(fill = NA)
      )

    if (save.plots == TRUE) {
      ggsave(
        paste(plot.path, 'stat_lex.png'),
        plot = stat_plot,
        width = 36,
        height = 18,
        units = 'cm',
        dpi = 600
      )
    } else {
      return(stat_plot)
    }
  }
