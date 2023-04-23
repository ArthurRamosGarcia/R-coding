
library(rb3)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(tidyjson)

gera.curvas.juros.di.mes = function(pasttime, by){
  
  #pasttime = numero de dias entre a data atual e a curva mais antiga
  #by = diferenÃ§a de dias entre cada curva
  
  df_yc <- yc_mget(first_date = Sys.Date() - pasttime, last_date = Sys.Date(), by = by)
  
  df_yc = df_yc%>%
    dplyr::mutate(anos = (forward_date - Sys.Date())/252)
  
  curves = ggplot(df_yc, aes(x = anos, y = r_252, group = refdate, color = factor(refdate)))+
    geom_line()+
    labs(x = 'Years', title = "Construídas usando Taxas de Contratos Futuros", x = '', y = "Annual Interest Rate", color = "Reference Date", fill = 'Curvas', caption = 'Fonte: B3')+
    theme_light()+
    scale_y_continuous(labels = scales::percent)+
    scale_x_continuous(breaks = seq(0, 55, 2))
  
  return(curves)
  
}

height <- 7.5
width <- 7.5

top_weight <- function(.data, n = 10) {
  top_10 <- .data |>
    arrange(desc(weight)) |>
    dplyr::slice_head(n = n) |>
    dplyr::select(symbol, weight)
  total_weight <- sum(top_10$weight)
  others <- tibble(
    symbol = "Others",
    weight = 1 - total_weight
  )
  
  tidyjson::bind_rows(top_10, others) |>
    mutate(cum_weight = cumsum(weight)) |>
    mutate(
      ymax = cum_weight,
      ymin = c(0, head(cum_weight, n = -1)),
      label_pos = (ymax + ymin) / 2,
      label = paste0(symbol, "\n", scales::percent(weight)),
      symbol = factor(symbol, ordered = TRUE)
    )
}

ggdonut <- function(.data, index_name) {
  ggplot(.data, aes(
    ymax = ymax, ymin = ymin,
    xmax = 4, xmin = 3,
    fill = symbol
  )) +
    geom_rect(colour = "white") +
    geom_label(
      x = 4.5, aes(y = label_pos, label = label), size = 3, colour = 'black'
    ) +
    annotate(
      "text",
      x = 0, y = 0, label = index_name, size = 16, colour = "grey",
      fontface = 2
    ) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Set3") +
    scale_color_brewer(palette = "Set3") +
    xlim(c(0, 4)) +
    theme_void() +
    theme(legend.position = "none") +
    labs(
      caption = paste("Source: B3 | Data imported at",Sys.Date())
    )
}

get.pesos.indice = function(index_name){
  
  #index_name = 'IFIX' or 'IMOB' or 'IBOV'
  
  weights = index_weights_get(index_name) |>
    top_weight() |>
    ggdonut(index_name)
  
  return(weights)
  
}

