library(tidyr)
library(purrr)
library(quantmod)
library(ggplot2)
library(plotly)
library(ggdark)

vf <- function(p, r, t) p * (1 + r)**t

name_xts <- function(x, name) {
  names(x) <- name
  return(x)
}

simple_returns <- function(p) na.fill(p / lag.xts(p) - 1, 0)

log_returns <- function(p) na.fill(log(p / lag.xts(p)), 0)

sharpe_ratio <- function(r, rf) mean(r - rf) / sd(r)

info_ratio <- function(r, rb) mean(r - rb) / sd(r - rb)

s_price <- function(d, mkt, asset) mkt[d, paste(asset, ".Close", sep = "")]

port_optimize <- function(v_r, m_cov) {
  n <- length(v_r)
  m_r1 <- matrix(c(v_r, rep(1, n)), ncol = 2)
  m_uv <- solve(m_cov, m_r1)
  m_r1_s <- solve(t(m_r1) %*% m_uv)
  m_a <- m_uv %*% m_r1_s
  m_b <- t(m_a) %*% m_cov %*% m_a
  
  given_r <- function(r) m_a %*% c(r, 1)
  given_s <- function(s) {
    m <- -m_b[1, 2] / m_b[1, 1]
    p <- (m_b[2, 2] - s * s) / m_b[1, 1]
    d <- m * m - p
    if (d < 0) {
      s_min <- sqrt(m_b[2, 2] - m_b[1, 2] * m_b[1, 2] / m_b[1, 1])
      warning(paste(s, "é muito pequeno: assumindo vol mínima de:", s_min))
      return(given_r(m))
    }
    given_r(m + sqrt(d))
  }
  w2r <- function(w) t(w) %*% v_r
  w2s <- function(w) sqrt(t(w) %*% m_cov %*% w)
  list(given_r = given_r, given_s = given_s, w2r = w2r, w2s = w2s)
}


tickers <- list("PETR4.SA", "VALE3.SA", "CMIG4.SA", "USIM5.SA", "ITUB4.SA", "LREN3.SA")

# como o exercicio pede para separar os dados em dois grupos, usar dados_train = st[1:727,] e dados_test = st[728:nrow(st),], sendo o primeiro até o final de 2022.

from <- "2021-01-01" 
to <- "2023-05-31" 
st <- do.call(
  merge,
  lapply(
    tickers,
    function(ticker) {
      getSymbols(ticker,
                 from = from,
                 to = to,
                 auto.assign = FALSE,
                 verbose = FALSE,
                 warnings = FALSE
      ) |>
        na.omit()
    }
  )
)


close_st <- do.call(
  merge,
  lapply(
    tickers,
    function(x) st[, paste(x, "Close", sep = ".")]
  )
)

rets <- simple_returns(close_st)
covs <- cov(rets)
mean_r <- colMeans(rets)

mw <- port_optimize(mean_r, covs)
rates <- seq(0.0, 0.003, 0.00001)
vols1 <- sapply(rates, function(r)  mw$w2s(mw$given_r(r)))

mw2 <- port_optimize(mean_r[-2], covs[-2,-2])
vols2 <- sapply(rates, function(r)  mw2$w2s(mw2$given_r(r)))

stock <- data.frame(
  nome = names(mean_r),
  rates = vf(1.0, mean_r, 252) - 1.0,
  vols = sqrt(diag(covs) * 252)
)

(
  data.frame(
    r = vf(1.0, rates, 252) - 1.0,
    s1 = vols1 * sqrt(252),
    s2 = vols2 * sqrt(252)
  ) |>
    ggplot() +
    geom_path(aes(s1, r), color = "blue") +
    geom_path(aes(s2, r), color = "green") +
    geom_point(data = stock, aes(vols, rates, color = nome)) +
    labs(
      x = "Volatilidade",
      y = "Retorno",
      title = "Fronteira Eficiente"
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    dark_theme_gray()
) |> ggplotly()

di <- do.call(
  merge,
  lapply(
    tickers,
    function(t) getDividends(t, from = from, to = to)
  )
)

st <- merge(st, di) |> na.fill(0)

new_port <- function(inv_inicial) {
  p <- new.env()
  p$inv <- inv_inicial
  p$cash <- inv_inicial
  p$stock <- 0
  
  p$dedo_duro <- 0
  p$pm <- 0
  p$lucro <- 0
  p$sold <- 0
  return(p)
}

value_port <- function(p, d, mkt) {
  s <- mkt[d, "PETR4.SA.Close"]
  return(p$cash + p$stock * s)
}

receive_div <- function(p, d, mkt) {
  d <- mkt[d, "PETR4.SA.div"]
  p$cash <- p$cash + d * p$stock
  return(p)
}

do_ir <- function(p) {
  if (p$sold <= 20000) {
    p$sold <- 0
    return(p)
  }
  
  if (p$lucro <= 0) {
    p$sold <- 0
    return(p)
  }
  
  p$cash <- p$cash - p$lucro * 0.15 + p$dedo_duro
  p$dedo_duro <- 0
  p$lucro <- 0
  p$sold <- 0
  return(p)
}

simul_p <- function(st, iv, op_cost, fun, start = 1) {
  mkt <- coredata(st)
  n <- nrow(mkt)
  days <- index(st) |> as.POSIXlt()
  # variável para guardar o valor do nosso portifolio.
  v <- xts(
    x = matrix(rep(0, n),
               nrow = n,
               ncol = 1
    ),
    order.by = index(st)
  )
  names(v) <- c("Valor")
  
  p <- new_port(iv)
  for (d in c(1:(start - 1))) {
    v[d, 1] <- value_port(p, d, mkt)
  }
  prev_m <- days[start]$mon
  for (d in c(start:n)) {
    p <- receive_div(p, d, mkt)
    p <- fun(p, d, mkt)
    
    if (prev_m != days[d]$mon) {
      prev_m <- days[d]$mon
      p <- do_ir(p)
    }
    
    v[d, 1] <- value_port(p, d, mkt)
    p$inv <- vf(p$inv, op_cost, 1.0 / 252.0)
  }
  return(v)
}



emolumentos <- function(v) v * 0.0003
dd <- function(v) v * 0.00005
avg_price <- function(p1, q1, p2, q2) (p1 * q1 + p2 * q2) / (q1 + q2)

buy <- function(p, d, mkt, q, asset) {
  pv <- s_price(d, mkt, asset)
  v <- q * pv
  p$cash <- p$cash - v - emolumentos(v)
  p$pm <- avg_price(p$pm, p$stock, pv + emolumentos(v), q)
  p$stock <- p$stock + q
  return(p)
}

sell <- function(p, d, mkt, q, asset) {
  
  p$stock <- p$stock - q
  v <- q * s_price(d, mkt, asset)
  p$sold <- p$sold + v
  ir_retido <- 0
  if (v > 20000) {
    ir_retido <- dd(v)
  }
  p$dedo_duro <- p$dedo_duro + ir_retido
  
  p$cash <- p$cash + v - emolumentos(v) - ir_retido
  p$lucro <- p$lucro + v - emolumentos(v) - q * p$pm
  return(p)
}

strategy <- function(p, d, mkt) {
  s <- s_price(d, mkt, "PETR4.SA")
  s_1 <- s_price(d - 1, mkt, "PETR4.SA")
  v <- p$cash + p$stock * s
  
  # vendo se retoro > 20%
  # reinvisto tudo que possuo.
  
    
  if (v / p$inv - 1 > 0.20) {
    p <- sell(p, d, mkt, p$stock, "PETR4.SA")
    p$inv <- v
  }
  
  # vendo se perder 15%

  if (v / p$inv - 1 < -0.15) {
    p <- sell(p, d, mkt, p$stock, "PETR4.SA")
    p$inv <- v
  }
  
  # compro se cair
   
  if (s < s_1) {
      if (p$cash > 0.01) {
        p <- buy(p, d, mkt, p$cash * 0.10 / s, "PETR4.SA")
      }
  }
    return(p)
}


(
  simul_p(st, 1000, 0.06, strategy, 2) |>
    coredata() |>
    log_returns() |>
    accumulate(`+`) |>
    xts(order.by = index(st)) |>
    name_xts("s") |>
    merge(
      st[, "PETR4.SA.Adjusted"] |>
        log_returns() |>
        accumulate(`+`) |>
        xts(order.by = index(st)) |>
        name_xts("p")
    ) |>
    ggplot() +
    geom_line(aes(Index, s), color = "#16c032") +
    geom_line(aes(Index, p), color = "blue", linetype = "dashed") +
    labs(title='Desempenho da Estratégia', x='')+
    dark_theme_gray()
) |>
  ggplotly()

