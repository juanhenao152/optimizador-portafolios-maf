# ══════════════════════════════════════════════════════════════════════════
# BACKEND API — OPTIMIZADOR DE PORTAFOLIOS MAF
# Servidor plumber en R para datos reales de Yahoo Finance
# ══════════════════════════════════════════════════════════════════════════

# INSTALACIÓN DE PAQUETES
paquetes <- c("plumber","quantmod","dplyr","jsonlite","quadprog")
for (p in paquetes) {
  if (!require(p, character.only = TRUE, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
    library(p, character.only = TRUE)
  }
}

# ══════════════════════════════════════════════════════════════════════════
# PARÁMETROS GLOBALES
# ══════════════════════════════════════════════════════════════════════════
DIAS_ANIO  <- 252
FECHA_INI  <- Sys.Date() - (365 * 15)   # últimos 15 años
FECHA_FIN  <- Sys.Date()
N_SIM_OPT  <- 50000    # simulaciones para optimización
N_SIM_MC   <- 5000     # simulaciones Monte Carlo
N_DIAS_MC  <- 126      # 6 meses hábiles

# Acciones verificadas en Yahoo Finance
ACCIONES_DOW <- c(
  "AAPL","AMGN","AMZN","AXP","BA","CAT","CRM","CSCO","CVX","DIS",
  "GS","HD","HON","IBM","JNJ","JPM","KO","MCD","MMM","MRK",
  "MSFT","NKE","NVDA","PG","SHW","TRV","UNH","V","VZ","WMT"
)
ACCIONES_COL <- c(
  "BOGOTA.CL","CELSIA.CL","CEMARGOS.CL","CIBEST.CL","EC",
  "EXITO.CL","GEB.CL","ISA.CL","MINEROS.CL","PEI.CL",
  "PFAVAL.CL","PFCIBEST.CL","TERPEL.CL"
)
ACCIONES_CHI <- c(
  "AGUAS-A.SN","BCI.SN","BSANTANDER.SN","CAP.SN","CCU.SN",
  "CENCOSUD.SN","CHILE.SN","CMPC.SN","COLBUN.SN","COPEC.SN",
  "ECL.SN","ENELAM.SN","ENTEL.SN","FALABELLA.SN","IAM.SN",
  "ILC.SN","ITAUCL.SN","LTM.SN","PARAUCO.SN","RIPLEY.SN",
  "SMU.SN","SQM-B.SN","VAPORES.SN"
)

# Cache de precios para no re-descargar
cache_precios <- list()

# ══════════════════════════════════════════════════════════════════════════
# FUNCIONES AUXILIARES
# ══════════════════════════════════════════════════════════════════════════

# Descargar precios de Yahoo Finance con cache
descargar_precio <- function(ticker) {
  key <- paste0(ticker, "_", Sys.Date())
  if (!is.null(cache_precios[[key]])) return(cache_precios[[key]])

  tryCatch({
    datos <- getSymbols(
      ticker, src = "yahoo",
      from        = FECHA_INI,
      to          = FECHA_FIN,
      auto.assign = FALSE,
      warnings    = FALSE
    )
    precio <- data.frame(
      Fecha = as.Date(index(datos)),
      Price = as.numeric(Ad(datos))
    ) %>%
      filter(!is.na(Price)) %>%
      filter(!weekdays(Fecha) %in% c("Saturday","Sunday","sábado","domingo")) %>%
      arrange(Fecha) %>%
      mutate(Price_lag = lag(Price)) %>%
      filter(is.na(Price_lag) | Price != Price_lag) %>%
      select(-Price_lag)

    cache_precios[[key]] <<- precio
    return(precio)
  }, error = function(e) NULL)
}

# Calcular retornos logarítmicos diarios
calc_retornos <- function(precios_df) {
  precios_df %>%
    arrange(Fecha) %>%
    mutate(Retorno = log(Price / lag(Price))) %>%
    filter(!is.na(Retorno))
}

# Estadísticos de un vector de retornos
estadisticos_ret <- function(ret, rf_anual = 0.045) {
  mu     <- mean(ret, na.rm = TRUE)
  sigma  <- sd(ret,   na.rm = TRUE)
  mu_an  <- mu * DIAS_ANIO
  sig_an <- sigma * sqrt(DIAS_ANIO)
  list(
    ret_diario   = mu,
    ret_mensual  = mu * 21,
    vol_diaria   = sigma,
    vol_mensual  = sigma * sqrt(21),
    ret_anual    = mu_an,
    vol_anual    = sig_an,
    sharpe       = (mu_an - rf_anual) / sig_an
  )
}

# Correlación de Pearson entre dos vectores
pearson_cor <- function(a, b) {
  n <- min(length(a), length(b))
  if (n < 5) return(0)
  cor(a[1:n], b[1:n], use = "complete.obs")
}

# Retorno de portafolio
port_ret <- function(mus, pesos) sum(mus * pesos) * DIAS_ANIO

# Volatilidad de portafolio
port_vol <- function(cov_mat, pesos) {
  sqrt(as.numeric(t(pesos) %*% (cov_mat * DIAS_ANIO) %*% pesos))
}

# Mínima Varianza analítica con quadprog
min_varianza_analitica <- function(cov_mat) {
  n    <- nrow(cov_mat)
  Dmat <- 2 * cov_mat * DIAS_ANIO
  dvec <- rep(0, n)
  Amat <- cbind(rep(1, n), diag(n))
  bvec <- c(1, rep(0, n))
  tryCatch({
    sol <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
    w   <- sol$solution
    w / sum(w)
  }, error = function(e) rep(1/n, n))
}

# Portafolio Tangente analítico (Máximo Sharpe sin restricción de cortos)
portafolio_tangente <- function(mus, cov_mat, rf) {
  n   <- length(mus)
  exc <- mus - rf / DIAS_ANIO
  if (all(exc <= 0)) return(min_varianza_analitica(cov_mat / DIAS_ANIO))
  tryCatch({
    cov_inv <- solve(cov_mat)
    z       <- as.numeric(cov_inv %*% exc)
    z       <- pmax(z, 0)
    if (sum(z) == 0) return(rep(1/n, n))
    w       <- z / sum(z)
    names(w) <- names(mus)
    w
  }, error = function(e) {
    # Fallback: simulación con 20000 portafolios
    mejor_s <- -Inf; mejor_w <- rep(1/n, n)
    for (i in 1:20000) {
      w <- runif(n); w <- w/sum(w)
      r <- port_ret(mus, w)
      v <- port_vol(cov_mat, w)
      s <- (r - rf) / v
      if (s > mejor_s) { mejor_s <- s; mejor_w <- w }
    }
    names(mejor_w) <- names(mus)
    mejor_w
  })
}

# Aplicar restricción de peso máximo 50% por acción
aplicar_max_peso <- function(pesos, max_w = 0.50) {
  activos <- names(pesos)
  # Iterar hasta que ningún activo supere el límite
  for (iter in 1:20) {
    exceso <- pesos > max_w
    if (!any(exceso)) break
    # Recortar los que exceden y redistribuir el exceso
    sobrante <- sum(pesos[exceso] - max_w)
    pesos[exceso] <- max_w
    # Distribuir sobrante entre los que no están al límite
    libres <- !exceso
    if (sum(libres) == 0) break
    pesos[libres] <- pesos[libres] + sobrante * pesos[libres] / sum(pesos[libres])
  }
  pesos / sum(pesos)
}

# Poda iterativa: elimina acciones de menor peso hasta que el Sharpe empeore
podar_portafolio <- function(pesos_ini, mus, cov_mat, rf, metodo, max_act = 7, min_act = 3) {
  activos <- names(pesos_ini)
  # Aplicar restricción de peso máximo desde el inicio
  pesos <- aplicar_max_peso(pesos_ini, max_w = 0.50)

  ret_base    <- port_ret(mus[activos], pesos)
  vol_base    <- port_vol(cov_mat[activos, activos], pesos)
  sharpe_base <- (ret_base - rf) / vol_base

  repeat {
    # Respetar mínimo de activos
    if (length(activos) <= min_act) break

    # Candidato a eliminar: el de menor peso
    idx_min   <- which.min(pesos[activos])
    candidato <- activos[idx_min]
    restantes <- activos[-idx_min]

    mu_r  <- mus[restantes]
    cov_r <- cov_mat[restantes, restantes, drop = FALSE]

    # Re-optimizar sin el candidato
    w_new <- switch(metodo,
      sharpe = {
        w <- portafolio_tangente(mu_r, cov_r, rf)
        names(w) <- restantes; w
      },
      minvar = {
        w <- min_varianza_analitica(cov_r / DIAS_ANIO)
        if (is.null(w)) w <- rep(1/length(restantes), length(restantes))
        names(w) <- restantes; w
      },
      maxret = {
        n_r       <- length(restantes)
        idx_order <- order(mu_r, decreasing = TRUE)
        w <- rep(0, n_r)
        if (n_r >= 3) {
          w[idx_order[1]] <- 0.50
          w[idx_order[2]] <- 0.30
          w[idx_order[3:n_r]] <- rep(0.20 / (n_r - 2), n_r - 2)
        } else if (n_r == 2) {
          w[idx_order[1]] <- 0.60
          w[idx_order[2]] <- 0.40
        } else {
          w[1] <- 1
        }
        names(w) <- restantes; w
      },
      { w <- rep(1/length(restantes), length(restantes)); names(w) <- restantes; w }
    )
    w_new <- w_new / sum(w_new)
    # Aplicar restricción de peso máximo 50%
    w_new <- aplicar_max_peso(w_new, max_w = 0.50)

    ret_new    <- port_ret(mu_r, w_new)
    vol_new    <- port_vol(cov_r, w_new)
    sharpe_new <- (ret_new - rf) / vol_new

    # Degradación del Sharpe al eliminar este activo
    degradacion <- (sharpe_base - sharpe_new) / abs(sharpe_base + 1e-10)

    message("Poda: eliminando ", candidato,
            " | Sharpe: ", round(sharpe_base,4), " -> ", round(sharpe_new,4),
            " | Degradacion: ", round(degradacion*100,2), "%")

    # Parar si empeora más del 5%
    if (degradacion > 0.05) {
      message("Poda detenida: ", length(activos), " activos finales")
      break
    }

    activos     <- restantes
    pesos       <- w_new
    sharpe_base <- sharpe_new
  }

  # Respetar límite máximo
  if (length(activos) > max_act) {
    activos <- names(sort(pesos[activos], decreasing=TRUE))[1:max_act]
    pesos   <- pesos[activos]; pesos <- pesos / sum(pesos)
    names(pesos) <- activos
  }
  list(activos = activos, pesos = pesos)
}

# Optimización (mantener para compatibilidad con Excel)
optimizar_port <- function(mus, cov_mat, metodo, rf, n_sim = N_SIM_OPT) {
  n <- length(mus)
  if (metodo == "minvar") return(min_varianza_analitica(cov_mat))
  mejor_val <- -Inf; mejor_w <- rep(1/n, n)
  for (i in seq_len(n_sim)) {
    w <- runif(n); w <- w / sum(w)
    r <- port_ret(mus, w); v <- port_vol(cov_mat, w)
    val <- if (metodo == "maxret") r else (r - rf) / v
    if (val > mejor_val) { mejor_val <- val; mejor_w <- w }
  }
  mejor_w
}

# Correlación promedio mensual
corr_mensual <- function(retornos_list) {
  tickers <- names(retornos_list)
  n_obs   <- min(sapply(retornos_list, length))
  n_meses <- floor(n_obs / 21)

  meses_corr <- sapply(seq_len(n_meses), function(m) {
    ini <- (m-1)*21 + 1
    fin <- m * 21
    pares <- combn(tickers, 2, simplify = FALSE)
    corrs <- sapply(pares, function(p) {
      a <- retornos_list[[p[1]]][ini:min(fin, length(retornos_list[[p[1]]]))]
      b <- retornos_list[[p[2]]][ini:min(fin, length(retornos_list[[p[2]]]))]
      if (length(a) < 5 || length(b) < 5) return(NA)
      pearson_cor(a, b)
    })
    mean(corrs, na.rm = TRUE)
  })
  meses_corr
}

# Monte Carlo
monte_carlo <- function(mu_d, sigma_d, inversion = 10000, n_dias = N_DIAS_MC, n_sim = N_SIM_MC) {
  set.seed(42)
  trayectorias <- matrix(0, nrow = n_sim, ncol = n_dias + 1)
  trayectorias[, 1] <- inversion

  for (d in seq_len(n_dias)) {
    ret_d <- rnorm(n_sim, mean = mu_d, sd = sigma_d)
    trayectorias[, d+1] <- trayectorias[, d] * exp(ret_d)
  }

  pcts <- apply(trayectorias, 2, function(col) {
    quantile(col, probs = c(0.05, 0.25, 0.75, 0.95))
  })

  list(
    p5  = as.numeric(pcts[1,]),
    p25 = as.numeric(pcts[2,]),
    p75 = as.numeric(pcts[3,]),
    p95 = as.numeric(pcts[4,]),
    media = as.numeric(apply(trayectorias, 2, mean)),
    valor_final_medio = mean(trayectorias[, n_dias+1]),
    var_95 = inversion - quantile(trayectorias[, n_dias+1], 0.05)
  )
}

# ══════════════════════════════════════════════════════════════════════════
# API PLUMBER
# ══════════════════════════════════════════════════════════════════════════

#* @apiTitle Optimizador de Portafolios MAF
#* @apiDescription API para optimización de portafolios con datos de Yahoo Finance

#* Habilitar CORS para que el HTML pueda conectarse
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin",  "*")
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type,Authorization")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

# ──────────────────────────────────────────────────────────────────────────
#* Verificar que la API está activa
#* @get /ping
function() {
  list(
    status  = "ok",
    mensaje = "API Optimizador MAF activa",
    fecha   = as.character(Sys.Date())
  )
}

# ──────────────────────────────────────────────────────────────────────────
#* Lista de acciones disponibles por bolsa
#* @get /acciones
function() {
  list(
    dow  = ACCIONES_DOW,
    col  = ACCIONES_COL,
    chi  = ACCIONES_CHI
  )
}

# ──────────────────────────────────────────────────────────────────────────
#* Optimizar portafolio con datos de Yahoo Finance
#* @post /optimizar
#* @param tickers:[str] Lista de tickers a analizar
#* @param metodo:str Método: sharpe | minvar | maxret | equal
#* @param rf:float Tasa libre de riesgo anual (ej: 0.045)
#* @param inversion:float Monto de inversión (opcional)
#* @param modo:str optimo | manual
function(req) {
  body    <- jsonlite::fromJSON(req$postBody)
  tickers <- body$tickers
  metodo  <- body$metodo  %||% "sharpe"
  rf      <- as.numeric(body$rf %||% 0.045)
  inversion <- as.numeric(body$inversion %||% 10000)
  modo    <- body$modo %||% "manual"

  avisos <- c()

  # En modo optimo siempre usar todas las acciones disponibles
  # ignorar los tickers enviados por el usuario
  if (modo == "optimo") {
    tickers <- c(ACCIONES_DOW, ACCIONES_COL, ACCIONES_CHI)
  }

  # Validacion solo para modo manual
  if (modo != "optimo" && (is.null(tickers) || length(tickers) < 2)) {
    return(list(error = "Se necesitan al menos 2 tickers"))
  }

  # Descargar precios
  precios_list  <- list()
  retornos_list <- list()
  tickers_ok    <- c()
  bolsa_map     <- list()

  for (t in tickers) {
    p <- descargar_precio(t)
    if (!is.null(p) && nrow(p) >= 30) {
      # Verificar datos < 15 años
      anios_datos <- as.numeric(difftime(max(p$Fecha), min(p$Fecha), units="days")) / 365
      # No mostrar avisos por años de datos — se usa lo disponible

      ret <- calc_retornos(p)
      if (nrow(ret) >= 20) {
        precios_list[[t]]  <- p
        retornos_list[[t]] <- ret$Retorno
        tickers_ok         <- c(tickers_ok, t)

        # Bolsa
        if (t %in% ACCIONES_DOW)      bolsa_map[[t]] <- "DOW"
        else if (t %in% ACCIONES_COL) bolsa_map[[t]] <- "COLCAP"
        else if (t %in% ACCIONES_CHI) bolsa_map[[t]] <- "IPSA"
        else                           bolsa_map[[t]] <- "EXCEL"
      }
    }
  }

  if (length(tickers_ok) < 2) {
    return(list(error = "No se pudieron descargar suficientes datos. Intenta con otros tickers."))
  }

  # Pre-filtro Top 30 por Sharpe si modo óptimo
  if (modo == "optimo" && length(tickers_ok) > 30) {
    sharpes <- sapply(tickers_ok, function(t) {
      st <- estadisticos_ret(retornos_list[[t]], rf)
      st$sharpe
    })
    top30       <- names(sort(sharpes, decreasing = TRUE)[1:30])
    tickers_ok  <- top30
    retornos_list <- retornos_list[tickers_ok]
    # Pre-filtro silencioso
  }

  # Alinear longitudes (usar fechas en común)
  n_min <- min(sapply(retornos_list, length))
  retornos_mat <- sapply(retornos_list[tickers_ok], function(r) tail(r, n_min))
  # Asegurar que la matriz tiene nombres de columnas
  if (is.null(colnames(retornos_mat))) colnames(retornos_mat) <- tickers_ok
  retornos_mat <- as.matrix(retornos_mat)

  # Estadísticos individuales
  stats_ind <- lapply(tickers_ok, function(t) {
    s <- estadisticos_ret(retornos_list[[t]], rf)
    c(ticker = t, bolsa = bolsa_map[[t]] %||% "DOW",
      lapply(s, function(x) round(x, 6)))
  })
  stats_df <- do.call(rbind, lapply(stats_ind, as.data.frame)) %>%
    arrange(desc(as.numeric(sharpe)))

  # Matrices
  mus     <- colMeans(retornos_mat)
  cov_mat <- cov(retornos_mat)

  # Optimización con poda natural de activos
  if (metodo == "equal") {
    # Igual ponderado: todos los activos disponibles (max 7)
    act_eq   <- if (length(tickers_ok) > 7) tickers_ok[1:7] else tickers_ok
    pesos_eq <- rep(1/length(act_eq), length(act_eq))
    names(pesos_eq) <- act_eq
    resultado <- list(activos = act_eq, pesos = pesos_eq)

  } else if (metodo == "sharpe") {
    pesos_ini        <- portafolio_tangente(mus, cov_mat, rf)
    names(pesos_ini) <- tickers_ok
    resultado        <- podar_portafolio(pesos_ini, mus, cov_mat, rf, "sharpe")

  } else if (metodo == "minvar") {
    pesos_ini        <- min_varianza_analitica(cov_mat / DIAS_ANIO)
    if (is.null(pesos_ini)) pesos_ini <- rep(1/length(tickers_ok), length(tickers_ok))
    names(pesos_ini) <- tickers_ok
    resultado        <- podar_portafolio(pesos_ini, mus, cov_mat, rf, "minvar")

  } else {  # maxret
    # Concentrar en acciones de mayor retorno respetando límite 50%
    n_ok      <- length(tickers_ok)
    idx_order <- order(mus, decreasing = TRUE)
    # Top acción recibe 50%, segunda recibe 30%, resto se distribuye
    pesos_ini <- rep(0, n_ok)
    if (n_ok >= 3) {
      pesos_ini[idx_order[1]] <- 0.50
      pesos_ini[idx_order[2]] <- 0.30
      resto <- rep(0.20 / (n_ok - 2), n_ok - 2)
      pesos_ini[idx_order[3:n_ok]] <- resto
    } else if (n_ok == 2) {
      pesos_ini[idx_order[1]] <- 0.50
      pesos_ini[idx_order[2]] <- 0.50
    } else {
      pesos_ini[1] <- 1
    }
    pesos_ini        <- pesos_ini / sum(pesos_ini)
    names(pesos_ini) <- tickers_ok
    resultado        <- podar_portafolio(pesos_ini, mus, cov_mat, rf, "maxret")
  }

  tickers_port <- resultado$activos
  pesos_port   <- resultado$pesos / sum(resultado$pesos)
  # Aplicar restricción final: ninguna acción > 50%
  pesos_port   <- aplicar_max_peso(pesos_port, max_w = 0.50)
  names(pesos_port) <- tickers_port

  message("PORTAFOLIO FINAL: ", length(tickers_port), " activos: ",
          paste(tickers_port, collapse = ", "))

  # Stats portafolio
  mus_port     <- mus[tickers_port]
  cov_port     <- cov_mat[tickers_port, tickers_port]
  ret_port_an  <- port_ret(mus_port, pesos_port)
  vol_port_an  <- port_vol(cov_port, pesos_port)

  port_stats <- list(
    ticker      = "PORTAFOLIO",
    ret_diario  = round(ret_port_an / DIAS_ANIO, 6),
    ret_mensual = round(ret_port_an / 12, 6),
    vol_diaria  = round(vol_port_an / sqrt(DIAS_ANIO), 6),
    vol_mensual = round(vol_port_an / sqrt(DIAS_ANIO) * sqrt(21), 6),
    ret_anual   = round(ret_port_an, 6),
    vol_anual   = round(vol_port_an, 6),
    sharpe      = round((ret_port_an - rf) / vol_port_an, 6)
  )

  # Correlación mensual — últimos 5 años (máx 1260 dias hábiles = 60 meses)
  dias_5anios   <- min(252 * 5, n_min)
  ret_port_5y   <- lapply(retornos_list[tickers_port], function(r) tail(r, dias_5anios))
  corr_m        <- corr_mensual(ret_port_5y)
  corr_acum     <- cumsum(corr_m) / seq_along(corr_m)

  # Matriz de correlación (serializar como lista nombrada para JSON correcto)
  ret_port_mat  <- retornos_mat[, tickers_port, drop = FALSE]
  corr_mat_raw  <- cor(as.matrix(ret_port_mat))
  # Convertir a lista de listas nombradas: {ticker: {ticker: valor}}
  corr_mat <- setNames(
    lapply(tickers_port, function(ti) {
      setNames(
        lapply(tickers_port, function(tj) {
          round(as.numeric(corr_mat_raw[ti, tj]), 4)
        }),
        tickers_port
      )
    }),
    tickers_port
  )

  # Frontera eficiente
  n_front <- 3000
  front   <- lapply(seq_len(n_front), function(i) {
    w <- runif(length(tickers_port))
    w <- w / sum(w)
    r <- port_ret(mus_port, w)
    v <- port_vol(cov_port, w)
    list(ret = round(r * 100, 4), vol = round(v * 100, 4),
         sharpe = round((r - rf) / v, 4))
  })

  # Monte Carlo
  mu_d    <- ret_port_an / DIAS_ANIO
  sigma_d <- vol_port_an / sqrt(DIAS_ANIO)
  mc      <- monte_carlo(mu_d, sigma_d, inversion)

  # Serializar stats_ind como lista de registros para JSON correcto
  stats_list <- lapply(seq_len(nrow(stats_df)), function(i) {
    as.list(stats_df[i,])
  })

  # Respuesta
  list(
    avisos       = avisos,
    tickers_port = tickers_port,
    pesos        = as.list(round(pesos_port, 6)),
    bolsa_map    = bolsa_map[tickers_port],
    stats_ind    = stats_list,
    port_stats   = port_stats,
    corr_mensual = round(corr_m, 4),
    corr_acum    = round(corr_acum, 4),
    corr_mat     = corr_mat,
    frontera     = front,
    monte_carlo  = mc,
    ultimo_precio = lapply(tickers_port, function(t) {
      p <- precios_list[[t]]
      if (!is.null(p) && nrow(p) > 0) {
        list(ticker = t, precio = tail(p$Price, 1), fecha = as.character(tail(p$Fecha, 1)))
      }
    })
  )
}

# ──────────────────────────────────────────────────────────────────────────
#* Optimizar portafolio con datos de Excel (subida de archivo)
#* @post /optimizar-excel
#* @serializer json
function(req) {
  body      <- jsonlite::fromJSON(req$postBody)
  datos_raw <- body$datos   # matriz: filas=precios, columnas=tickers
  tickers   <- body$tickers
  metodo    <- body$metodo  %||% "sharpe"
  rf        <- as.numeric(body$rf %||% 0.045)
  inversion <- as.numeric(body$inversion %||% 10000)

  avisos <- c()

  if (length(tickers) > 80) {
    tickers   <- tickers[1:80]
    datos_raw <- datos_raw[, 1:80]
    avisos    <- c(avisos, "Se truncaron las acciones al máximo de 80.")
  }

  # Convertir a dataframe de precios
  precios_mat <- matrix(as.numeric(unlist(datos_raw)),
                        ncol = length(tickers))
  colnames(precios_mat) <- tickers

  # Truncar a 15 años
  max_obs <- DIAS_ANIO * 15
  if (nrow(precios_mat) > max_obs) {
    precios_mat <- tail(precios_mat, max_obs)
    avisos <- c(avisos, "Se truncaron los datos a 15 años (máximo permitido).")
  }

  # Calcular retornos
  retornos_list <- list()
  tickers_ok    <- c()
  for (t in tickers) {
    p <- precios_mat[, t]
    p <- p[!is.na(p) & p > 0]
    if (length(p) < 30) next
    ret <- diff(log(p))
    retornos_list[[t]] <- ret
    tickers_ok <- c(tickers_ok, t)
  }

  if (length(tickers_ok) < 2) {
    return(list(error = "No hay suficientes datos válidos en el archivo."))
  }

  # Verificar años disponibles por acción
  for (t in tickers_ok) {
    anios <- length(retornos_list[[t]]) / DIAS_ANIO
    if (anios < 15) {
      avisos <- c(avisos, paste0(t, ": ", round(anios, 1), " años de datos"))
    }
  }

  # Alinear longitudes
  n_min <- min(sapply(retornos_list, length))
  retornos_mat <- sapply(retornos_list, function(r) tail(r, n_min))

  mus     <- colMeans(retornos_mat)
  cov_mat <- cov(retornos_mat)

  # Optimización
  pesos <- if (metodo == "equal") {
    rep(1/length(tickers_ok), length(tickers_ok))
  } else {
    optimizar_port(mus, cov_mat, metodo, rf)
  }
  names(pesos) <- tickers_ok

  tickers_port <- tickers_ok
  pesos_port   <- pesos / sum(pesos)
  mus_port     <- mus[tickers_port]
  cov_port     <- cov_mat[tickers_port, tickers_port]

  ret_port_an  <- port_ret(mus_port, pesos_port)
  vol_port_an  <- port_vol(cov_port, pesos_port)

  port_stats <- list(
    ticker      = "PORTAFOLIO",
    ret_diario  = round(ret_port_an / DIAS_ANIO, 6),
    ret_mensual = round(ret_port_an / 12, 6),
    vol_diaria  = round(vol_port_an / sqrt(DIAS_ANIO), 6),
    vol_mensual = round(vol_port_an / sqrt(DIAS_ANIO) * sqrt(21), 6),
    ret_anual   = round(ret_port_an, 6),
    vol_anual   = round(vol_port_an, 6),
    sharpe      = round((ret_port_an - rf) / vol_port_an, 6)
  )

  stats_ind <- lapply(tickers_ok, function(t) {
    s <- estadisticos_ret(retornos_list[[t]], rf)
    c(ticker = t, bolsa = "EXCEL", lapply(s, function(x) round(x, 6)))
  })
  stats_df <- do.call(rbind, lapply(stats_ind, as.data.frame)) %>%
    arrange(desc(as.numeric(sharpe)))

  ret_port_list <- retornos_list[tickers_port]
  corr_m        <- corr_mensual(ret_port_list)
  corr_acum     <- cumsum(corr_m) / seq_along(corr_m)
  corr_mat_raw_xl <- cor(retornos_mat[, tickers_port, drop=FALSE])
  corr_mat_res  <- lapply(seq_len(nrow(corr_mat_raw_xl)), function(i) {
    row <- as.list(round(corr_mat_raw_xl[i,], 4))
    names(row) <- tickers_port
    row
  })
  names(corr_mat_res) <- tickers_port

  front <- lapply(seq_len(2000), function(i) {
    w <- runif(length(tickers_port)); w <- w/sum(w)
    r <- port_ret(mus_port, w)
    v <- port_vol(cov_port, w)
    list(ret=round(r*100,4), vol=round(v*100,4), sharpe=round((r-rf)/v,4))
  })

  mu_d    <- ret_port_an / DIAS_ANIO
  sigma_d <- vol_port_an / sqrt(DIAS_ANIO)
  mc      <- monte_carlo(mu_d, sigma_d, inversion)

  stats_list_xl <- lapply(seq_len(nrow(stats_df)), function(i) as.list(stats_df[i,]))

  list(
    avisos       = avisos,
    tickers_port = tickers_port,
    pesos        = as.list(round(pesos_port, 6)),
    bolsa_map    = setNames(rep("EXCEL", length(tickers_port)), tickers_port),
    stats_ind    = stats_list_xl,
    port_stats   = port_stats,
    corr_mensual = round(corr_m, 4),
    corr_acum    = round(corr_acum, 4),
    corr_mat     = round(corr_mat_res, 4),
    frontera     = front,
    monte_carlo  = mc
  )
}

# Operador null-coalesce
`%||%` <- function(a, b) if (!is.null(a)) a else b
