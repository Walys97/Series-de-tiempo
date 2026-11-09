# =====================================================================
# ARIMA-X: Tasa de politica monetaria (BanRep) en funcion del IPI (DANE)
# Seminario Series de Tiempo - Grupo 3
# Version script del notebook arimax_ipi.ipynb (mismo codigo).
# Ejecutar desde la carpeta notebooks/ (rutas relativas ../datos, ../figuras).
# =====================================================================


# 0. Librerías y configuración ------------------------------------------

library(tidyverse)
library(forecast)
library(tseries)
library(lmtest)
library(zoo)

ruta_ipi  <- "../datos/ipi_mensual.csv"
ruta_tasa <- "../datos/tasa_politica_monetaria.csv"   # SUAMECA

# TRUE = busqueda rapida (stepwise). Para los resultados FINALES usa FALSE.
rapido <- TRUE

h <- 12   # meses de validacion

# Dummy de pandemia: 1 entre estas fechas, 0 en el resto
covid_ini <- as.Date("2020-03-01")
covid_fin <- as.Date("2020-10-01")

dir.create("../figuras",   showWarnings = FALSE)
dir.create("../resultados", showWarnings = FALSE)

# --- Estilo de los graficos ---
col_main <- "#1f4e79"   # azul
col_alt  <- "#c0392b"   # rojo

theme_set(
  theme_minimal(base_size = 13) +
    theme(plot.title       = element_text(face = "bold"),
          strip.text       = element_text(face = "bold", hjust = 0),
          panel.grid.minor = element_blank(),
          legend.position  = "bottom",
          legend.title     = element_blank())
)

# Muestra el grafico en pantalla Y lo guarda en ../figuras/<nombre>.pdf
# Acepta un ggplot o una funcion (para graficos base: ccf, checkresiduals, ggtsdisplay)
guardar <- function(p, nombre, w = 9, h = 5.5) {
  dibujar <- function() {
    r <- if (is.function(p)) p() else p
    if (inherits(r, c("gg", "ggplot", "patchwork", "gtable"))) print(r)
    invisible(r)
  }
  dibujar()
  dev_pdf <- if (capabilities("cairo")) cairo_pdf else pdf
  dev_pdf(file.path("../figuras", paste0(nombre, ".pdf")), width = w, height = h)
  dibujar()
  dev.off()
  invisible(NULL)
}

guardar_tabla <- function(df, nombre) {
  write_csv(df, file.path("../resultados", paste0(nombre, ".csv")))
  df
}


# 1. Datos --------------------------------------------------------------

ipi <- read_csv(ruta_ipi, show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha)) %>%
  select(fecha, ipi)

# Tasa de politica (SUAMECA): separador ";", coma decimal, fechas "AAAA/MM/DD", diaria
tasa_raw <- read_delim(ruta_tasa, delim = ";",
                       locale = locale(decimal_mark = ",", encoding = "UTF-8"),
                       col_names = c("fecha", "tasa"), skip = 1,
                       show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha, format = "%Y/%m/%d"),
         tasa  = as.numeric(tasa)) %>%
  arrange(fecha)

stopifnot(!anyNA(tasa_raw$fecha), !anyNA(tasa_raw$tasa))

# Diaria -> mensual: se rellenan dias faltantes y se promedia por mes
dias <- tibble(fecha = seq(min(tasa_raw$fecha), max(ipi$fecha) + 31, by = "day")) %>%
  left_join(tasa_raw, by = "fecha") %>%
  mutate(tasa = zoo::na.locf(tasa, na.rm = FALSE))

tasa_m <- dias %>%
  mutate(fecha = as.Date(format(fecha, "%Y-%m-01"))) %>%
  group_by(fecha) %>%
  summarise(tasa = mean(tasa, na.rm = TRUE), .groups = "drop")

datos <- ipi %>%
  inner_join(tasa_m, by = "fecha") %>%
  mutate(ly = log(ipi))

stopifnot(nrow(datos) == nrow(ipi), !anyNA(datos))

inicio <- c(as.integer(format(min(datos$fecha), "%Y")),
            as.integer(format(min(datos$fecha), "%m")))

# log(IPI) ORIGINAL: solo para el analisis descriptivo (estacionalidad)
ly_ts <- ts(datos$ly, start = inicio, frequency = 12)

# IPI DESESTACIONALIZADO (STL sobre el log), escala 100*log:
# un cambio de 1 unidad equivale aprox. a 1 % de variacion del IPI
ly_sa <- seasadj(stl(ly_ts, s.window = "periodic", robust = TRUE))
datos <- datos %>% mutate(ipi_sa = 100 * as.numeric(ly_sa))

# --- Variables del modelo ---
y <- ts(datos$tasa,   start = inicio, frequency = 12)   # DEPENDIENTE: tasa de politica (%)
x <- ts(datos$ipi_sa, start = inicio, frequency = 12)   # EXOGENA: IPI desestacionalizado

cat("y (tasa, %):", round(range(y), 2),
    " | x (IPI desest., 100*log):", round(range(x), 1), "\n")

# Revisar que sí estemos en la fecha seleccionada por el equipo: 2014 a 2026

cat("Datos desde", format(min(datos$fecha)), "hasta", format(max(datos$fecha)), "-", nrow(datos), "meses\n")
head(datos, 3)
tail(datos, 3)


# 2. Análisis descriptivo -----------------------------------------------

esperadas <- seq(min(datos$fecha), max(datos$fecha), by = "month")
cat("Periodo:", format(min(datos$fecha)), "a", format(max(datos$fecha)),
    "| n =", nrow(datos), "\n")
cat("NAs:", sum(is.na(datos)),
    "| meses faltantes:", length(setdiff(esperadas, datos$fecha)), "\n\n")

resumen <- datos %>%
  pivot_longer(c(ipi, tasa), names_to = "variable", values_to = "valor") %>%
  group_by(variable) %>%
  summarise(media = mean(valor), desv = sd(valor),
            minimo = min(valor), maximo = max(valor),
            fecha_min = fecha[which.min(valor)],
            fecha_max = fecha[which.max(valor)], .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(guardar_tabla(resumen, "00_resumen_descriptivo"))

ipi_feb <- datos$ipi[datos$fecha == "2020-02-01"]
ipi_abr <- datos$ipi[datos$fecha == "2020-04-01"]
cat("\nCaida IPI abr-2020 vs feb-2020:", round(100 * (ipi_abr / ipi_feb - 1), 1), "%\n")

# Grafico 1: series originales, con tendencia suavizada
p1 <- datos %>%
  select(fecha, `IPI (indice original)` = ipi, `Tasa BanRep (%)` = tasa) %>%
  pivot_longer(-fecha) %>%
  ggplot(aes(fecha, value)) +
  annotate("rect", xmin = covid_ini, xmax = covid_fin,
           ymin = -Inf, ymax = Inf, fill = col_alt, alpha = 0.12) +
  geom_line(colour = col_main, linewidth = 0.8) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              colour = col_alt, linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "IPI y tasa de politica monetaria (series originales)",
       subtitle = "Linea punteada: tendencia suavizada | Sombreado: periodo de pandemia",
       x = NULL, y = NULL)
guardar(p1, "01_series_originales", h = 6.5)

# Grafico 2: estacionalidad del IPI (una linea por anio)
p2 <- ggseasonplot(ly_ts, year.labels = FALSE) +
  scale_colour_viridis_d(option = "C") +
  labs(title = "Estacionalidad del log(IPI)", x = NULL, y = "log(IPI)")
guardar(p2, "02_estacionalidad")


# 3. Estacionariedad ----------------------------------------------------
# - ADF: H_0 = raíz unitaria. p < 0.05 → estacionaria
# - KPSS: H_0 = estacionaria. p < 0.05 → NO estacionaria

d_y <- ndiffs(y, test = "kpss")
d_x <- ndiffs(x, test = "kpss")
cat("\nDiferencias regulares sugeridas (KPSS):  tasa:", d_y, "  IPI desest.:", d_x, "\n")
cat("Diferencias estacionales sugeridas (OCSB) tasa:", nsdiffs(y), "\n\n")

# ADF:  H0 = raiz unitaria (no estacionaria). p < 0.05 -> estacionaria
# KPSS: H0 = estacionaria.                    p < 0.05 -> NO estacionaria
prueba <- function(s, nombre) {
  tibble(serie  = nombre,
         ADF_p  = suppressWarnings(adf.test(s)$p.value),
         KPSS_p = suppressWarnings(kpss.test(s)$p.value))
}
tabla_est <- bind_rows(prueba(y,       "Tasa"),
                       prueba(diff(y), "diff Tasa"),
                       prueba(x,       "IPI desest."),
                       prueba(diff(x), "diff IPI desest.")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
print(guardar_tabla(tabla_est, "03_pruebas_estacionariedad"))

# ACF y PACF de la tasa ya diferenciada
y_dif <- if (d_y > 0) diff(y, differences = d_y) else y
guardar(function() ggtsdisplay(y_dif, lag.max = 36,
          main = paste0("Tasa diferenciada (d = ", d_y, ")")),
        "03_acf_pacf")


# 4. Regresoras exógenas: IPI desestacionalizado y sus rezagos ----------

rez <- function(v, k) stats::lag(v, -k)

# CCF pre-blanqueada: se ajusta un ARIMA al IPI (x) y se aplica el mismo filtro
# a la tasa (y). Barras en rezagos NEGATIVOS = el IPI se adelanta a la tasa.
mx <- auto.arima(x, d = 1, seasonal = FALSE)
rx <- residuals(mx)
ry <- residuals(Arima(y, model = mx))

guardar(function() ccf(rx, ry, lag.max = 18,
                       main = "CCF pre-blanqueada: IPI desest. (x) vs tasa (y)",
                       xlab = "Rezago (meses)", ylab = "Correlacion cruzada",
                       lwd = 2, col = col_main),
        "04_ccf")


# 5. Entrenamiento / validación (últimos h meses) -----------------------

covid <- ts(as.numeric(datos$fecha >= covid_ini & datos$fecha <= covid_fin),
            start = inicio, frequency = 12)
cat("\nMeses con dummy COVID = 1:", sum(covid), "\n")

X <- cbind(ipi    = x,
           ipi_L3 = rez(x, 3),
           ipi_L6 = rez(x, 6),
           covid  = covid)
X  <- window(X, start = start(y), end = end(y))
ok <- complete.cases(X)                      # los rezagos dejan NA al inicio
y2 <- ts(y[ok],   start = time(y)[which(ok)[1]], frequency = 12)
X2 <- ts(X[ok, ], start = time(y)[which(ok)[1]], frequency = 12)
n2 <- length(y2)

y_tr <- window(y2, end   = time(y2)[n2 - h]); y_te <- window(y2, start = time(y2)[n2 - h + 1])
X_tr <- window(X2, end   = time(y2)[n2 - h]); X_te <- window(X2, start = time(y2)[n2 - h + 1])

cols1 <- c("ipi", "covid")                      # ARIMA-X: IPI + dummy COVID
cols2 <- c("ipi", "ipi_L3", "ipi_L6", "covid")  # ARIMA-X con rezagos + dummy COVID


# 6. Ajuste: ARIMA vs ARIMA-X -------------------------------------------

m0 <- auto.arima(y_tr, d = 1, seasonal = FALSE, stepwise = rapido)
m1 <- auto.arima(y_tr, xreg = X_tr[, cols1, drop = FALSE],
                 d = 1, seasonal = FALSE, stepwise = rapido)
m2 <- auto.arima(y_tr, xreg = X_tr[, cols2, drop = FALSE],
                 d = 1, seasonal = FALSE, stepwise = rapido)

print(summary(m0)); print(summary(m1)); print(summary(m2))

tabla_coef <- function(m) {
  out <- as.data.frame(unclass(coeftest(m)))
  out <- tibble::rownames_to_column(out, "termino")
  names(out) <- c("termino", "estimado", "error_est", "estadistico", "p_valor")
  out %>% mutate(across(where(is.numeric), ~ round(.x, 4)),
                 significativo = abs(estadistico) > 2)   # criterio |t| > 2
}
print(guardar_tabla(tabla_coef(m1), "coef_m1_arimax"))
print(guardar_tabla(tabla_coef(m2), "coef_m2_arimax_rezagos"))

ic <- tibble(modelo = c("ARIMA", "ARIMA-X (IPI + COVID)", "ARIMA-X (IPI con rezagos + COVID)"),
             especificacion = c(forecast:::arima.string(m0, padding = FALSE),
                                forecast:::arima.string(m1, padding = FALSE),
                                forecast:::arima.string(m2, padding = FALSE)),
             AIC = c(AIC(m0), AIC(m1), AIC(m2)),
             BIC = c(BIC(m0), BIC(m1), BIC(m2))) %>%
  mutate(across(c(AIC, BIC), ~ round(.x, 2)))
print(guardar_tabla(ic, "ic_modelos"))

# Si una variable no es significativa en coef_m1_arimax.csv, quitala de cols1/cols2
# (seccion 5) y vuelve a correr desde ahi.


# 7. Diagnóstico de residuos --------------------------------------------

guardar(function() checkresiduals(m1, lag = 24), "05_residuos", h = 7)

ord <- arimaorder(m1)
print(Box.test(residuals(m1), lag = 24, type = "Ljung-Box",
               fitdf = sum(ord[c("p", "q")])))     # fitdf = solo parametros ARMA
print(shapiro.test(residuals(m1)))


# 8. Valores atípicos (opcional) ----------------------------------------

# Solo si los residuos de la celda 7 muestran atipicos que la dummy COVID no resuelve.
# install.packages("tsoutliers")
# library(tsoutliers)
# at <- tso(y_tr, xreg = X_tr[, cols1, drop = FALSE],
#           types = c("AO", "TC", "LS"), cval = 3.5,
#           tsmethod = "arima",
#           args.tsmethod = list(order = arimaorder(m1)[c("p", "d", "q")]))
# print(at); plot(at)


# 9. Pronóstico en la muestra de validación -----------------------------

# Pronostico "ex post": se usan los valores observados del IPI en la validacion.
f0 <- forecast(m0, h = h)
f1 <- forecast(m1, h = h, xreg = X_te[, cols1, drop = FALSE])
f2 <- forecast(m2, h = h, xreg = X_te[, cols2, drop = FALSE])

metr <- function(f) accuracy(f, y_te)["Test set", c("RMSE", "MAE", "MAPE")]
resultados <- rbind(ARIMA = metr(f0), `ARIMA-X` = metr(f1), `ARIMA-X rezagos` = metr(f2))
resultados <- as.data.frame(round(resultados, 3)) %>% tibble::rownames_to_column("modelo")
print(guardar_tabla(resultados, "metricas_validacion"))

# Pronostico ARIMA-X con banda de incertidumbre
p6 <- autoplot(f1, include = 60) +
  autolayer(y_te, series = "Observado", linewidth = 1) +
  scale_colour_manual(values = c(Observado = col_alt)) +
  labs(title = "Pronostico ARIMA-X de la tasa de politica", x = NULL, y = "Tasa (%)")
guardar(p6, "06_pronostico_arimax")

# Validacion: tasa observada vs pronosticos de los tres modelos
val <- tibble(fecha = tail(datos$fecha, h),
              Observado         = as.numeric(y_te),
              ARIMA             = as.numeric(f0$mean),
              `ARIMA-X`         = as.numeric(f1$mean),
              `ARIMA-X rezagos` = as.numeric(f2$mean)) %>%
  pivot_longer(-fecha, names_to = "serie", values_to = "tasa") %>%
  mutate(serie = factor(serie, levels = c("Observado", "ARIMA", "ARIMA-X", "ARIMA-X rezagos")))

p7 <- ggplot(val, aes(fecha, tasa, colour = serie, linetype = serie, linewidth = serie)) +
  geom_line() +
  geom_point(data = filter(val, serie == "Observado"), size = 2) +
  scale_colour_manual(values = c(Observado = "black", ARIMA = "#7f8c8d",
                                 `ARIMA-X` = col_main, `ARIMA-X rezagos` = col_alt)) +
  scale_linetype_manual(values = c(Observado = "solid", ARIMA = "dashed",
                                   `ARIMA-X` = "solid", `ARIMA-X rezagos` = "solid")) +
  scale_linewidth_manual(values = c(Observado = 1.1, ARIMA = 0.8,
                                    `ARIMA-X` = 0.8, `ARIMA-X rezagos` = 0.8)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(title = "Validacion: tasa observada vs pronosticos", x = NULL, y = "Tasa (%)")
guardar(p7, "07_validacion")

cat("\nListo. Graficos en graficos/ y tablas en resultados/\n")


# 10. Descargar todos los gráficos en un solo PDF -----------------------

pdf("../figuras/graficos_arimax.pdf", width = 10, height = 6.5)

print(p1)                                                        # 1. Series originales
print(p2)                                                        # 2. Estacionalidad del log(IPI)
ggtsdisplay(y_dif, lag.max = 36,
            main = paste0("Tasa diferenciada (d = ", d_y, ")"))  # 3. ACF y PACF
ccf(rx, ry, lag.max = 18,
    main = "CCF pre-blanqueada: IPI desest. (x) vs tasa (y)",
    xlab = "Rezago (meses)", ylab = "Correlacion cruzada",
    lwd = 2, col = col_main)                                     # 4. CCF
checkresiduals(m1, lag = 24)                                     # 5. Residuos ARIMA-X
print(p6)                                                        # 6. Pronostico ARIMA-X
print(p7)                                                        # 7. Validacion

invisible(dev.off())
cat("\nListo. Graficos en ../figuras/ y tablas en ../resultados/\n")
