### ----------------------------------------------------------------- ###
###   MODELOS BAYESIANOS — DESAPARICIONES CDMX                        ###
### ----------------------------------------------------------------- ###


prob <- function(x) {
  out <- min(length(x[x > 0]) / length(x), length(x[x < 0]) / length(x))
  out
}

library(R2jags)

getwd()

# =================================================================== #
# LECTURA Y PREPARACION DE DATOS
# =================================================================== #

panel <- read.csv("panel_final.csv")
panel$fecha <- as.Date(panel$fecha)

# -------------------------------------------------------------------
# Serie agregada CDMX mensual (Modelos 1 y 2)
# -------------------------------------------------------------------
agg <- aggregate(
  cbind(n_desapariciones, post_2018, post_2024, pct_seguros_cdmx, n_delitos_libertad) ~ anio + mes,
  data  = panel,
  FUN   = function(x) if (is.numeric(x)) sum(x) else x[1]
)
agg <- agg[order(agg$anio, agg$mes), ]

# Normalizar covariables continuas para mejor comportamiento del sampler
agg$pct_seguros_std    <- scale(agg$pct_seguros_cdmx)[, 1]
agg$n_delitos_lib_std  <- scale(agg$n_delitos_libertad)[, 1]

# Indice temporal (1 a n)
n_agg <- nrow(agg)
t_idx <- 1:n_agg

# -------------------------------------------------------------------
# Panel alcaldia x mes (Modelo 3)
# -------------------------------------------------------------------
panel_clean <- panel[!is.na(panel$pct_seguros_cdmx) & !is.na(panel$pob_total), ]
n_panel     <- nrow(panel_clean)
K           <- length(unique(panel_clean$CVE_MUN))   # 16 alcaldias

# Reindexar CVE_MUN a 1:16
panel_clean$alcaldia_idx <- as.integer(factor(panel_clean$CVE_MUN))

# Offset: log(poblacion / 100000) para modelar tasa por 100k
panel_clean$offset <- panel_clean$pob_total / 100000

panel_clean$pct_seguros_std   <- scale(panel_clean$pct_seguros_cdmx)[, 1]
panel_clean$n_delitos_lib_std <- scale(panel_clean$n_delitos_libertad)[, 1]


# =================================================================== #
# MODELO 1 — Poisson con dummies de cambio de gobierno
#            Serie agregada CDMX, variable dependiente: n_desapariciones
# =================================================================== #

data1 <- list(
  "n"        = n_agg,
  "y"        = as.integer(agg$n_desapariciones),
  "t"        = t_idx,
  "post2018" = as.integer(agg$post_2018),
  "post2024" = as.integer(agg$post_2024)
)

inits1 <- function() { list(beta = rep(0, 4), yf = rep(1, n_agg)) }

pars1 <- c("beta", "mu", "yf")

mod1.sim <- jags(data1, inits1, pars1,
                 model.file = "Mod1_Poisson_Breakpoint.txt",
                 n.iter     = 10000,
                 n.chains   = 2,
                 n.burnin   = 1000,
                 n.thin     = 2)

# --- Diagnosticos Modelo 1 ---
traceplot(mod1.sim)

out1.a   <- mod1.sim$BUGSoutput$sims.array
out1     <- mod1.sim$BUGSoutput$sims.list
out1.sum <- mod1.sim$BUGSoutput$summary

# Cadenas beta[1] (intercept)
z1 <- out1.a[, 1, 1]
z2 <- out1.a[, 2, 1]
par(mfrow = c(3, 2))
plot(z1, type = "l", col = "grey50", main = "Mod1: traza beta[1]")
lines(z2, col = "firebrick2")
y1 <- cumsum(z1) / (1:length(z1))
y2 <- cumsum(z2) / (1:length(z2))
plot(y1, type = "l", col = "grey50", ylim = range(y1, y2))
lines(y2, col = "firebrick2")
hist(z1, freq = FALSE, col = "grey50")
hist(z2, freq = FALSE, col = "firebrick2")
acf(z1); acf(z2)

# Resumen de betas
out1.sum.t <- out1.sum[grep("beta", rownames(out1.sum)), c(1, 3, 7)]
out1.sum.t <- cbind(out1.sum.t, apply(out1$beta, 2, prob))
dimnames(out1.sum.t)[[2]][4] <- "prob"
print("=== MODELO 1: Poisson Breakpoint ===")
print(out1.sum.t)

# DIC
out1.dic <- mod1.sim$BUGSoutput$DIC
print(c("DIC Mod1 =", out1.dic))

# Predicciones vs observado
out1.yf <- out1.sum[grep("^yf", rownames(out1.sum)), ]
par(mfrow = c(1, 1))
plot(t_idx, agg$n_desapariciones, type = "l", col = "grey50",
     xlab = "Mes (índice)", ylab = "Desapariciones",
     main = "Modelo 1: Poisson con Breakpoints")
lines(t_idx, out1.yf[, 1], lwd = 2, col = 2)
lines(t_idx, out1.yf[, 3], lty = 2, col = 2)
lines(t_idx, out1.yf[, 7], lty = 2, col = 2)
abline(v = which(agg$post_2018 == 1)[1], col = "steelblue", lty = 3)
abline(v = which(agg$post_2024 == 1)[1], col = "darkgreen",  lty = 3)
legend("topright", legend = c("Observado", "Media posterior", "IC 95%",
                               "Cambio 2018", "Cambio 2024"),
       col = c("grey50", 2, 2, "steelblue", "darkgreen"),
       lty = c(1, 1, 2, 3, 3), bty = "n")


# =================================================================== #
# MODELO 2 — Binomial Negativa con covariables
#            Controla sobredispersion + percepcion + delitos libertad
# =================================================================== #

data2 <- list(
  "n"              = n_agg,
  "y"              = as.integer(agg$n_desapariciones),
  "t"              = t_idx,
  "post2018"       = as.integer(agg$post_2018),
  "post2024"       = as.integer(agg$post_2024),
  "pct_seguros"    = agg$pct_seguros_std,
  "n_delitos_lib"  = agg$n_delitos_lib_std
)

inits2 <- function() { list(beta = rep(0, 6), r = 1, yf = rep(1, n_agg)) }

pars2 <- c("beta", "r", "mu", "yf")

mod2.sim <- jags(data2, inits2, pars2,
                 model.file = "Mod2_NegBin_Covariables.txt",
                 n.iter     = 10000,
                 n.chains   = 2,
                 n.burnin   = 1000,
                 n.thin     = 2)

# --- Diagnosticos Modelo 2 ---
out2.a   <- mod2.sim$BUGSoutput$sims.array
out2     <- mod2.sim$BUGSoutput$sims.list
out2.sum <- mod2.sim$BUGSoutput$summary

z1 <- out2.a[, 1, 1]
z2 <- out2.a[, 2, 1]
par(mfrow = c(3, 2))
plot(z1, type = "l", col = "grey50", main = "Mod2: traza beta[1]")
lines(z2, col = "firebrick2")
y1 <- cumsum(z1) / (1:length(z1))
y2 <- cumsum(z2) / (1:length(z2))
plot(y1, type = "l", col = "grey50", ylim = range(y1, y2))
lines(y2, col = "firebrick2")
hist(z1, freq = FALSE, col = "grey50")
hist(z2, freq = FALSE, col = "firebrick2")
acf(z1); acf(z2)

# Resumen de betas
out2.sum.t <- out2.sum[grep("beta", rownames(out2.sum)), c(1, 3, 7)]
out2.sum.t <- cbind(out2.sum.t, apply(out2$beta, 2, prob))
dimnames(out2.sum.t)[[2]][4] <- "prob"
print("=== MODELO 2: Binomial Negativa con Covariables ===")
print(out2.sum.t)
print(paste("r (sobredispersion) =", round(out2.sum["r", 1], 3)))

# DIC
out2.dic <- mod2.sim$BUGSoutput$DIC
print(c("DIC Mod2 =", out2.dic))

# Predicciones
out2.yf <- out2.sum[grep("^yf", rownames(out2.sum)), ]
par(mfrow = c(1, 1))
plot(t_idx, agg$n_desapariciones, type = "l", col = "grey50",
     xlab = "Mes (índice)", ylab = "Desapariciones",
     main = "Modelo 2: Binomial Negativa + Covariables")
lines(t_idx, out2.yf[, 1], lwd = 2, col = 3)
lines(t_idx, out2.yf[, 3], lty = 2, col = 3)
lines(t_idx, out2.yf[, 7], lty = 2, col = 3)
abline(v = which(agg$post_2018 == 1)[1], col = "steelblue", lty = 3)
abline(v = which(agg$post_2024 == 1)[1], col = "darkgreen",  lty = 3)


# =================================================================== #
# MODELO 3 — Poisson con efectos aleatorios por alcaldia (panel)
#            Variable dependiente: n_desapariciones, offset = pob/100k
# =================================================================== #

data3 <- list(
  "n"             = n_panel,
  "K"             = K,
  "y"             = as.integer(panel_clean$n_desapariciones),
  "post2018"      = as.integer(panel_clean$post_2018),
  "post2024"      = as.integer(panel_clean$post_2024),
  "pct_seguros"   = panel_clean$pct_seguros_std,
  "n_delitos_lib" = panel_clean$n_delitos_lib_std,
  "alcaldia"      = as.integer(panel_clean$alcaldia_idx),
  "offset"        = panel_clean$offset
)

inits3 <- function() {
  list(beta   = rep(0, 5),
       theta  = rep(0, K),
       tau.b  = 1,
       yf     = rep(1, n_panel))
}

pars3 <- c("beta", "theta", "tau.b", "sig.b", "yf")

mod3.sim <- jags(data3, inits3, pars3,
                 model.file = "Mod3_Poisson_EfectosAleatorios.txt",
                 n.iter     = 10000,
                 n.chains   = 2,
                 n.burnin   = 1000,
                 n.thin     = 2)

# --- Diagnosticos Modelo 3 ---
out3.a   <- mod3.sim$BUGSoutput$sims.array
out3     <- mod3.sim$BUGSoutput$sims.list
out3.sum <- mod3.sim$BUGSoutput$summary

z1 <- out3.a[, 1, 1]
z2 <- out3.a[, 2, 1]
par(mfrow = c(3, 2))
plot(z1, type = "l", col = "grey50", main = "Mod3: traza beta[1]")
lines(z2, col = "firebrick2")
y1 <- cumsum(z1) / (1:length(z1))
y2 <- cumsum(z2) / (1:length(z2))
plot(y1, type = "l", col = "grey50", ylim = range(y1, y2))
lines(y2, col = "firebrick2")
hist(z1, freq = FALSE, col = "grey50")
hist(z2, freq = FALSE, col = "firebrick2")
acf(z1); acf(z2)

# Resumen de betas (efectos fijos)
out3.sum.t <- out3.sum[grep("^beta", rownames(out3.sum)), c(1, 3, 7)]
out3.sum.t <- cbind(out3.sum.t, apply(out3$beta, 2, prob))
dimnames(out3.sum.t)[[2]][4] <- "prob"
print("=== MODELO 3: Poisson con Efectos Aleatorios por Alcaldía ===")
print(out3.sum.t)

# Efectos aleatorios por alcaldia
out3.theta <- out3.sum[grep("^theta", rownames(out3.sum)), c(1, 3, 7)]
k <- K
ymin <- min(out3.theta)
ymax <- max(out3.theta)
par(mfrow = c(1, 1))
plot(1:k, out3.theta[, 1], xlab = "Alcaldía (índice)", ylab = "",
     ylim = c(ymin, ymax), main = "Modelo 3: Efectos aleatorios por alcaldía")
segments(1:k, out3.theta[, 3], 1:k, out3.theta[, 7])
abline(h = 0, col = "grey70")

# DIC
out3.dic <- mod3.sim$BUGSoutput$DIC
print(c("DIC Mod3 =", out3.dic))


# =================================================================== #
# COMPARACION DE MODELOS POR DIC
# =================================================================== #

print("=== COMPARACION DIC ===")
print(data.frame(
  Modelo      = c("Mod1 Poisson Breakpoint",
                  "Mod2 NegBin Covariables",
                  "Mod3 Poisson EfAleatorios"),
  DIC         = c(out1.dic, out2.dic, out3.dic),
  stringsAsFactors = FALSE
))
