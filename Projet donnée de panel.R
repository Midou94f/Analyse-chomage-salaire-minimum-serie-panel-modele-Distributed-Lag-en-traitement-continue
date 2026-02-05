# ==============================================================================
# EFFET DU SALAIRE MINIMUM SUR LE CHÔMAGE
# Méthodologie : Schmidheiny & Siegloch (2023)
# Distributed Lag avec traitement continu
# ==============================================================================

# ==============================================================================
# PARTIE A : DONNÉES
# ==============================================================================

# --- A.1) PACKAGES ------------------------------------------------------------

library(eurostat)
library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(stringr)
library(patchwork)

# --- A.2) IMPORT DES DONNÉES EUROSTAT -----------------------------------------

cat("\n================================================================\n")
cat("                    IMPORT DES DONNÉES                          \n")
cat("================================================================\n\n")

# Salaire minimum (Eurostat)
mw_raw <- get_eurostat("earn_mw_cur", time_format = "date")

mw <- mw_raw %>%
  mutate(
    TIME_PERIOD = as.Date(TIME_PERIOD),
    year  = as.integer(format(TIME_PERIOD, "%Y")),
    month = as.integer(format(TIME_PERIOD, "%m"))
  ) %>%
  filter(
    currency == "EUR",
    freq == "S",
    month == 1
  ) %>%
  group_by(geo, year) %>%
  summarise(minw = mean(values, na.rm = TRUE), .groups = "drop")

cat("Salaire minimum : ", nrow(mw), " observations\n", sep = "")

# Taux de chômage (Eurostat)
u_raw <- get_eurostat("une_rt_a", time_format = "num")

u <- u_raw %>%
  filter(
    freq == "A",
    unit == "PC_ACT",
    sex  == "T",
    age  == "Y15-74"
  ) %>%
  transmute(
    geo,
    year = as.integer(TIME_PERIOD),
    unemp = values
  )

cat("Chômage : ", nrow(u), " observations\n", sep = "")

# --- A.3) CONSTRUCTION DU PANEL BRUT ------------------------------------------

cat("\n================================================================\n")
cat("                 CONSTRUCTION DU PANEL BRUT                     \n")
cat("================================================================\n\n")

dat <- u %>%
  left_join(mw, by = c("geo", "year")) %>%
  filter(!geo %in% c("EA20", "EU27_2020")) %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(
    has_minw = !is.na(minw) & minw > 0,
    lminw = ifelse(has_minw, log(minw), NA_real_)
  ) %>%
  ungroup()

cat("Panel brut :\n")
cat("  - Pays : ", n_distinct(dat$geo), "\n", sep = "")
cat("  - Années : ", min(dat$year), " - ", max(dat$year), "\n", sep = "")
cat("  - Observations : ", nrow(dat), "\n", sep = "")

# ==============================================================================
# PARTIE B : DÉFINITION DE LA FENÊTRE ET VÉRIFICATIONS
# ==============================================================================

# --- B.1) DÉFINITION DE LA FENÊTRE D'EFFET ET DATA REQUIREMENTS (Remark 4) ----

cat("\n================================================================\n")
cat("       DÉFINITION FENÊTRE & DATA REQUIREMENTS (Remark 4)        \n")
cat("================================================================\n\n")

# Paramètres de la fenêtre d'effet
n_leads <- 2  # leads (k = -2, -1) pour tester parallel trends
n_lags <- 5   # lags (k = 0, 1, 2, 3, 4, 5) pour effets dynamiques

# Période d'analyse souhaitée
analyse_start <- 2009
analyse_end <- 2024

# Vérification des data requirements
t_min <- min(dat$year)
t_max <- max(dat$year)

# Pour estimer l(lminw, -2:5), on a besoin de :
# - lminw_{t+2} pour les leads → besoin de données jusqu'à analyse_end + 2
# - lminw_{t-5} pour les lags → besoin de données depuis analyse_start - 5
treat_start_needed <- analyse_start - n_lags
treat_end_needed <- analyse_end + n_leads

cat("Fenêtre d'effet : [", -n_leads, ", +", n_lags, "]\n", sep = "")
cat("  → Leads k ∈ {-2, -1} : test des parallel trends\n")
cat("  → Lags  k ∈ {0, 1, 2, 3, 4, 5} : effets dynamiques\n\n")

cat("Période d'analyse souhaitée : ", analyse_start, " - ", analyse_end, "\n", sep = "")
cat("\nData requirements :\n")
cat("  - Besoin d'observer lminw de ", treat_start_needed, " à ", treat_end_needed, "\n", sep = "")
cat("  - Données disponibles : ", t_min, " à ", t_max, "\n", sep = "")

# Vérification et ajustement si nécessaire
data_req_ok <- TRUE

if (t_min > treat_start_needed) {
  cat("  ⚠️  Données insuffisantes au début\n")
  data_req_ok <- FALSE
}

if (t_max < treat_end_needed) {
  cat("  ⚠️  Données insuffisantes à la fin\n")
  # Ajuster la fin d'analyse
  analyse_end <- t_max - n_leads
  cat("     → Fin d'analyse ajustée à ", analyse_end, "\n", sep = "")
  data_req_ok <- FALSE
}

if (data_req_ok) {
  cat("  ✅ Data requirements satisfaits\n")
}

# --- B.2) ÉCHANTILLON D'ESTIMATION --------------------------------------------

cat("\n================================================================\n")
cat("                 ÉCHANTILLON D'ESTIMATION                       \n")
cat("================================================================\n\n")

dat_analysis <- dat %>%
  filter(year >= analyse_start, year <= analyse_end) %>%
  filter(!is.na(lminw))

cat("Échantillon d'estimation (", analyse_start, "-", analyse_end, ") :\n", sep = "")
cat("  - Pays : ", n_distinct(dat_analysis$geo), "\n", sep = "")
cat("  - Observations : ", nrow(dat_analysis), "\n\n", sep = "")

cat("Liste des pays :\n")
print(sort(unique(dat_analysis$geo)))

# --- B.3) STATISTIQUES DESCRIPTIVES -------------------------------------------

cat("\n\n================================================================\n")
cat("                STATISTIQUES DESCRIPTIVES                       \n")
cat("================================================================\n\n")

cat("Taux de chômage (%) :\n")
cat("  Min    : ", round(min(dat_analysis$unemp, na.rm = TRUE), 2), "\n", sep = "")
cat("  Médiane: ", round(median(dat_analysis$unemp, na.rm = TRUE), 2), "\n", sep = "")
cat("  Moyenne: ", round(mean(dat_analysis$unemp, na.rm = TRUE), 2), "\n", sep = "")
cat("  Max    : ", round(max(dat_analysis$unemp, na.rm = TRUE), 2), "\n", sep = "")
cat("  E-T    : ", round(sd(dat_analysis$unemp, na.rm = TRUE), 2), "\n", sep = "")

cat("\nSalaire minimum (EUR) :\n")
cat("  Min    : ", round(min(dat_analysis$minw, na.rm = TRUE), 0), "\n", sep = "")
cat("  Médiane: ", round(median(dat_analysis$minw, na.rm = TRUE), 0), "\n", sep = "")
cat("  Moyenne: ", round(mean(dat_analysis$minw, na.rm = TRUE), 0), "\n", sep = "")
cat("  Max    : ", round(max(dat_analysis$minw, na.rm = TRUE), 0), "\n", sep = "")

cat("\nLog(salaire minimum) :\n")
cat("  Moyenne: ", round(mean(dat_analysis$lminw, na.rm = TRUE), 3), "\n", sep = "")
cat("  E-T    : ", round(sd(dat_analysis$lminw, na.rm = TRUE), 3), "\n", sep = "")

# --- B.4) VARIATION WITHIN-COUNTRY (Remark 5 - Identification) ----------------

# Variation within-country (importante pour identification)
var_within <- dat_analysis %>%
  group_by(geo) %>%
  summarise(
    sd_lminw = sd(lminw, na.rm = TRUE),
    range_lminw = max(lminw, na.rm = TRUE) - min(lminw, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nVariation within-country de log(minw) :\n")
cat("  - E-T moyen   : ", round(mean(var_within$sd_lminw, na.rm = TRUE), 3), "\n", sep = "")
cat("  - Range moyen : ", round(mean(var_within$range_lminw, na.rm = TRUE), 3), "\n", sep = "")

if (mean(var_within$sd_lminw, na.rm = TRUE) > 0.1) {
  cat("  ✅ Variation suffisante pour identification\n")
} else {
  cat("  ⚠️  Faible variation within - identification potentiellement fragile\n")
}

# Graphiques descriptifs
par(mfrow = c(1, 2))
hist(dat_analysis$unemp, main = "Distribution du chômage", 
     xlab = "Taux (%)", col = "lightblue", breaks = 20)
hist(dat_analysis$lminw, main = "Distribution log(salaire min)", 
     xlab = "log(EUR)", col = "lightgreen", breaks = 20)
par(mfrow = c(1, 1))

# ==============================================================================
# PARTIE C : ESTIMATION PRINCIPALE
# ==============================================================================

# --- C.1) MODÈLE DISTRIBUTED LAG ----------------------------------------------

cat("\n================================================================\n")
cat("      MODÈLE DISTRIBUTED LAG (leads + lags)                     \n")
cat("================================================================\n\n")

cat("Modèle : unemp_it = Σ γ_k · lminw_{i,t-k} + μ_i + θ_t + ε_it\n")
cat("         pour k ∈ {-2, -1, 0, 1, 2, 3, 4, 5}\n\n")

cat("INTERPRÉTATION des γ_k :\n")
cat("  • Leads (γ₋₂, γ₋₁) : effet du salaire min FUTUR sur chômage actuel\n")
cat("    → Devraient être ≈ 0 (test parallel trends)\n")
cat("  • Lag 0 (γ₀)       : effet contemporain\n")
cat("  • Lags (γ₁...γ₅)   : effets retardés (dynamique)\n\n")

# Estimation du modèle DL avec leads et lags
dl_model <- feols(
  unemp ~ l(lminw, -n_leads:n_lags) | geo + year,
  data = dat_analysis,
  cluster = ~geo,
  panel.id = ~geo + year
)

cat("Résultats :\n")
etable(dl_model)

# --- C.2) EXTRACTION DES COEFFICIENTS γ_k -------------------------------------

cat("\n================================================================\n")
cat("              COEFFICIENTS INCRÉMENTAUX γ_k                     \n")
cat("================================================================\n\n")

# Extraire coefficients et matrice variance-covariance
coefs <- coef(dl_model)
V <- vcov(dl_model)
se_vec <- sqrt(diag(V))

# Identifier les termes de lminw
lminw_terms <- names(coefs)[str_detect(names(coefs), "l\\(lminw")]

# Construire le dataframe des γ_k
df_gamma <- data.frame(
  term = lminw_terms,
  gamma = as.numeric(coefs[lminw_terms]),
  se_gamma = as.numeric(se_vec[lminw_terms])
) %>%
  mutate(
    # Pattern corrigé : chercher le nombre avant la parenthèse fermante
    k = as.integer(str_extract(term, "-?\\d+(?=\\))")),
    ci_low = gamma - 1.96 * se_gamma,
    ci_high = gamma + 1.96 * se_gamma,
    type = ifelse(k < 0, "Lead (pré-tendance)", "Lag (effet)")
  ) %>%
  arrange(k)

print(df_gamma %>% select(k, gamma, se_gamma, ci_low, ci_high, type))

# --- C.3) TEST PARALLEL TRENDS ------------------------------------------------

cat("\n================================================================\n")
cat("         TEST PARALLEL TRENDS (leads ≈ 0 ?)                     \n")
cat("================================================================\n\n")

cat("H₀ : γ₋₂ = γ₋₁ = 0 (pas d'effet du salaire min futur)\n")
cat("    → Si on ne rejette pas H₀, parallel trends plausibles\n\n")

# Test individuel sur chaque lead
cat("Tests individuels :\n")
leads_df <- df_gamma %>% filter(k < 0)

for (i in 1:nrow(leads_df)) {
  t_stat <- leads_df$gamma[i] / leads_df$se_gamma[i]
  p_val <- 2 * pnorm(-abs(t_stat))
  sig <- ifelse(p_val < 0.01, "***", ifelse(p_val < 0.05, "**", 
                                            ifelse(p_val < 0.1, "*", "")))
  cat(sprintf("  k = %d : γ = %+.4f (SE = %.4f), t = %.2f, p = %.4f %s\n",
              leads_df$k[i], leads_df$gamma[i], leads_df$se_gamma[i], 
              t_stat, p_val, sig))
}

# Test joint de Wald sur les leads (méthode manuelle robuste)
cat("\nTest joint de Wald :\n")

b <- coef(dl_model)
V <- vcov(dl_model)
idx_leads <- grep("::-", names(b))

if (length(idx_leads) > 0) {
  b_leads <- b[idx_leads]
  V_leads <- V[idx_leads, idx_leads, drop = FALSE]
  
  # Wald = b' * V^{-1} * b ~ Chi²(df)
  wald_stat <- as.numeric(t(b_leads) %*% solve(V_leads) %*% b_leads)
  df_wald <- length(idx_leads)
  p_wald <- 1 - pchisq(wald_stat, df = df_wald)
  
  cat(sprintf("  Wald = %.3f (df = %d), p = %.4f\n", wald_stat, df_wald, p_wald))
  
  if (p_wald > 0.05) {
    cat("  ✅ On ne rejette pas H₀ : parallel trends VALIDÉES\n")
    parallel_trends_ok <- TRUE
  } else {
    cat("  ⚠️  On rejette H₀ : PROBLÈME de parallel trends !\n")
    cat("     → Les résultats doivent être interprétés avec prudence\n")
    parallel_trends_ok <- FALSE
  }
} else {
  cat("  Aucun lead trouvé dans le modèle\n")
  parallel_trends_ok <- NA
}

# --- C.4) TRANSFORMATION EN EFFETS CUMULÉS βₖ (Équation 12) -------------------

cat("\n================================================================\n")
cat("    EFFETS CUMULÉS βₖ (= Event Study généralisée)               \n")
cat("================================================================\n\n")

# --- Rappel des γₖ ---
cat("Coefficients incrémentaux γₖ :\n")
print(df_gamma %>% select(k, gamma, se_gamma))

# --- Matrice de transformation L telle que β = L × γ ---
# 
# Ordre des γ : γ₋₂, γ₋₁, γ₀, γ₁, γ₂, γ₃, γ₄, γ₅
# Ordre des β : β₋₂, β₋₁, β₀, β₁, β₂, β₃, β₄, β₅
#
# Selon l'équation (12) :
#   β₋₂ = -(γ₋₁)           → on cumule de k+1=-1 à -1
#   β₋₁ = 0                → référence (pas besoin de l'estimer)
#   β₀  = γ₀
#   β₁  = γ₀ + γ₁
#   β₂  = γ₀ + γ₁ + γ₂
#   β₃  = γ₀ + γ₁ + γ₂ + γ₃
#   β₄  = γ₀ + γ₁ + γ₂ + γ₃ + γ₄
#   β₅  = γ₀ + γ₁ + γ₂ + γ₃ + γ₄ + γ₅

# Créer la matrice de transformation (sans β₋₁ qui est normalisé à 0)
# Lignes = β, Colonnes = γ (dans l'ordre γ₋₂, γ₋₁, γ₀, γ₁, γ₂, γ₃, γ₄, γ₅)

L <- matrix(0, nrow = 7, ncol = 8)
rownames(L) <- c("beta_-2", "beta_0", "beta_1", "beta_2", "beta_3", "beta_4", "beta_5")
colnames(L) <- c("gamma_-2", "gamma_-1", "gamma_0", "gamma_1", "gamma_2", "gamma_3", "gamma_4", "gamma_5")

# β₋₂ = -γ₋₁
L[1, 2] <- -1

# β₀ = γ₀
L[2, 3] <- 1

# β₁ = γ₀ + γ₁
L[3, 3:4] <- 1

# β₂ = γ₀ + γ₁ + γ₂
L[4, 3:5] <- 1

# β₃ = γ₀ + γ₁ + γ₂ + γ₃
L[5, 3:6] <- 1

# β₄ = γ₀ + γ₁ + γ₂ + γ₃ + γ₄
L[6, 3:7] <- 1

# β₅ = γ₀ + γ₁ + γ₂ + γ₃ + γ₄ + γ₅
L[7, 3:8] <- 1

cat("Matrice de transformation L :\n")
print(L)

# --- Calcul des βₖ ---
gamma_vec <- df_gamma$gamma
beta_vec <- as.vector(L %*% gamma_vec)

# --- Calcul des erreurs-types par Delta Method ---
# Var(β) = L × Var(γ) × L'
V_gamma <- as.matrix(V[lminw_terms, lminw_terms])
V_beta <- L %*% V_gamma %*% t(L)
se_beta <- sqrt(diag(V_beta))

# --- Construire le dataframe des βₖ ---
df_beta <- data.frame(
  k = c(-2, 0, 1, 2, 3, 4, 5),
  beta = beta_vec,
  se_beta = se_beta
) %>%
  mutate(
    ci_low = beta - 1.96 * se_beta,
    ci_high = beta + 1.96 * se_beta,
    t_stat = beta / se_beta,
    p_value = 2 * pnorm(-abs(t_stat)),
    signif = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    )
  )

# Ajouter β₋₁ = 0 (référence)
df_beta <- bind_rows(
  df_beta %>% filter(k == -2),
  data.frame(k = -1, beta = 0, se_beta = NA, ci_low = NA, ci_high = NA, 
             t_stat = NA, p_value = NA, signif = "(ref)"),
  df_beta %>% filter(k >= 0)
) %>%
  arrange(k)

cat("\n")
cat("Effets cumulés βₖ (équivalent Event Study) :\n")
cat("─────────────────────────────────────────────\n")
print(df_beta %>% select(k, beta, se_beta, ci_low, ci_high, signif))

# ==============================================================================
# PARTIE D : TESTS DE ROBUSTESSE
# ==============================================================================

# --- D.1) COMPARAISON FE vs FD (Section 3.4.4) --------------------------------

cat("\n================================================================\n")
cat("     COMPARAISON FIXED EFFECTS vs FIRST DIFFERENCE              \n")
cat("              (Section 3.4.4 de l'article)                      \n")
cat("================================================================\n\n")

cat("Principe : Si FE et FD donnent des résultats similaires,\n")
cat("           alors les effets ont convergé (fenêtre suffisante).\n\n")

# Modèle Fixed Effects (seulement les lags, sans leads pour comparaison propre)
dl_fe <- feols(
  unemp ~ l(lminw, 0:n_lags) | geo + year,
  data = dat_analysis,
  cluster = ~geo,
  panel.id = ~geo + year
)

# Pour First Difference, on crée manuellement les variables
dat_fd <- dat_analysis %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(
    d_unemp = unemp - lag(unemp),
    d_lminw_0 = lminw - lag(lminw),
    d_lminw_1 = lag(lminw, 1) - lag(lminw, 2),
    d_lminw_2 = lag(lminw, 2) - lag(lminw, 3),
    d_lminw_3 = lag(lminw, 3) - lag(lminw, 4),
    d_lminw_4 = lag(lminw, 4) - lag(lminw, 5),
    d_lminw_5 = lag(lminw, 5) - lag(lminw, 6)
  ) %>%
  ungroup()

# Modèle First Difference
dl_fd <- feols(
  d_unemp ~ d_lminw_0 + d_lminw_1 + d_lminw_2 + d_lminw_3 + d_lminw_4 + d_lminw_5 | year,
  data = dat_fd,
  cluster = ~geo
)

cat("Comparaison des coefficients γₖ :\n\n")
etable(dl_fe, dl_fd, headers = c("Fixed Effects", "First Difference"))

# --- Extraction des coefficients pour comparaison ---

# FE
coef_fe <- coef(dl_fe)
se_fe <- sqrt(diag(vcov(dl_fe)))
terms_fe <- names(coef_fe)[grepl("lminw", names(coef_fe))]

df_fe <- data.frame(
  k = 0:n_lags,
  gamma = as.numeric(coef_fe[terms_fe]),
  se = as.numeric(se_fe[terms_fe]),
  model = "Fixed Effects"
)
df_fe$ci_low <- df_fe$gamma - 1.96 * df_fe$se
df_fe$ci_high <- df_fe$gamma + 1.96 * df_fe$se

# FD
coef_fd <- coef(dl_fd)
se_fd <- sqrt(diag(vcov(dl_fd)))
terms_fd <- names(coef_fd)[grepl("d_lminw", names(coef_fd))]

df_fd <- data.frame(
  k = 0:n_lags,
  gamma = as.numeric(coef_fd[terms_fd]),
  se = as.numeric(se_fd[terms_fd]),
  model = "First Difference"
)
df_fd$ci_low <- df_fd$gamma - 1.96 * df_fd$se
df_fd$ci_high <- df_fd$gamma + 1.96 * df_fd$se

# Combiner
df_fe_fd <- bind_rows(df_fe, df_fd)

# --- Diagnostic ---

cat("\n--- Diagnostic de convergence ---\n\n")

df_compare <- data.frame(
  k = 0:n_lags,
  gamma_fe = df_fe$gamma,
  gamma_fd = df_fd$gamma
) %>%
  mutate(
    diff = abs(gamma_fe - gamma_fd),
    diff_pct = ifelse(gamma_fe != 0, abs(diff / gamma_fe) * 100, NA)
  )

cat("Écarts FE vs FD par horizon :\n")
print(df_compare %>% mutate(across(where(is.numeric), ~round(., 3))))

max_diff <- max(df_compare$diff, na.rm = TRUE)
cat("\nÉcart absolu maximum : ", round(max_diff, 3), " pp\n", sep = "")

if (max_diff < 1) {
  cat("✅ Estimations très similaires → effets convergés\n")
  fe_fd_ok <- TRUE
} else if (max_diff < 2) {
  cat("⚠️  Estimations assez proches → convergence probable\n")
  fe_fd_ok <- TRUE
} else {
  cat("❌ Estimations divergentes → fenêtre peut-être trop courte\n")
  fe_fd_ok <- FALSE
}

# --- Graphique FE vs FD ---

p_fe_fd <- ggplot(df_fe_fd, aes(x = k, y = gamma, color = model, shape = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                width = 0.2, position = position_dodge(0.3)) +
  geom_line(linewidth = 0.8, position = position_dodge(0.3)) +
  geom_point(size = 2.5, position = position_dodge(0.3)) +
  scale_color_manual(values = c("Fixed Effects" = "steelblue", "First Difference" = "darkorange")) +
  scale_shape_manual(values = c("Fixed Effects" = 16, "First Difference" = 17)) +
  scale_x_continuous(breaks = 0:n_lags) +
  labs(
    x = "Horizon k (années)",
    y = expression(gamma[k] ~ "(pp)"),
    title = "Comparaison Fixed Effects vs First Difference",
    subtitle = "Si divergence → effets pas encore convergés (fenêtre trop courte)",
    color = "", shape = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p_fe_fd)


# --- D.2) ROBUSTESSE : DIFFÉRENTES FENÊTRES (Remark 3) ------------------------

cat("\n================================================================\n")
cat("      ROBUSTESSE : DIFFÉRENTES FENÊTRES (Remark 3)              \n")
cat("================================================================\n\n")

cat("Principe : Les effets cumulés doivent se stabiliser\n")
cat("           si la fenêtre est suffisamment longue.\n\n")

# Tester plusieurs fenêtres de lags (on va plus loin que 5)
windows <- c(3, 5, 7, 9)
results_windows <- list()

for (max_lag in windows) {
  
  # Formule dynamique
  fml <- as.formula(paste0("unemp ~ l(lminw, 0:", max_lag, ") | geo + year"))
  
  tryCatch({
    # Estimation
    mod <- feols(fml, data = dat_analysis, cluster = ~geo, panel.id = ~geo + year)
    
    # Extraire coefficients
    b_mod <- coef(mod)
    V_mod <- vcov(mod)
    terms_mod <- names(b_mod)[str_detect(names(b_mod), "l\\(lminw")]
    
    # Effet cumulé (somme des γ)
    beta_cum <- sum(b_mod[terms_mod])
    
    # Erreur-type par delta method
    w <- rep(1, length(terms_mod))
    se_cum <- sqrt(as.numeric(t(w) %*% V_mod[terms_mod, terms_mod] %*% w))
    
    # t-stat et significativité
    t_stat <- beta_cum / se_cum
    signif <- ifelse(abs(t_stat) > 2.58, "***", 
                     ifelse(abs(t_stat) > 1.96, "**", 
                            ifelse(abs(t_stat) > 1.64, "*", "")))
    
    results_windows[[as.character(max_lag)]] <- data.frame(
      max_lag = max_lag,
      beta_cum = beta_cum,
      se = se_cum,
      ci_low = beta_cum - 1.96 * se_cum,
      ci_high = beta_cum + 1.96 * se_cum,
      n_obs = nobs(mod),
      signif = signif
    )
    
    cat(sprintf("Lags 0:%d → β_cum = %+7.3f (SE = %.3f) %s  [N = %d]\n", 
                max_lag, beta_cum, se_cum, signif, nobs(mod)))
    
  }, error = function(e) {
    cat(sprintf("Lags 0:%d → ERREUR : %s\n", max_lag, e$message))
  })
}

df_windows <- bind_rows(results_windows)

# --- Diagnostic de stabilité ---

cat("\n--- Diagnostic de stabilité ---\n\n")

if (nrow(df_windows) > 1) {
  
  # Tableau récapitulatif
  cat("Tableau récapitulatif :\n")
  print(df_windows %>% 
          mutate(across(where(is.numeric), ~round(., 3))) %>%
          select(max_lag, beta_cum, se, ci_low, ci_high, signif, n_obs))
  
  # Calculs de stabilité
  range_beta <- max(df_windows$beta_cum) - min(df_windows$beta_cum)
  mean_beta <- mean(df_windows$beta_cum)
  sd_beta <- sd(df_windows$beta_cum)
  cv <- abs(sd_beta / mean_beta) * 100
  
  cat("\nMesures de stabilité :\n")
  cat("  - Étendue (max - min) : ", round(range_beta, 3), " pp\n", sep = "")
  cat("  - Moyenne des β_cum   : ", round(mean_beta, 3), " pp\n", sep = "")
  cat("  - Écart-type          : ", round(sd_beta, 3), " pp\n", sep = "")
  cat("  - Coef. de variation  : ", round(cv, 1), " %\n\n", sep = "")
  
  # Diagnostic
  if (range_beta < 3 & cv < 25) {
    cat("✅ Effets relativement stables → fenêtre probablement adéquate\n")
    window_ok <- TRUE
  } else if (range_beta < 5) {
    cat("⚠️  Effets modérément stables → à interpréter avec prudence\n")
    window_ok <- TRUE
  } else {
    cat("❌ Effets instables → les effets n'ont peut-être pas convergé\n")
    window_ok <- FALSE
  }
  
  # Vérifier la tendance
  cat("\nTendance des effets cumulés :\n")
  if (df_windows$beta_cum[nrow(df_windows)] > df_windows$beta_cum[1]) {
    cat("  → Les effets augmentent avec la fenêtre (effet qui se construit)\n")
  } else {
    cat("  → Les effets diminuent avec la fenêtre (possible sur-estimation initiale)\n")
  }
}

# --- Graphique de robustesse ---

p_windows <- ggplot(df_windows, aes(x = max_lag, y = beta_cum)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, color = "steelblue") +
  geom_text(aes(label = signif), vjust = -1.5, size = 4) +
  scale_x_continuous(breaks = windows) +
  labs(
    x = "Nombre maximum de lags",
    y = "Effet cumulé sur le chômage (pp)",
    title = "Robustesse : Convergence des effets selon la fenêtre",
    subtitle = "Les effets devraient se stabiliser si la fenêtre est suffisante",
    caption = "Source : Eurostat. Méthode : Schmidheiny & Siegloch (2023), Remark 3"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

print(p_windows)

# Sauvegarder
ggsave("robustesse_fenetres.png", p_windows, width = 8, height = 5, dpi = 150)
cat("\n→ Graphique sauvegardé : robustesse_fenetres.png\n")

# --- Résumé robustesse ---

cat("\n================================================================\n")
cat("              RÉSUMÉ TESTS DE ROBUSTESSE                        \n")
cat("================================================================\n\n")

cat("1. Comparaison FE vs FD : ")
if (exists("fe_fd_ok") && fe_fd_ok) {
  cat("✅ Convergence OK\n")
} else {
  cat("⚠️  À interpréter avec prudence\n")
}

cat("2. Stabilité fenêtres   : ")
if (exists("window_ok") && window_ok) {
  cat("✅ Effets stables\n")
} else {
  cat("⚠️  À interpréter avec prudence\n")
}

cat("3. Parallel trends      : ")
if (exists("parallel_trends_ok") && !is.na(parallel_trends_ok)) {
  if (parallel_trends_ok) {
    cat("✅ Validées\n")
  } else {
    cat("❌ Rejetées (p < 0.05)\n")
  }
} else {
  cat("Non testé\n")
}

# --- D.3) ANALYSE DE SENSIBILITÉ (Rambachan & Roth, 2022) ---------------------

library(HonestDiD)

cat("\n================================================================\n")
cat("    ANALYSE DE SENSIBILITÉ (Rambachan & Roth, 2022)             \n")
cat("================================================================\n\n")

# Nos coefficients βₖ (sans la référence)
betahat <- c(
  df_beta$beta[df_beta$k == -2],  # β₋₂
  df_beta$beta[df_beta$k == 0],   # β₀
  df_beta$beta[df_beta$k == 1],   # β₁
  df_beta$beta[df_beta$k == 2],   # β₂
  df_beta$beta[df_beta$k == 3],   # β₃
  df_beta$beta[df_beta$k == 4],   # β₄
  df_beta$beta[df_beta$k == 5]    # β₅
)
names(betahat) <- c("t-2", "t0", "t1", "t2", "t3", "t4", "t5")

cat("Coefficients βₖ (inputs HonestDiD) :\n")
print(round(betahat, 3))

# Matrice variance-covariance
sigma <- as.matrix(V_beta)

# Paramètres
numPrePeriods <- 1   # β₋₂ seulement
numPostPeriods <- 6  # β₀ à β₅

# l_vec pour sélectionner β₅
l_vec <- c(0, 0, 0, 0, 0, 1)

cat("\nParamètres HonestDiD :\n")
cat("  numPrePeriods  : ", numPrePeriods, "\n")
cat("  numPostPeriods : ", numPostPeriods, "\n")
cat("  l_vec          : ", l_vec, " (sélection de β₅)\n\n")

# Test Smoothness Restriction
cat("--- Smoothness Restriction ---\n\n")
tryCatch({
  
  results_smooth <- createSensitivityResults(
    betahat = betahat,
    sigma = sigma,
    numPrePeriods = numPrePeriods,
    numPostPeriods = numPostPeriods,
    Mvec = c(0, 0.5, 1, 1.5, 2),
    l_vec = l_vec
  )
  
  cat("Résultats :\n")
  print(results_smooth)
  
}, error = function(e) {
  cat("Erreur :", e$message, "\n")
})

# Test Relative Magnitudes
cat("\n--- Relative Magnitudes ---\n\n")
tryCatch({
  
  results_rm <- createSensitivityResults_relativeMagnitudes(
    betahat = betahat,
    sigma = sigma,
    numPrePeriods = numPrePeriods,
    numPostPeriods = numPostPeriods,
    Mbarvec = c(0, 0.5, 1, 1.5, 2),
    l_vec = l_vec
  )
  
  cat("Résultats :\n")
  print(results_rm)
  
}, error = function(e) {
  cat("Erreur :", e$message, "\n")
})

# ==============================================================================
# PARTIE E : GRAPHIQUES ET RÉSULTATS
# ==============================================================================

# --- E.1) GRAPHIQUE EVENT STUDY -----------------------------------------------

ggplot(df_beta %>% filter(!is.na(se_beta) | k == -1), 
       aes(x = k, y = beta)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "red", linewidth = 1) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, fill = "steelblue", na.rm = TRUE) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_point(data = df_beta %>% filter(k == -1), 
             aes(x = k, y = beta), color = "red", size = 4, shape = 18) +
  scale_x_continuous(breaks = -2:5) +
  annotate("text", x = -1.5, y = max(df_beta$ci_high, na.rm = TRUE) + 1, 
           label = "Pré-traitement", color = "darkorange", size = 3.5) +
  annotate("text", x = 2.5, y = max(df_beta$ci_high, na.rm = TRUE) + 1, 
           label = "Post-traitement", color = "darkgreen", size = 3.5) +
  labs(
    x = "Années relatives (k)",
    y = expression(beta[k] ~ "(points de pourcentage)"),
    title = "Event Study généralisée : Effet cumulé du salaire minimum sur le chômage",
    subtitle = "β₋₁ = 0 (référence). Intervalle de confiance à 95%.",
    caption = "Source : Eurostat. Méthode : Schmidheiny & Siegloch (2023)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40")
  )

# --- E.2) GRAPHIQUE SENSIBILITÉ -----------------------------------------------

df_sensitivity <- data.frame(
  Mbar = c(0, 0.5, 1, 1.5, 2),
  lb = c(6.88, -9.96, -31.2, -52.6, -55.0),
  ub = c(17.3, 35.8, 55.0, 55.0, 55.0)
)

df_sensitivity$signif <- ifelse(df_sensitivity$lb > 0, "Significatif", "Non significatif")

ggplot(df_sensitivity, aes(x = Mbar)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.25, fill = "steelblue") +
  geom_line(aes(y = lb), color = "steelblue", linewidth = 1) +
  geom_line(aes(y = ub), color = "steelblue", linewidth = 1) +
  geom_point(aes(y = lb, color = signif), size = 3) +
  geom_point(aes(y = ub), color = "steelblue", size = 3) +
  geom_hline(yintercept = 12.13, linetype = "dotted", color = "darkgreen", linewidth = 0.8) +
  annotate("text", x = 1.8, y = 15, label = "β₅ = 12.1", color = "darkgreen", size = 3.5) +
  geom_vline(xintercept = 0.25, linetype = "dotted", color = "orange", linewidth = 0.8) +
  annotate("text", x = 0.35, y = -50, label = "Breakdown\n≈ 0.25", color = "orange", size = 3) +
  scale_color_manual(values = c("Significatif" = "darkgreen", "Non significatif" = "steelblue")) +
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2)) +
  scale_y_continuous(limits = c(-60, 60), breaks = seq(-60, 60, 20)) +
  labs(
    x = expression(bar(M) ~ "(ratio violation post/pré)"),
    y = "Intervalle de confiance (pp)",
    title = "Analyse de sensibilité (Rambachan & Roth, 2022)",
    subtitle = "Méthode Relative Magnitudes - Effet cumulé à 5 ans (β₅)",
    color = "Borne inférieure"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "bottom"
  )

ggsave("sensitivity_rambachan_roth.png", width = 8, height = 6, dpi = 150)




# ==============================================================================
# PARTIE F : INTERPRÉTATION ÉCONOMIQUE
# ==============================================================================

cat("\n================================================================\n")
cat("              INTERPRÉTATION ÉCONOMIQUE                         \n")
cat("================================================================\n\n")

# Statistiques de base
sd_lminw <- sd(dat_analysis$lminw, na.rm = TRUE)
mean_unemp <- mean(dat_analysis$unemp, na.rm = TRUE)
beta_5 <- df_beta$beta[df_beta$k == 5]
se_5 <- df_beta$se_beta[df_beta$k == 5]

cat("Statistiques clés :\n")
cat("  - É-T de log(salaire min) : ", round(sd_lminw, 3), "\n", sep = "")
cat("  - Chômage moyen           : ", round(mean_unemp, 2), " %\n\n", sep = "")

cat("Effet cumulé à 5 ans (β₅) : ", round(beta_5, 2), " pp (SE = ", round(se_5, 2), ")\n\n", sep = "")

cat("Traduction économique :\n")
cat("  → Hausse de 1% du salaire min   : ", sprintf("%+.3f", beta_5 * 0.01), " pp de chômage\n", sep = "")
cat("  → Hausse de 10% du salaire min  : ", sprintf("%+.2f", beta_5 * 0.10), " pp de chômage\n", sep = "")
cat("  → Hausse de 1 écart-type        : ", sprintf("%+.2f", beta_5 * sd_lminw), " pp de chômage\n\n", sep = "")

# Élasticité
elasticity <- beta_5 / mean_unemp
cat("Élasticité implicite (ε = β₅ / ū) : ", round(elasticity, 3), "\n\n", sep = "")

cat("Comparaison avec la littérature :\n")
cat("  • Card & Krueger (1994)    : ε ≈ 0 à +0.1 (pas d'effet)\n")
cat("  • Dube et al. (2010)       : ε ≈ -0.1 à 0\n")
cat("  • Neumark & Wascher (2007) : ε ≈ -0.1 à -0.3\n")
cat("  • Notre estimation         : ε ≈ ", round(elasticity, 2), "\n\n", sep = "")

if (elasticity > 0.5) {
  cat("⚠️  Notre élasticité est PLUS ÉLEVÉE que la littérature.\n")
  cat("   Cela renforce les doutes sur l'interprétation causale\n")
  cat("   (parallel trends violées, endogénéité probable).\n")
}



# --- Forcer le statut parallel trends basé sur β₋₂ ---
# Si β₋₂ est significatif, les parallel trends sont violées

beta_m2 <- df_beta$beta[df_beta$k == -2]
se_m2 <- df_beta$se_beta[df_beta$k == -2]
t_stat_m2 <- beta_m2 / se_m2
p_val_m2 <- 2 * pnorm(-abs(t_stat_m2))

cat("Vérification parallel trends via β₋₂ :\n")
cat(sprintf("  β₋₂ = %.2f, t = %.2f, p = %.4f\n", beta_m2, t_stat_m2, p_val_m2))

if (p_val_m2 < 0.10) {
  parallel_trends_ok <- FALSE
  cat("  → Parallel trends REJETÉES (p < 0.10)\n\n")
} else {
  parallel_trends_ok <- TRUE
  cat("  → Parallel trends validées\n\n")
}
cat("\n================================================================\n")
cat("                    RÉCAPITULATIF FINAL                         \n")
cat("================================================================\n\n")

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║                    DONNÉES                                   ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  Pays           : %-3d                                        ║\n", n_distinct(dat_analysis$geo)))
cat(sprintf("║  Période        : %d - %d                                  ║\n", analyse_start, analyse_end))
cat(sprintf("║  Observations   : %-3d                                        ║\n", nrow(dat_analysis)))
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║                    VALIDITÉ                                  ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")

# Parallel trends - version robuste
if (exists("parallel_trends_ok") && !is.na(parallel_trends_ok) && parallel_trends_ok == TRUE) {
  pt_status <- "✅ Validées"
  pt_ok <- TRUE
} else if (exists("parallel_trends_ok") && !is.na(parallel_trends_ok) && parallel_trends_ok == FALSE) {
  pt_status <- "❌ Rejetées"
  pt_ok <- FALSE
} else {
  pt_status <- "⚠️  Non testé"
  pt_ok <- FALSE
}
cat(sprintf("║  Parallel trends     : %-20s              ║\n", pt_status))

# FE vs FD - version robuste
if (exists("fe_fd_ok") && !is.na(fe_fd_ok) && fe_fd_ok == TRUE) {
  fefd_status <- "✅ Convergence OK"
} else {
  fefd_status <- "❌ Divergents"
}
cat(sprintf("║  Convergence FE/FD   : %-20s              ║\n", fefd_status))

# Fenêtres - version robuste
if (exists("window_ok") && !is.na(window_ok) && window_ok == TRUE) {
  win_status <- "✅ Stables"
} else {
  win_status <- "❌ Instables"
}
cat(sprintf("║  Stabilité fenêtres  : %-20s              ║\n", win_status))

cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║                    RÉSULTATS                                 ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")

for (i in 1:nrow(df_beta)) {
  if (!is.na(df_beta$se_beta[i])) {
    cat(sprintf("║  β_%d = %+7.2f (SE = %5.2f) %-4s                            ║\n",
                df_beta$k[i], df_beta$beta[i], df_beta$se_beta[i], df_beta$signif[i]))
  } else {
    cat(sprintf("║  β_%d = %+7.2f (référence)                                  ║\n",
                df_beta$k[i], df_beta$beta[i]))
  }
}

cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║                    CONCLUSION                                ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")

# Conclusion - version robuste
if (!pt_ok) {
  cat("║  ⚠️  INTERPRÉTATION CAUSALE NON JUSTIFIÉE                   ║\n")
  cat("║  Les résultats doivent être interprétés comme des          ║\n")
  cat("║  CORRÉLATIONS CONDITIONNELLES, pas des effets causaux.     ║\n")
} else {
  cat("║  ✅ Les diagnostics soutiennent une interprétation causale  ║\n")
}

cat("╚══════════════════════════════════════════════════════════════╝\n")



# ==============================================================================
# PARTIE H : GRAPHIQUES FINAUX
# ==============================================================================

library(patchwork)

# Panel A : Effets incrémentaux γₖ
p_gamma <- ggplot(df_gamma, aes(x = k, y = gamma, color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "red", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = type), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Lead (pré-tendance)" = "darkorange", "Lag (effet)" = "steelblue")) +
  scale_fill_manual(values = c("Lead (pré-tendance)" = "darkorange", "Lag (effet)" = "steelblue")) +
  scale_x_continuous(breaks = -n_leads:n_lags) +
  labs(x = "k", y = expression(gamma[k]), title = "A) Effets incrémentaux (DL)", color = "", fill = "") +
  theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 10))

# Panel B : Effets cumulés βₖ
p_beta <- ggplot(df_beta, aes(x = k, y = beta)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "red", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, fill = "steelblue", na.rm = TRUE) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_point(data = df_beta %>% filter(k == -1), color = "red", size = 4, shape = 18) +
  scale_x_continuous(breaks = -n_leads:n_lags) +
  labs(x = "k", y = expression(beta[k]), title = "B) Effets cumulés (Event Study)") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", size = 10))

# Panel C : FE vs FD
p_fe_fd_small <- ggplot(df_fe_fd, aes(x = k, y = gamma, color = model, shape = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, position = position_dodge(0.3)) +
  geom_point(size = 2.5, position = position_dodge(0.3)) +
  scale_color_manual(values = c("Fixed Effects" = "steelblue", "First Difference" = "darkorange")) +
  labs(x = "k", y = expression(gamma[k]), title = "C) FE vs First Difference", color = "", shape = "") +
  theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 10))

# Panel D : Robustesse fenêtres
p_windows_small <- ggplot(df_windows, aes(x = max_lag, y = beta_cum)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = signif), vjust = -1.5, size = 3) +
  scale_x_continuous(breaks = windows) +
  labs(x = "Max lags", y = expression(beta[cum]), title = "D) Robustesse fenêtres") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", size = 10))

# Combinaison
p_combined <- (p_gamma + p_beta) / (p_fe_fd_small + p_windows_small) +
  plot_annotation(
    title = "Effet du salaire minimum sur le chômage — Panel européen",
    subtitle = "Méthodologie : Schmidheiny & Siegloch (2023)",
    caption = "Source : Eurostat | IC 95%, erreurs-types clustérisées par pays",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

print(p_combined)
ggsave("figure_principale_4panels.png", p_combined, width = 12, height = 10, dpi = 300)
cat("✅ Sauvegardé : figure_principale_4panels.png\n")

# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================