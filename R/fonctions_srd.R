# ============================================================
# Calcul du SRD et du niveau d'alerte pour n'importe quelles valeurs
# d'indicateurs (observées, projetées ou simulées).
# Utilisé par l'application (simulateur). Même règle que 04_analyse_srd.R :
# chaque valeur est située dans la distribution observée 2016-2025.
# ============================================================

# Part d'observations historiques « moins risquées » que la valeur x (0-1)
rang_hist <- function(ref_valeurs, sens, x) {
  ref <- sens * ref_valeurs
  ref <- ref[!is.na(ref)]
  vapply(sens * x, function(v) if (is.na(v)) NA_real_ else mean(ref < v), numeric(1))
}

# df     : lignes à évaluer (colonnes des composantes du SRD)
# ref    : lignes observées 2016-2025 (distribution de référence)
# poids  : sortie srd-composantes-poids (axe, variable, sens, poids)
# seuils : sortie srd-seuils-matrice (axe, percentile, seuil)
scorer_srd <- function(df, ref, poids, seuils, part_situation = 0.7) {
  score_axe <- function(a) {
    p <- poids[poids$axe == a, ]
    m <- vapply(seq_len(nrow(p)), function(i)
      rang_hist(ref[[p$variable[i]]], p$sens[i], df[[p$variable[i]]]), numeric(nrow(df)))
    m <- matrix(m, nrow = nrow(df))
    w <- matrix(p$poids, nrow = nrow(m), ncol = ncol(m), byrow = TRUE)
    w[is.na(m)] <- 0
    m[is.na(m)] <- 0
    100 * rowSums(m * w) / pmax(rowSums(w), 1e-9)
  }
  s <- function(a, q) seuils$seuil[seuils$axe == a & seuils$percentile == q]
  sit <- score_axe("situation")
  deg <- score_axe("degradation")
  df$situation <- sit
  df$degradation <- deg
  df$srd <- part_situation * sit + (1 - part_situation) * deg
  df$alerte <- ifelse(sit >= s("situation", "P90") | (sit >= s("situation", "P75") & deg >= s("degradation", "P50")), "rouge",
               ifelse(sit >= s("situation", "P75") | (sit >= s("situation", "P50") & deg >= s("degradation", "P75")), "orange",
               ifelse(sit >= s("situation", "P50") | deg >= s("degradation", "P75"), "jaune", "vert")))
  df
}
