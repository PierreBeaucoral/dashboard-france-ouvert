# ============================================================
# scripts/08_drees_apl.R
#
# Bloc 5 (Santé) — Accessibilité Potentielle Localisée (APL) à 5
# professions de santé : médecins généralistes, infirmiers, sages-femmes,
# chirurgiens-dentistes, kinésithérapeutes.
#
# Source : DREES (data.drees.solidarites-sante.gouv.fr, jeu 530), reprise sur data.gouv.fr :
#   https://www.data.gouv.fr/datasets/accessibilite-potentielle-localisee-apl-aux-professionnels-de-sante
#   Éditions de juillet 2026 : un classeur XLSX par profession, un onglet « APL <année> » par
#   millésime (2022, 2023, 2024). Les URL d'attachement DREES ci-dessous sont stables (le fichier
#   est remplacé sur place à chaque édition).
#
# Millésime : le plus récent onglet « APL <année> » commun aux 5 classeurs (lu dans les
# fichiers, jamais figé). Il est écrit dans la colonne `apl_millesime` du parquet, que les
# pages lisent pour leurs libellés.
#
# Mise en page (éd. 2026) : lignes 1-8 métadonnées (ligne 3 = « Millésime : AAAA »), ligne 9 =
# intitulés de colonnes, ligne 10 = unités, données dès la ligne 11. col 1 = code commune INSEE,
# col 2 = commune, col 3 = APL principal. Médecins : col 4/5/6 = APL des médecins de 65 / 62 /
# 60 ans et moins. Les colonnes sont repérées par leur intitulé, avec stopifnot sur la mise en page.
#
# Sortie : data/processed/drees/apl_medecins.parquet (schéma inchangé + `apl_millesime`)
#   code_commune, nom_commune, apl_medecins, apl_medecins_jeunes (<= 65 ans), apl_infirmieres,
#   apl_sagefemmes, apl_dentistes, apl_kines, apl_millesime
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(arrow)
  library(here)
  library(glue)
  library(stringr)
  library(purrr)
  library(tibble)
  library(curl)
})

raw_dir <- here::here("data", "raw", "drees")
out_dir <- here::here("data", "processed", "drees")
out_path <- file.path(out_dir, "apl_medecins.parquet")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

BASE <- paste0("https://data.drees.solidarites-sante.gouv.fr/api/v2/catalog/datasets/",
               "530_l-accessibilite-potentielle-localisee-apl/attachments/")

# Un classeur par profession. `pat_main` : intitulé attendu de la colonne principale ;
# `pat_jeunes` : intitulé de la variante « 65 ans et moins » (médecins seulement).
pros <- list(
  list(file = "apl_medecins.xlsx",    att = "indicateur_d_apl_aux_medecins_generalistes_xlsx",
       var = "apl_medecins",    pat_main = "^APL aux médecins généralistes$",
       pat_jeunes = "^APL aux médecins généralistes de 65 ans (et|ou) moins$"),
  list(file = "apl_infirmieres.xlsx", att = "indicateur_d_apl_aux_infirmiers_xlsx",
       var = "apl_infirmieres", pat_main = "^APL aux infirmi", pat_jeunes = NA),
  list(file = "apl_sagefemmes.xlsx",  att = "indicateur_d_apl_aux_sages_femmes_xlsx",
       var = "apl_sagefemmes",  pat_main = "^APL aux sages-femmes$", pat_jeunes = NA),
  list(file = "apl_dentistes.xlsx",   att = "indicateur_d_apl_aux_chirurgiens_dentistes_xlsx",
       var = "apl_dentistes",   pat_main = "^APL aux chirurgiens-dentistes$", pat_jeunes = NA),
  list(file = "apl_kines.xlsx",       att = "indicateur_d_apl_aux_kinesitherapeutes_xlsx",
       var = "apl_kines",       pat_main = "^APL aux kin", pat_jeunes = NA)
)

# ---- Téléchargement (cache idempotent ; un XLSX valide est un zip : octets « PK ») ----
is_xlsx <- function(p) file.exists(p) && file.size(p) > 1e5 &&
  identical(readBin(p, "raw", 2L), as.raw(c(0x50, 0x4b)))
# Le cache est invalidé si la taille distante (HEAD, Content-Length) diffère de celle du fichier local :
# une nouvelle édition DREES est ainsi reprise. HEAD en échec ou sans taille -> on garde le cache.
remote_size <- function(u) tryCatch({
  h <- curl::new_handle(useragent = "Mozilla/5.0", followlocation = TRUE, nobody = TRUE, timeout = 30)
  r <- curl::curl_fetch_memory(u, handle = h)
  as.numeric(curl::parse_headers_list(r$headers)[["content-length"]])
}, error = function(e) NA_real_)
for (s in pros) {
  p <- file.path(raw_dir, s$file)
  u <- paste0(BASE, s$att)
  rs <- remote_size(u)
  stale <- is_xlsx(p) && length(rs) == 1L && !is.na(rs) && rs > 0 && rs != file.size(p)
  if (stale) message(glue::glue("{s$file} : taille distante {rs} != locale {file.size(p)}, nouvelle édition ?"))
  if (!is_xlsx(p) || stale) {
    message(glue::glue("Téléchargement {s$file}…"))
    h <- curl::new_handle(useragent = "Mozilla/5.0", followlocation = TRUE)
    curl::curl_download(u, p, handle = h, quiet = TRUE)
  }
  if (!is_xlsx(p)) stop("08_drees_apl.R : ", s$file, " n'est pas un XLSX valide (URL changée ?)")
}

# ---- Millésime commun le plus récent ----
sheet_years <- function(p) {
  sh <- grep("^APL [0-9]{4}$", readxl::excel_sheets(p), value = TRUE)
  as.integer(sub("^APL ", "", sh))
}
common <- Reduce(intersect, purrr::map(pros, ~ sheet_years(file.path(raw_dir, .x$file))))
stopifnot("aucun onglet « APL <année> » commun aux 5 classeurs" = length(common) > 0)
MILLESIME <- max(common)
message("→ Millésime APL retenu (plus récent commun aux 5 classeurs) : ", MILLESIME)

# Helper : lit l'onglet du millésime, vérifie la mise en page, renvoie code + APL
parse_apl <- function(spec) {
  path  <- file.path(raw_dir, spec$file)
  d <- suppressMessages(readxl::read_excel(path, sheet = paste("APL", MILLESIME),
                                           col_names = FALSE, col_types = "text"))
  # Garde-fou de mise en page (éd. juillet 2026)
  stopifnot(
    # (DREES : l'onglet « APL 2023 » du classeur infirmiers porte « Millésime : 2024 » ; on lit donc
    # l'onglet du millésime retenu ET on exige que son en-tête annonce le même millésime)
    "ligne 3 = « Millésime : <millésime de l'onglet> »" = identical(d[[1]][3], paste("Millésime :", MILLESIME)),
    "ligne 9 col 1 = « Code commune INSEE »" = identical(d[[1]][9], "Code commune INSEE"),
    "ligne 9 col 2 = « Commune »" = identical(d[[2]][9], "Commune")
  )
  hdr <- unlist(d[9, ], use.names = FALSE)
  col_main <- grep(spec$pat_main, hdr)
  stopifnot("colonne APL principale introuvable/ambiguë" = length(col_main) == 1L,
            "colonne principale attendue en col 3" = col_main == 3L)
  body <- d[-(1:10), ]
  keep <- !is.na(body[[1]]) & nchar(body[[1]]) == 5L
  out <- tibble::tibble(
    code_commune = body[[1]][keep],
    nom_commune  = body[[2]][keep],
    !!spec$var  := suppressWarnings(as.numeric(body[[col_main]][keep]))
  )
  if (!is.na(spec$pat_jeunes)) {
    col_j <- grep(spec$pat_jeunes, hdr)
    stopifnot("colonne « 65 ans et moins » introuvable/ambiguë" = length(col_j) == 1L)
    out[[paste0(spec$var, "_jeunes")]] <- suppressWarnings(as.numeric(body[[col_j]][keep]))
  }
  stopifnot("codes commune dupliqués" = !anyDuplicated(out$code_commune),
            "moins de 34 000 communes" = nrow(out) > 34000L,
            "APL négatif" = all(out[[spec$var]] >= 0, na.rm = TRUE),
            "trop de NA" = mean(is.na(out[[spec$var]])) < 0.02)
  out
}

message("→ Lecture des 5 classeurs, onglet APL ", MILLESIME, "...")
parsed_list <- purrr::map(pros, parse_apl)

# Réduit par left_join sur code_commune (les médecins fixent le périmètre de communes)
combined <- purrr::reduce(
  parsed_list,
  function(a, b) dplyr::left_join(a, b |> dplyr::select(-dplyr::any_of("nom_commune")),
                                  by = "code_commune")
)
combined$apl_millesime <- as.integer(MILLESIME)

arrow::write_parquet(combined, out_path)

cat(glue::glue("

  ✔ APL {nrow(combined)} communes → {out_path}
    ({round(file.size(out_path)/1024)} KB)
    Colonnes : {paste(names(combined), collapse=', ')}

  Désert médical (APL généraliste < 2.5) :
"))
if ("apl_medecins" %in% names(combined)) {
  n_dm <- sum(combined$apl_medecins < 2.5, na.rm = TRUE)
  n_t  <- sum(!is.na(combined$apl_medecins))
  cat(glue::glue("    {n_dm} / {n_t} ({round(100*n_dm/n_t,1)}%)\n"))
}

cat("\n  Distributions par profession :\n")
for (col in grep("^apl_(medecins|infirmieres|sagefemmes|dentistes|kines)$",
                 names(combined), value = TRUE)) {
  s <- summary(combined[[col]])
  cat(glue::glue("    {col} : med={s['Median']} mean={round(s['Mean'],2)}\n"))
}
