# Wiring des arrondissements PLM dans la page Législatives : le lookup (fiche communale) porte le
# résultat PROPRE de chaque arrondissement ; la ligne ville reste la somme. Lancer APRES le rendu
# (ou apres R/build_web_assets.R) : Rscript tests/check_elections_arr.R
lk <- jsonlite::fromJSON(here::here("assets", "data", "elections_lookup.json"))
el <- arrow::read_parquet(here::here("data", "processed", "elections", "legislatives_2024_t2.parquet"))
ea <- arrow::read_parquet(here::here("data", "processed", "elections", "legislatives_2024_arrondissements.parquet"))
i <- function(code) match(code, lk$c)
stopifnot(!anyDuplicated(lk$c), !anyNA(i(ea$code_insee)), !anyNA(i(c("75056", "69123", "13055"))))
# chaque arrondissement du lookup == sa ligne du parquet arrondissements
stopifnot(identical(lk$famille_vainqueur[i(ea$code_insee)], ea$famille_vainqueur),
          identical(lk$tour[i(ea$code_insee)], ea$tour),
          all(lk$inscrits[i(ea$code_insee)] == ea$inscrits))
# chaque ville du lookup == sa ligne du parquet commune (somme), distincte de ses arrondissements
for (cty in c("75056", "69123", "13055")) {
  stopifnot(lk$inscrits[i(cty)] == el$inscrits[el$code_insee == cty],
            lk$famille_vainqueur[i(cty)] == el$famille_vainqueur[el$code_insee == cty])
}
# Paris 16e : son propre résultat (ENS), pas celui de la ville (NFP)
stopifnot(lk$famille_vainqueur[i("75116")] == "ENS", lk$famille_vainqueur[i("75056")] == "NFP")
# Geay : repli T1 signalé
stopifnot(isTRUE(lk$repli_t1[i("79131")]), sum(lk$repli_t1, na.rm = TRUE) == 1L)
cat("OK check_elections_arr\n")
