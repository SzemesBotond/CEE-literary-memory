# Run distributions.R first.
library(dplyr)
library(openxlsx)

# Authors appearing in all three foreign-language Wikipedias.
hu_canon  <- intersect(intersect(hu_sl$cid, hu_pol$cid), hu_cz$cid)
pol_canon <- intersect(intersect(pol_sl$cid, pol_hu$cid), pol_cz$cid)
sl_canon  <- intersect(intersect(sl_pol$cid, sl_hu$cid), sl_cz$cid)
cz_canon  <- intersect(intersect(cz_pol$cid, cz_hu$cid), cz_sl$cid)

canon_ids <- list(Hungarian = hu_canon, Polish = pol_canon,
                  Czech = cz_canon, Slovak = sl_canon)
canon <- bind_rows(author_groups, .id = "Language") %>%
  group_by(Language) %>%
  filter(cid %in% canon_ids[[first(Language)]]) %>%
  distinct(cid, .keep_all = TRUE) %>%
  ungroup() %>%
  select(születési_név, Language)

write.xlsx(canon, "visegrad-canon.xlsx", overwrite = TRUE)
