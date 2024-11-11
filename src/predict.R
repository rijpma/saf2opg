rm(list = ls())

library("data.table")
library("xgboost")
library("stringdist")
library("stringi")

# remotes::install_github("rijpma/capelinker", dependencies = FALSE)
library("capelinker")

# set stringdist cores
options(sd_num_thread = 8)
# set xgboost cores?
# set data.table cores?
getDTthreads()
setDTthreads(8)

# cnd = fread("~/data/cape/saf/saf_couples_candidates_full.csv", na.strings = "")
# cnd = fread("~/data/cape/saf/saf/saf2opg_candidates_1750_1820.csv", na.strings = "")
# cnd = fread("saf2opg_candidates_1750_1820.csv", na.strings = "")
# cnd = fread("saf2opg_candidates_1700_1824.csv", na.strings = "")
cnd = fread("~/data/cape/opg/saf2opg_candidates_1600_1824.csv", na.strings = "")


cnd[, mfirstdist := stringdist::stringdist(firstnames, mfirst, method = "jw")]
cnd[, mlastdist := stringdist::stringdist(surname, mlast, method = "jw")]

cnd[, wfirstdist := stringdist::stringdist(spouse_firstnames_clean, wfirst, method = "jw")]
cnd[, wlastdist := stringdist::stringdist(spouse_surname_clean, wlast, method = "jw")]

# maybe we had this at an earlier stage?
# later merge back into original cleaned files
cnd[, minitals_saf := ego_initials]
cnd[, minitals_opg := minitials] # def. here
cnd[, minitialsdist_osa := 1 - stringdist::stringsim(minitals_saf, minitals_opg, method = "osa")]

cnd[, winitals_saf := spouse_initials]
cnd[, winitals_opg := winitials]
cnd[, winitialsdist_osa := 1 - stringdist::stringsim(winitals_saf, winitals_opg, method = "osa")]

# this might be problematic because the year range is now off
# sigh
# though really, someone not age 20-70 surely shouldn't be in there?
cnd[, implied_age := year - sy]
cnd[, husb_implied_age_at_mar := year - married_year]

# cnd[, maryear_initialsdist_osa := 1 - stringdist::stringsim(as.character(married_year_ego_husb), as.character(married_year_ego_wife), method = "osa")]
# cnd[, list(married_year_ego_husb, married_year_ego_wife)]


# add surnamedist
cnd[, cross_surnamedist := stringdist::stringdist(firstnames, wlast, method = "jw")]

cnd[, wfirst_is_initials:= len_longest_word(wfirst) == 1]
cnd[, mfirst_is_initials:= len_longest_word(mfirst) == 1]


cnd[, years_married := year - married_year]

cnd[, `(Intercept)` := 1] #jfc

# gc()

cnd[,
    pred := predict(
        object = pretrained_models$m_boost_saf2opg$model,
        newdata = xgboost::xgb.DMatrix(as.matrix(.SD))), 
    .SDcols = pretrained_models$m_boost_saf2opg$model$feature_names]

# like this means tie for first place is >1, and so discarded below
cnd[, r := rank(-pred), by = list(couple_id, year)]
cnd[persid == "stel_18916" & pred > 0.5][order(r)]
# so note that in some cases, you have multiple couples for one opg observation (persid) which meet this criterion
# so you also need to select based on persid
cnd[, r2 := rank(-pred), by = persid]

cnd[, lapply(5:9 / 10, function(i) sum(r == 1 & pred > i))]
cnd[, lapply(5:9 / 10, function(i) sum(r == 1 & pred > i) / uniqueN(couple_id))]

out = cnd[r == 1 & r2 == 1 & pred > 0.5, list(couple_id, persid, idx, year, pred)]
out[, individual_id := stringi::stri_replace_last_regex(couple_id, "_mar_\\d", "")]
out[, nth_marriage := stringi::stri_extract_last_regex(couple_id, "\\d$")]


fwrite(out, "saf2opg_1600_1824_2024nov7.csv")
# fwrite(out, "saf2opg_1700_1824_2022mar7.csv")
# fwrite(out, "saf2opg_1750_1820_2022jan27.csv")
