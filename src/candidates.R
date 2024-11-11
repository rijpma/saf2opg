library("data.table")
library("bit64")
library("capelinker")
# library("writexl")
library("stringi")

options(sd_num_thread = 8)
setDTthreads(8)

setwd("~/data/cape/opg/")

saf = fread("../saf/saf4spouselinkage_full_clean.csv",
    na.string = "")

# opg = fread("gunzip -c ../opgaafrol_all_linked_20220220.csv.gz",
#     na.string = "")
opg = fread("stellenbosch_long_linked_graphs.csv",
    na.string = "")

opg[, mlast_woprefix := mlast]
opg[, mlast := paste(mvoor, mlast)]
opg[, mlast := stringi::stri_trim_both(mlast)]
opg[, wlast_woprefix := wlast]
opg[, wlast := paste(wvoor, wlast)]
opg[, wlast := stringi::stri_trim_both(wlast)]
opg[opg == ""] = NA

opg[, rol := "stel-long"]

opg[, persid := stri_join(rol, "_", persid)]
opg[, idx := stri_join(rol, "_", index)]
opg[, wifepresent := !is.na(names_women_clean)]
# opg[, wineproducer := as.logical(wineproducer)]

opg = opg[,
    list(
        rol,
        year = as.integer(year),
        persid,
        idx,
        nr,
        # districtall,
        mlast,
        mfirst,
        # mlast_woprefix,
        # mprefix,
        minitials,
        wlast,
        wfirst,
        # wlast_woprefix,
        # wprefix,
        winitials,
        # wineproducer,
        old,
        young,
        wifepresent,
        # spousenamedist = as.numeric(spousenamedist),
        # namefreq = as.integer(namefreq),
        # mfirst_uniqueness = as.numeric(mfirst_uniqueness),
        # mfirst_cos_uniqueness = as.numeric(mfirst_cos_uniqueness),
        settlerchildren = as.numeric(settler_children),
        # settlermen = as.integer(settler_men),
        # settlerwomen = as.integer(settler_women),
        NULL)]

opg = rapply(opg, tolower, classes = "character", how = "replace")
# id idn year old young are duplicated columns, so drop


# these look ok
# capelinker::preflight(
#     dat = opg,
#     modstring = "m_boost_stel_rein")
# capelinker::preflight(
#     dat = saf[, list(
#         mlast = surname,
#         mfirst = firstnames,
#         minitials = ego_initials,
#         wlast = spouse_surname_clean,
#         wfirst = spouse_firstnames_clean,
#         winitials = spouse_initials,
#         year = as.numeric(married_year))], 
#     modstring = "m_boost_stel_rein")


# saf has more multiple first names: 80 v. 50%
# nb that usually, but not always the man is the mlast
fill = list()

# gaps in years now, so get unique years first
years = saf[sy <= 1826 & gender == 1, sort(unique(sy))]

for (i in years){
    gc()
    tolink_saf = saf[sy == i & gender == 1, 
                list(
                    couple_id, # individual_id, family_id,
                    surname,
                    firstnames,
                    ego_initials,
                    spouse_surname_clean,
                    spouse_firstnames_clean,
                    spouse_initials,
                    sy,
                    married_year,
                    deathyear,
                    marriage)]

    tolink_opg = opg[between(year, i + 16, i + 100)]

    cat(i, "-- linking", nrow(tolink_saf), "couples ")

    # issue: this fails if nrow = 1 (dim problem) or 1 individual married multiple times (apply returns data frame/matrix instead of list)
    fill[[as.character(i)]] <- capelinker::candidates(
        dat_from = tolink_saf,
        dat_to = tolink_opg,
        idvariable_from = "couple_id", # rename on the fly?
        idvariable_to = "persid",
        blockvariable_from = "surname",
        blockvariable_to = "mlast",
        blocktype = "bigram distance",
        linktype = "one:one",
        maxdist = 0.2)

    cat("-- created", nrow(fill[[as.character(i)]]), "candidates\n")
}

gc()
cnd = rbindlist(fill, id = "linkvariable")
gc()
fwrite(cnd, "saf2opg_candidates_1600_1824.csv")

# pretoriusa2_mar_1
# stel-long_1284

# candidates for training data

# plot(saf[, .N, by = sy])

saf_sample = saf[between(sy, 1750, 1800) & gender == 1]
set.seed(123)
smpl = sample(saf_sample$couple_id, 500)
saf_sample = saf_sample[couple_id %in% smpl]

years = saf_sample[, sort(unique(sy))]
# so let's say we take saf people from 1750


x = capelinker::candidates(
    # saf_sample[sy == years[1]], # 500 peoples
    saf_sample, # 500 peoples
    opg,
    # stel[between(year, 1750 + 16, 1750 + 65)], # 52k peoples ()
    idvariable_from = "couple_id", # rename on the fly?
    idvariable_to = "persid",
    blockvariable_from = "surname",
    blockvariable_to = "mlast",
    blocktype = "bigram distance",
    linktype = "one:one",
    maxdist = 0.2)

out = x[, list(couple_id, sy, surname, firstnames, spouse_surname_clean, spouse_firstnames_clean, married_year, deathyear,
        persid, idx, year, mlast, mfirst, old, young, wlast, wfirst)]

out[, implied_age := year - sy]
out[, mlastsim := stringdist::stringsim(surname, mlast, method = "jw")]
out[, mfirstsim := stringdist::stringsim(firstnames, mfirst, method = "jw")]
out[, wlastsim := stringdist::stringsim(spouse_surname_clean, wlast, method = "jw")]
out[, wfirstsim := stringdist::stringsim(spouse_firstnames_clean, wfirst, method = "jw")]
out[, overallsim := exp(rowMeans(log(.SD))), .SDcols = patterns("tsim")]
out[, mean_overallsim := mean(overallsim, na.rm = TRUE), by = list(couple_id, idx)]

out[, sum(overallsim > 0.8, na.rm = TRUE)]
out[overallsim > 0.8, uniqueN(idx)]
out[overallsim > 0.8, uniqueN(couple_id)]

# ftable(out[, list(round(overallsim, 1), round(mean_overallsim, 1))])

out = out[order(-mean_overallsim, idx, overallsim)]
out_long = split(out, by = "couple_id")
set.seed(312)
shuffled_names = sample(names(out_long))
out_long = out_long[shuffled_names]
out_long = rbindlist(out_long)
out_long = out_long[, rbindlist(list(.SD, list(NA)), fill = TRUE), by = couple_id]
out_long[, link := NA]
out_long[, comment := NA]

out_long[couple_id == "van der westhuizena1b3c8d10_mar_1"][1:10]
out_long[couple_id %in% "jansen van rensburga1b4c1d6_mar_1"][20:30]


# writexl::write_xlsx(out_long, "~/data/cape/saf/saf2opg_candidates.xlsx")
# writexl::write_xlsx(out_long[couple_id %in% shuffled_names[c(1:275)]], "~/data/cape/saf/saf2opg_candidates_batch1.xlsx")
# writexl::write_xlsx(out_long[couple_id %in% shuffled_names[c(1:50, 276:500)]], "~/data/cape/saf/saf2opg_candidates_batch2.xlsx")


out_wide = out[, lapply(.SD, function(x) stri_flatten(unique(x), collapse = "/")), 
    by = list(couple_id, sy, surname, firstnames, spouse_surname_clean, spouse_firstnames_clean, idx), 
    .SDcols = patterns("last$|first$|year")]
scores = out[, lapply(.SD, function(x) stri_flatten(round(range(x), 2), collapse = "-")), .SDcols = patterns("sim$"), by = list(couple_id, idx)]
scores2 = out[, list(mean_overallsim = mean(overallsim, na.rm = TRUE)), by = list(couple_id, idx)]
out_wide = merge(out_wide, scores, by = c("couple_id", "idx"))
out_wide = merge(out_wide, scores2, by = c("couple_id", "idx"))
out_wide = out_wide[order(-mean_overallsim)]
out_wide = split(out_wide, by = "couple_id")
out_wide = out_wide[shuffled_names]
out_wide = rbindlist(out_wide)
out_wide = out_wide[, rbindlist(list(.SD, list(NA)), fill = TRUE), by = couple_id]
# writexl::write_xlsx(out_wide, "~/data/cape/saf/saf2opg_candidates_widetest.xlsx")

# randomise the draw later
# check "withingenealogies.R" and spousallinks_candidates.Rfor how it was done previously

