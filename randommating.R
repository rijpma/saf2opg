rm(list = ls())

library("data.table")
library("stringi")

saf_long = fread("~/data/cape/saf/saf4spouselinkage_full_clean.csv", na.strings = "")
cousins = fread("~/data/cape/saf/cousins.csv", na.strings = "")
links = fread("~/data/cape/saf/spousallinks_full_2021jan6.csv", na.strings = "")
saf2opg = fread("~/data/cape/saf2opg_1700_1824_2022mar7.csv", na.strings = "")
opg = fread(cmd = "gunzip -c /Users/aukerijpma/data/cape/opg/opgaafrol_all_linked_20220220.csv.gz", na.strings = "")

cousins = unique(cousins[siblings == FALSE])
cousins[, couple := stringi::stri_join(child.x, child.y)]

# add opgaafrollen for location info
opg[, persid := paste0(rol, "_", persid)]
opg = merge(
    opg[, list(rol, persid, district, districtall, index, year, mfirst, mlast, wfirst, wlast)],
    saf2opg, 
    by = "persid",
    all.x = TRUE,
    suffixes = c("", "_link"))

# first observation
firstrol = opg[!is.na(couple_id)][order(year), 
    list(firstrol = rol[1],
        firstdistrict = na.omit(district)[1],
        firstdistrictall = na.omit(districtall)[1]), 
    by = couple_id]
# nb na.omit leads to some rol-district discrepancies

# merge into saf
saf_long = merge(saf_long,
    firstrol,
    by = "couple_id",
    all.x = TRUE)

# on basis of average number of cousins
cousins[, uniqueN(child.y), by = child.x][, mean(V1)]
cousins[, .N, by = child.x][, mean(N)]
# numbers differ because sometimes through multiple grandparents
# note also that the genders here are tricky, child.x and child.y are both!
saf_long[individual_id %chin% cousins$child.x, .N, by = gender]
# so on average, 15 female cousins?
15 / saf_long[gender == 2, .N] * 100
15 / saf_long[!duplicated(individual_id) & gender == 2, .N] * 100
# 0.01 pct

# marry completely at random

N = 100
x = rep(NA_integer_, N)
for (i in 1:N){
    husbands_sample = sample(saf_long[gender == 1, individual_id], 10e3)
    wives_sample = sample(saf_long[gender == 2, individual_id], 10e3)
    x[i] = sum(stringi::stri_join(husbands_sample, wives_sample) %chin% cousins$couple)
    cat(i, "-")
}
x = x / 100 # now in percentages (actual!)
mean(x) # 0.003 pct
sd(x)
table(x)

# alternatively, pair a sample with everyone
husbands_sample = sample(saf_long[gender == 1, individual_id], 100)
husbands = saf_long[individual_id %in% husbands_sample]
couples = merge(husbands[, list(individual_id, id = 1)],
    saf_long[gender == 2, list(individual_id, id = 1)],
    by = "id", 
    allow.cartesian = TRUE)
couples[, couple := stringi::stri_join(individual_id.x, individual_id.y)]
couples[, cousin := couple %chin% cousins$couple]
couples[, mean(cousin), by = individual_id.x][, mean(V1)] * 100
# 0.002%
rm("couples")

# block sample by age
# first the distribution of spousal age gaps to see the range

links = merge(links,
    saf_long[, list(couple_id, sy_husb = sy, gender)],
    by.x = "couple_id_from", # husband
    by.y = "couple_id")
links = merge(links,
    saf_long[, list(couple_id, sy_wife = sy, gender)],
    by.x = "couple_id_to", # husband
    by.y = "couple_id")

# rare, but funky gaps exist
# is there birthyear in the model?
hist(links[between(sy_wife - sy_husb, -50, 50), sy_wife - sy_husb], breaks = 100)
quantile(links[between(sy_wife - sy_husb, -50, 50), sy_wife - sy_husb], (0:10)/10)
# so -2 to +14 covers pretty 80%

saf_long[, wife_upper := sy - 14]
saf_long[, wife_lower := sy + 2]

# allowing for age diff
# test run on 1
vermeulen = saf_long[
    individual_id %in% "vermeulena2b3c3d4e4f4g8", 
    list(individual_id_husb = individual_id, sy_husb = sy, sy_husb_original = sy)]
candidates = saf_long[
            gender == 2 & !is.na(sy), 
            list(individual_id_wife = individual_id, sy_wife = sy, wife_upper, wife_lower)]
candidates[between(sy_wife, 1892 - 2, 1892 + 14)]
candidates[vermeulen, on = list(wife_upper <= sy_husb, wife_lower >= sy_husb)]
# works

husbands_sample = sample(saf_long[gender == 1, individual_id], 1000)
husbands = saf_long[individual_id %in% husbands_sample, list(individual_id_husb = individual_id, sy_husb = sy, sy_husb_original = sy)]

# non-equi join on the years
spouses = candidates[husbands, on = list(wife_upper <= sy_husb, wife_lower >= sy_husb)]

spouses[, cousin := stringi::stri_join(individual_id_husb, individual_id_wife) %chin% cousins$couple]
spouses[, mean(cousin), by = individual_id_husb][, mean(V1)] * 100
# 0.02%
# or should it be?
spouses[, mean(cousin)] * 100

# so now also block on geo!
# that doesn't work well because we don't have a rol for the women
# which wouldn't make sense because we observe them in a couple

# samle of husbands
husbands = saf_long[
    individual_id %in% husbands_sample
    & !is.na(firstrol), 
    list(
        firstrol = firstrol, 
        firstdistrictall = firstdistrictall,
        individual_id_husb = individual_id, 
        sy_husb = sy, 
        sy_husb_original = sy)]

# add geo info to sample of women, nb only possible when they're married
links = merge(links, 
    firstrol,
    by.x = "couple_id_from", by.y = "couple_id",
    all.x = TRUE)
candidates = merge(saf_long[, -c("firstrol", "firstdistrictall")],
    links[, list(couple_id_to, firstrol, firstdistrictall)],
    by.x = "couple_id",
    by.y = "couple_id_to",
    all.x = TRUE,)
candidates = candidates[
    gender == 2 
    & !is.na(sy)
    & !is.na(firstrol),
    list(
        firstrol = firstrol, 
        firstdistrictall = firstdistrictall,
        individual_id_wife = individual_id, 
        sy_wife = sy, 
        wife_upper, 
        wife_lower)]

# non-equi join on the years and geo
spouses = candidates[husbands, on = list(firstrol, wife_upper <= sy_husb, wife_lower >= sy_husb)]

spouses[, cousin := stringi::stri_join(individual_id_husb, individual_id_wife) %chin% cousins$couple]
spouses[, .N, by = individual_id_husb]
spouses[, mean(cousin), by = individual_id_husb][, mean(V1)] * 100
# so, on basis of average N cousins, 0.01%
# so, on basis of simulation, ignoring age, 0.003%
# so, on basis of simulation, including age, 0.02%
# so, on the basis of district and age, 0.3%
# that's higher, but still low

# and finally, same thing but on veldcornetschap
# much larger number because vc is missing often
husbands_sample = sample(saf_long[gender == 1, individual_id], 10000)
husbands = saf_long[
    individual_id %in% husbands_sample
    & !is.na(firstdistrictall), 
    list(
        firstrol = firstrol, 
        firstdistrictall = firstdistrictall,
        individual_id_husb = individual_id, 
        sy_husb = sy, 
        sy_husb_original = sy)]
candidates = candidates[!is.na(firstdistrictall)]

# the non-equi join
spouses = candidates[husbands, on = list(firstdistrictall, wife_upper <= sy_husb, wife_lower >= sy_husb)]

spouses[, cousin := stringi::stri_join(individual_id_husb, individual_id_wife) %chin% cousins$couple]
spouses[, .N, by = individual_id_husb]
spouses[, mean(cousin), by = individual_id_husb][, mean(V1)] * 100
# 0.9% but this strikes me as a noisy number