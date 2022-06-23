rm(list = ls())

library("data.table")
library("stringi")
library("fixest")
library("ggplot2")

mypar = function(...){
    par(..., 
        bty = "l", 
        mar = c(3, 3, 2, 1), 
        mgp = c(1.7, .5, 0), 
        tck=-.01,
        font.main = 1)
}

# data in
saf_long = fread("~/data/cape/saf/saf4spouselinkage_full_clean.csv", na.strings = "")

spouselinks = fread("~/data/cape/saf/spousallinks_full_2021jan6.csv", na.strings = "")

parentchild = fread("~/data/cape/saf/parentchild.csv", na.strings = "")

siblings = fread("~/data/cape/saf/siblings.csv", na.strings = "")

fabr = fread("~/data/cape/saf/fabr.csv")
mobr = fread("~/data/cape/saf/mobr.csv")
fasi = fread("~/data/cape/saf/fasi.csv")
mosi = fread("~/data/cape/saf/mosi.csv")
consan = rbindlist(
    list(
        fbd = fabr[, list(child_id, husband_id, wife_id, grandparent_id)], 
        fsd = fasi[, list(child, husband, wife, grandparent_id)], 
        mbd = mobr[, list(child, husband, wife, grandparent_id)], 
        msd = mosi[, list(child, husband, wife, grandparent_id)]),
    use.names = FALSE, id = "type")
# some duplicates here due to multiple lines giving consan marriage

opg = fread(cmd = "gunzip -c ~/data/cape/opg/opgaafrol_all_linked_20220220.csv.gz")

saf2opg = fread("~/data/cape/saf2opg_1700_1824_2022mar7.csv")

opg[, persid := paste0(rol, "_", persid)]

opg[, carts := as.numeric(carts)]
opg[, vines := as.numeric(vines)]
opg[, reapwheat := as.numeric(reapwheat)]
opg[, reapbarley := as.numeric(reapbarley)]

vrbs = c("pigs", "carts", "sheep", "donkeys", "goats", "wagons", "horses", "cattle", "slaves", "vines")

m = prcomp(~ pigs + carts + sheep + goats + wagons + horses +
    cattle + slaves + vines, data = opg, rank. = 1, scale = TRUE)
opg[-na.action(m), pca := m$x]
opg[!is.na(pca), quantile(pca, 0:4/4)]
#         0%        25%        50%        75%       100% 
# -1.0319053 -1.0043750 -0.6722564  0.3176991 55.4929601 

# data merging
# men as ego only
saf_long = saf_long[gender == 1]


# spouse links
saf_long = merge(saf_long, 
    spouselinks[, list(couple_id = couple_id_from, individual_id_wife = individual_id_to, pred = pred)],
    by = "couple_id",
    all.x = TRUE)

saf_long[occupation != "", farmer := grepl("farm", occupation)]

saf_long[!is.na(firstrol), quantile(sy)]
cuts = c(1750, 1775, 1800, 1820)
saf_long[, period := cut(sy, cuts, labels = cuts[-length(cuts)])]
saf_long[, period := as.numeric(as.character(period))]


# quick check brother-sister marriages -- rare
saf_long[stringi::stri_sub(individual_id,0, -3) == stringi::stri_sub(individual_id_wife, 0, -3)]

# add consan marriages

saf_long[!is.na(individual_id_wife), con := paste(individual_id, individual_id_wife) %in% paste(consan$husband_id, consan$wife_id)]
saf_long[!is.na(individual_id_wife), fbd := paste(individual_id, individual_id_wife) %in% paste(fabr$husband_id, fabr$wife_id)]
saf_long[!is.na(individual_id_wife), mbd := paste(individual_id, individual_id_wife) %in% paste(mobr$husband, mobr$wife)]
saf_long[!is.na(individual_id_wife), fsd := paste(individual_id, individual_id_wife) %in% paste(fasi$husband, fasi$wife)]
saf_long[!is.na(individual_id_wife), msd := paste(individual_id, individual_id_wife) %in% paste(mosi$husband, mosi$wife)]
# note that some are two; need to check whether this makes sense
saf_long[fbd + fsd + mbd + msd == 2]

saf_long[!is.na(individual_id_wife), child_of_con := individual_id %in% consan$child_id]
saf_long[!is.na(individual_id_wife), child_of_fbd := individual_id %in% fabr$child_id]
saf_long[!is.na(individual_id_wife), child_of_mbd := individual_id %in% mobr$child]
saf_long[!is.na(individual_id_wife), child_of_fsd := individual_id %in% fasi$child]
saf_long[!is.na(individual_id_wife), child_of_msd := individual_id %in% mosi$child]

saf_long[!is.na(individual_id_wife), parent_of_con := individual_id %in% consan$grandparent_id]
saf_long[!is.na(individual_id_wife), parent_of_fbd := individual_id %in% fabr$grandparent_id]
saf_long[!is.na(individual_id_wife), parent_of_mbd := individual_id %in% mobr$grandparent_id]
saf_long[!is.na(individual_id_wife), parent_of_fsd := individual_id %in% fasi$grandparent_id]
saf_long[!is.na(individual_id_wife), parent_of_msd := individual_id %in% mosi$grandparent_id]

# add saf IDs to opg
opg = merge(
    opg[, cbind(rol, persid, index, year, mfirst, mlast, wfirst, wlast, .SD, pca, reapbarley, reapwheat), .SDcols = vrbs],
    saf2opg, 
    by = "persid",
    all.x = TRUE,
    suffixes = c("", "_link"))

# add opg to saf
# for which we need to average over couples
tomerge = opg[!is.na(couple_id), lapply(.SD, mean, na.rm = TRUE), .SDcols = c(vrbs, "pca", "reapwheat", "reapbarley", "year"), by = list(couple_id)]

# add opg locations to saf
freqrol = opg[!is.na(couple_id), .N, by = list(freqrol = rol, couple_id)][order(-N)][!duplicated(couple_id)]
firstrol = opg[!is.na(couple_id)][order(year), list(firstrol = rol[1]), by = couple_id]
# or do the pca here on the averaged data? barely makes a difference, not even for 

tomerge = merge(tomerge, freqrol, by = "couple_id", all = TRUE)
tomerge = merge(tomerge, firstrol, by = "couple_id", all = TRUE)

tomerge[!is.na(pca), pcaq := cut(pca, quantile(pca, 0:4/4), include.lowest = TRUE, dig.lab = 2)]
tomerge[, pcaqn := as.numeric(pcaq)]
quantile_top = c(0, 0.25, 0.5, 0.75, 0.8, 0.9, 0.99, 1)
tomerge[!is.na(pca), pcaq_top := cut(pca, quantile(pca, quantile_top), include.lowest = TRUE, dig.lab = 2)]
tomerge[, pcaq_top_n := ..quantile_top[as.numeric(pcaq_top) + 1]]

tomerge[!is.na(pca), 
    pcaq_reg := cut(pca, quantile(pca, 0:4/4), include.lowest = TRUE, dig.lab = 2),
    by = firstrol]
tomerge[, pcaq_reg_n := as.numeric(pcaq_reg)]
tomerge[!is.na(pcaq_reg_n), pcaq_reg_n := pcaq_reg_n - min(pcaq_reg_n) + 1, by = firstrol]

tomerge[cattle > 0, 
    cattleq_reg := cut(cattle, quantile(cattle, 0:4/4), include.lowest = TRUE, dig.lab = 2),
    by = firstrol]
tomerge[, cattleq_reg_n := as.numeric(cattleq_reg)]
tomerge[!is.na(cattleq_reg_n), cattleq_reg_n := cattleq_reg_n - min(cattleq_reg_n) + 1, by = firstrol]
tomerge[cattle == 0, cattleq_reg_n := 0]

tomerge[slaves > 0, 
    slavesq_reg := cut(slaves, quantile(slaves, 0:4/4), include.lowest = TRUE, dig.lab = 2),
    by = firstrol]
tomerge[, slavesq_reg_n := as.numeric(slavesq_reg)]
tomerge[!is.na(slavesq_reg_n), slavesq_reg_n := slavesq_reg_n - min(slavesq_reg_n) + 1, by = firstrol]
tomerge[slaves == 0, slavesq_reg_n := 0]

dim(saf_long)
saf_long = merge(saf_long, tomerge, by = "couple_id", all.x = TRUE, all.y = FALSE)
dim(saf_long)
# consider a all = TRUE link so you can get all the relevant info in one dataset
# but what do you do with the 

# check a few links
set.seed(8912)
linktests = saf2opg[sample(.N, 5)]
saf_long[match(linktests$couple_id, couple_id), .SD, .SDcols = patterns("(firstnames|surname)$")]
opg[match(linktests$persid, persid), .SD, .SDcols = patterns("(last|first)$")]

# linkage percentages
saf_long[between(sy, 1700, 1824), mean(!is.na(firstrol))]
opg[, mean(!is.na(couple_id))]

pdf("~/repos/saf2opg/out/saf2opgrates.pdf", height = 4, width = 8)
mypar(mfrow = c(1, 2))
toplot = saf_long[between(sy, 1700, 1824), mean(!is.na(firstrol)), by = sy][order(sy)]
plot(toplot, pch = 19, type = 'b', col = 2,
    xlab = "birth year", ylab = "share linked to opg",
    main = "OPG found in SAF")
toplot = opg[, mean(!is.na(couple_id)), by = year]
plot(toplot[order(year)], pch = 19, type = 'b', col = 2,
    xlab = "census year", ylab = "share linked to saf",
    main = "SAF found in OPG")
dev.off()

# prevalence
# overall
out = saf_long[!is.na(individual_id_wife) & !is.na(couple_id) & gen >= 3 & between(sy, 1750, 1820),
    lapply(.SD * 100, mean),
    .SDcols = c("con", "fbd", "fsd", "mbd", "msd")]
knitr::kable(transpose(out, keep.names = "type"), digits = 1)

saf_long[, decade := round(floor(sy / 10) * 10)]
saf_long[, linked := !is.na(firstrol)]

toplot = cube(
    saf_long[!is.na(individual_id_wife) & !is.na(couple_id) & gen >= 3 & between(sy, 1750, 1819)],
    j = list(share_con = mean(con)),
    by = c("linked", "decade"))

m = lm(con ~ as.factor(decade) - 1, data = saf_long[!is.na(individual_id_wife) & !is.na(couple_id) & gen >= 3 & between(sy, 1750, 1820)])
matplot(confint(m), type = 'l')
saf_long[!is.na(individual_id_wife) & !is.na(couple_id) & gen >= 3 & between(sy, 1750, 1820),
    mean(con)]

pdf("~/repos/saf2opg/out/consan_overtime.pdf", height = 6)
mypar() 
plot(share_con ~ decade,
    data = toplot[linked == TRUE][order(decade)], 
    type = 'b', pch = 19, col = 2,
    ylab = "share cons.", xlab = "birth decade",
    ylim = c(0, 0.13))
lines(share_con ~ decade,
    data = toplot[is.na(linked)][order(decade)],
    type = 'b', pch = 19, col = "gray")
lines(share_con ~ decade,
    data = toplot[linked == FALSE][order(decade)],
    type = 'b', pch = 19, col = 1)
legend("bottomright", fill = c(1, "gray", 2), legend = c("linked", "all", "unlinked"))
dev.off()

# by occupation
toplot = saf_long[!is.na(individual_id_wife) & !is.na(couple_id) & gen >= 3 & between(sy, 1750, 1819),
    j = list(share_con = mean(con), .N, sd = sd(con)),
    by = c("farmer", "decade")]
pdf("~/repos/saf2opg/out/consan_overtime_farmer.pdf", height = 6)
mypar()
plot(share_con ~ decade, data = toplot[farmer == TRUE][order(decade)], 
    type = 'b', pch = 19, col = 1,
    ylim = c(0, 0.12))
lines(share_con ~ decade, data = toplot[farmer == FALSE][order(decade)], 
    type = 'b', pch = 19, col = 2)
lines(share_con ~ decade, data = toplot[is.na(farmer)][order(decade)], 
    type = 'b', pch = 19, col = "gray")
legend("bottomright", fill = c(1, "gray", 2), legend = c("farmer", "no occ", "other"))
dev.off()

# by first observed opgaafrol location
out = cube(saf_long[!is.na(individual_id_wife) & !is.na(couple_id) & !is.na(firstrol) & gen >= 3 & between(sy, 1750, 1820)],
    j = lapply(.SD * 100, mean),
    by = "firstrol",
    .SDcols = c("con", "fbd", "fsd", "mbd", "msd"))
knitr::kable(out[order(con)], digits = 1)

# by most frequent observed opgaafrol location
out = cube(saf_long[!is.na(individual_id_wife) & !is.na(couple_id) & !is.na(freqrol) & gen >= 3 & between(sy, 1750, 1820)],
    j = lapply(.SD * 100, mean),
    by = "freqrol",
    .SDcols = c("con", "fbd", "fsd", "mbd", "msd"))
knitr::kable(out[order(con)], digits = 1)

# also with time
# first con with without firstrol missing
toplot = cube(saf_long[!is.na(firstrol) & !is.na(individual_id_wife) & !is.na(couple_id) & !is.na(firstrol) & gen >= 3 & between(sy, 1750, 1820)],
    j = list(share_con = mean(con), se = sd(con) / sqrt(.N), N = .N),
    by = c("period", "firstrol"))
toplot[, hi := share_con + se]
toplot[, lo := share_con - se]
toplot[is.na(firstrol), firstrol := "all"]

outplot = ggplot(toplot[!is.na(period) & N > 42], aes(period, share_con, col = firstrol)) + 
    geom_point() + geom_line() + theme_classic()
pdf("~/repos/saf2opg/out/region_overtime.pdf", height = 6)
print(outplot)
dev.off()

# by family line
saf_long[, lineage := stringi::stri_replace_all_regex(individual_id, "a\\d.*", "")]

out = saf_long[!is.na(individual_id_wife) & !is.na(couple_id) & gen >= 3 & between(sy, 1750, 1820),
    list(.N, pctcon = mean(con * 100), pca = mean(pca, na.rm = TRUE)), 
    by = "lineage"]
out
knitr::kable(
    list(out[order(-pctcon)][N > 50][1:11],
        out[order(-pctcon)][N > 50][12:22]), 
    digits = 1)
knitr::kable(
    list(out[order(-N)][1:11],
        out[order(-N)][12:22]), 
    digits = 1)

yl = out[!is.na(pca) & N > 10, range(pca)]
pdf("~/repos/saf2opg/out/families.pdf", height = 4, width = 8)
mypar(mfrow = c(1,3))
plot(pca ~ pctcon, data = out[N > 100 & !is.na(pca)], pch = 19, ylim = yl,
    xlab = "% con. marriage", ylab = "wealth PCA", main = "N marriages > 100")
m100 = lm(pca ~ pctcon, data = out[N > 100 & !is.na(pca)], weights = N)
abline(m100)
plot(pca ~ pctcon, data = out[N > 50 & !is.na(pca)], pch = 19, ylim = yl,
    xlab = "% con. marriage", ylab = "wealth PCA", main = "N marriages > 50")
m50 = lm(pca ~ pctcon, data = out[N > 50 & !is.na(pca)], weights = N)
abline(m50)
plot(pca ~ pctcon, data = out[N > 10 & !is.na(pca)], pch = 19, ylim = yl,
    xlab = "% con. marriage", ylab = "wealth PCA", main = "N marriages > 10")
m10 = lm(pca ~ pctcon, data = out[N > 10 & !is.na(pca)], weights = N)
abline(m10)
dev.off()

texreg::screenreg(list(m100, m50, m10))

pdf("~/repos/saf2opg/out/distributions.pdf", width = 9)
par(mfrow = c(2, 3))
plot(density(saf_long[parent_of_con == FALSE & !is.na(pca), pca]), main = "parents of CM")
lines(density(saf_long[parent_of_con == TRUE & !is.na(pca), pca]), col = 2)
plot(density(saf_long[con == FALSE & !is.na(pca), pca]), main = "CM")
lines(density(saf_long[con == TRUE & !is.na(pca), pca]), col = 2)
plot(density(saf_long[child_of_con == FALSE & !is.na(pca), pca]), main = "Children of CM")
lines(density(saf_long[child_of_con == TRUE & !is.na(pca), pca]), col = 2)

plot(density(saf_long[parent_of_con == TRUE & !is.na(pca), log(pca + 1)]), col = 2, main = "parents of CM (log scale)")
lines(density(saf_long[parent_of_con == FALSE & !is.na(pca), log(pca + 1)]))
plot(density(saf_long[con == FALSE & !is.na(pca), log(pca + 1)]), main = "CM (log scale)")
lines(density(saf_long[con == TRUE & !is.na(pca), log(pca + 1)]), col = 2)
plot(density(saf_long[child_of_con == FALSE & !is.na(pca), log(pca + 1)]), main = "Children of CM (log scale)")
lines(density(saf_long[child_of_con == TRUE & !is.na(pca), log(pca + 1)]), col = 2)
dev.off()

# gradients

toplot_parent = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820), list(mean(parent_of_con, na.rm = TRUE), .N), by = list(pcaq, pcaqn)][order(pcaq)]
toplot_couple = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820), list(mean(con, na.rm = TRUE), .N), by = list(pcaq, pcaqn)][order(pcaq)]
toplot_childr = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820), list(mean(child_of_con, na.rm = TRUE), .N), by = list(pcaq, pcaqn)][order(pcaq)]

pdf("~/repos/saf2opg/out/consan_parents_opg.pdf", height = 6)
mypar()
plot(V1 ~ pcaqn, data = toplot_parent, 
    main = "Parents", xlab = "asset PCA quantile", ylab = "share cons.",
    col = 2, type = "b", pch = 19, ylim = c(0, 0.21), axes = FALSE)
axis(1, at = 1:4, labels = toplot_parent[!is.na(pcaq), unique(pcaq)])
axis(2)
dev.off()
pdf("~/repos/saf2opg/out/consan_couples_opg.pdf", height = 6)
mypar()
plot(V1 ~ pcaqn, data = toplot_couple, 
    main = "Couple", xlab = "asset PCA quantile", ylab = "share cons.",
    col = 2, type = "b", pch = 19, ylim = c(0, 0.21), axes = FALSE)
axis(1, at = 1:4, labels = toplot_parent[!is.na(pcaq), unique(pcaq)])
axis(2)
dev.off()
pdf("~/repos/saf2opg/out/consan_children_opg.pdf", height = 6)
mypar()
plot(V1 ~ pcaqn, data = toplot_childr, 
    main = "Offspring", xlab = "asset PCA quantile", ylab = "share cons.",
    col = 2, type = "b", pch = 19, ylim = c(0, 0.21), axes = FALSE)
axis(1, at = 1:4, labels = toplot_parent[!is.na(pcaq), unique(pcaq)])
axis(2)
dev.off()

# split these by district (stel rein first) and slave/livestock
pdf("~/repos/saf2opg/out/gradient_byregion.pdf", width = 9, height = 4)
mypar(mfrow = c(1, 3))
toplot_parent = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820) & firstrol %in% c("rein", "stel"), list(mean(parent_of_con, na.rm = TRUE), .N), by = list(firstrol, pcaq_reg, pcaq_reg_n)][order(pcaq_reg)]
toplot_parent = dcast(toplot_parent[!is.na(firstrol) & !is.na(pcaq_reg_n)], pcaq_reg_n ~ firstrol, value.var = "V1")
matplot(toplot_parent[, 1], toplot_parent[, 2:3],
    xlab = "asset PCA quantile", ylab = "share cons.", main = "Parents",
    type = "b", pch = 19, lty = 1,
    ylim = c(0, 0.28))
legend("topleft", fill = 1:2, legend = colnames(toplot_parent)[2:3])

toplot_couple = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820) & firstrol %in% c("rein", "stel"), list(mean(con, na.rm = TRUE), .N), by = list(firstrol, pcaq_reg, pcaq_reg_n)][order(pcaq_reg)]
toplot_couple = dcast(toplot_couple[!is.na(firstrol) & !is.na(pcaq_reg_n)], pcaq_reg_n ~ firstrol, value.var = "V1")
matplot(toplot_couple[, 1], toplot_couple[, 2:3],
    xlab = "asset PCA quantile", ylab = "share cons.", main = "Couple",
    type = "b", pch = 19, lty = 1,
    ylim = c(0, 0.28))
legend("topleft", fill = 1:2, legend = colnames(toplot_couple)[2:3])

toplot_childr = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820) & firstrol %in% c("rein", "stel"), list(mean(child_of_con, na.rm = TRUE), .N), by = list(firstrol, pcaq_reg, pcaq_reg_n)][order(pcaq_reg)]
toplot_childr = dcast(toplot_childr[!is.na(firstrol) & !is.na(pcaq_reg_n)], pcaq_reg_n ~ firstrol, value.var = "V1")
matplot(toplot_childr[, 1], toplot_childr[, 2:3],
    xlab = "asset PCA quantile", ylab = "share cons.", main = "Offspring",
    type = "b", pch = 19, lty = 1,
    ylim = c(0, 0.28))
legend("topleft", fill = 1:2, legend = colnames(toplot_childr)[2:3])
dev.off()


# by specific assets
pdf("~/repos/saf2opg/out/assets_by_region.pdf", width = 9)
mypar(mfrow = c(2, 2))
toplot_parent = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820) & firstrol %in% c("rein", "stel"), 
    list(mean(parent_of_con), .N), by = list(slavesq_reg_n, firstrol)]
toplot_parent = dcast(toplot_parent[!is.na(firstrol) & !is.na(slavesq_reg_n)], slavesq_reg_n ~ firstrol, value.var = "V1")
matplot(toplot_parent[, 1], toplot_parent[, 2:3],
    xlab = "slaves q", ylab = "share cons.", main = "Parents, slaveholding",
    type = "b", pch = 19, lty = 1, axes = TRUE,
    ylim = c(0, 0.27))
legend("topleft", fill = 1:2, legend = colnames(toplot_parent)[2:3])

toplot_parent = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820) & firstrol %in% c("rein", "stel"), 
    list(mean(parent_of_con), .N), by = list(cattleq_reg_n, firstrol)]
toplot_parent = dcast(toplot_parent[!is.na(firstrol) & !is.na(cattleq_reg_n)], cattleq_reg_n ~ firstrol, value.var = "V1")
matplot(toplot_parent[, 1], toplot_parent[, 2:3],
    xlab = "cattle q.", ylab = "share cons.", main = "Parents, cattle",
    type = "b", pch = 19, lty = 1, axes = TRUE,
    ylim = c(0, 0.27))
legend("topleft", fill = 1:2, legend = colnames(toplot_parent)[2:3])

toplot_couple = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820) & firstrol %in% c("rein", "stel"), 
    list(mean(con), .N), by = list(slavesq_reg_n, firstrol)]
toplot_couple = dcast(toplot_couple[!is.na(firstrol) & !is.na(slavesq_reg_n)], slavesq_reg_n ~ firstrol, value.var = "V1")
matplot(toplot_couple[, 1], toplot_couple[, 2:3],
    xlab = "slaves q", ylab = "share cons.", main = "Couple, slaveholding",
    type = "b", pch = 19, lty = 1, axes = TRUE,
    ylim = c(0, 0.27))
legend("topleft", fill = 1:2, legend = colnames(toplot_couple)[2:3])

toplot_couple = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820) & firstrol %in% c("rein", "stel"), 
    list(mean(con), .N), by = list(cattleq_reg_n, firstrol)]
toplot_couple = dcast(toplot_couple[!is.na(firstrol) & !is.na(cattleq_reg_n)], cattleq_reg_n ~ firstrol, value.var = "V1")
matplot(toplot_couple[, 1], toplot_couple[, 2:3],
    xlab = "cattle q.", ylab = "share cons.", main = "Couple, cattle",
    type = "b", pch = 19, lty = 1, axes = TRUE,
    ylim = c(0, 0.27))
legend("topleft", fill = 1:2, legend = colnames(toplot_parent)[2:3])
dev.off()

toplot_parent = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820), list(mean(parent_of_con, na.rm = TRUE), .N), by = list(pcaq_top, pcaq_top_n)][order(pcaq_top)]
toplot_couple = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820), list(mean(con, na.rm = TRUE), .N), by = list(pcaq_top, pcaq_top_n)][order(pcaq_top)]
toplot_childr = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820), list(mean(child_of_con, na.rm = TRUE), .N), by = list(pcaq_top, pcaq_top_n)][order(pcaq_top)]

pdf("~/repos/saf2opg/out/gradient_top.pdf", height = 4, width = 9)
mypar(mfrow = c(1, 3))
plot(V1 ~ pcaq_top_n, data = toplot_parent, 
    main = "Parents", xlab = "asset PCA quantile", ylab = "share cons.",
    col = 2, type = "b", pch = 19, ylim = c(0, 0.25), axes = FALSE)
axis(1, at = toplot_parent$pcaq_top_n, labels = toplot_parent$pcaq_top)
axis(2)
plot(V1 ~ pcaq_top_n, data = toplot_couple, 
    main = "Couple", xlab = "asset PCA quantile", ylab = "share cons.",
    col = 2, type = "b", pch = 19, ylim = c(0, 0.25), axes = FALSE)
axis(1, at = toplot_couple$pcaq_top_n, labels = toplot_couple$pcaq_top)
axis(2)
plot(V1 ~ pcaq_top_n, data = toplot_childr, 
    main = "Offspring", xlab = "asset PCA quantile", ylab = "share cons.",
    col = 2, type = "b", pch = 19, ylim = c(0, 0.25), axes = FALSE)
axis(1, at = toplot_childr$pcaq_top_n, labels = toplot_childr$pcaq_top)
axis(2)
dev.off()

# regression dataset
# take saf_long
# add nsibling
# add mean age of observation
# merge paternal pca
# merge children's pca? or should that be grandpaternal pca?

dim(saf_long)
saf_long = merge(
    saf_long, 
    siblings[, list(nsib = .N), by = list(individual_id = individual_id.x)], 
    by = "individual_id", 
    all.x = TRUE)
dim(saf_long)
# add nchild
saf_long = merge(
    saf_long,
    parentchild[, list(nchild = .N), by = parent],
    by.x = "individual_id",
    by.y = "parent",
    all.x = TRUE)
# add parent_id
saf_long = merge(
    saf_long, 
    parentchild, 
    by.x = "individual_id", 
    by.y = "child", 
    all.x = TRUE)
dim(saf_long)
# add uncles
saf_long = merge(
    saf_long, 
    siblings[, list(nuncles = .N), by = list(individual_id = individual_id.x)], , 
    by.x = "parent", 
    by.y = "individual_id", 
    all.x = TRUE)
dim(saf_long)
# add grandparentid
saf_long = merge(
    saf_long, 
    parentchild, 
    by.x = "parent", 
    by.y = "child", 
    all.x = TRUE,
    suffixes = c("", "_parent"))
dim(saf_long)
# add "children is "

# and now those awful self-merges... note the averaging over individuals, you
# should actually do the merges on the couple id, which requires getting the
# parent child relations on the couple id in the first place which could be tricky
saf_long = merge(
    saf_long,
    unique(saf_long[, list(pca_parent = mean(pca, na.rm = TRUE)), by = list(individual_id)]),
    by.x = "parent",
    by.y = "individual_id",
    all.x = TRUE, all.y = FALSE)
saf_long = merge(
    saf_long,
    unique(saf_long[, list(pca_grandparent = mean(pca, na.rm = TRUE)), by = list(individual_id)]),
    by.x = "parent_parent",
    by.y = "individual_id",
    all.x = TRUE, all.y = FALSE)
# add child is consanguineous
saf_long = merge(saf_long,
    unique(consan[, list(grandparent_id, child_consan = TRUE)]),
    by.x = "individual_id",
    by.y = "grandparent_id",
    all.x = TRUE)
saf_long[]

saf_long[, decaded := floor(year / 10) * 10]
saf_long[, mean_age := year - sy]
dict = c(
    conTRUE = "cons. marriage",
    parent_of_conTRUE = "parent cons. marriage",
    child_of_conTRUE = "child cons. marriage",
    nsib = "n. siblings",
    nuncles = "n. uncle/aunts",
    mean_age = "mean age",
    pca = "asset index",
    decaded = "decade",
    firstrol = "district")

toreg = saf_long[!is.na(individual_id_wife) & gen >= 3 & between(sy, 1750, 1820)]
toreg = saf_long[!is.na(individual_id_wife) & gen >= 3]
toreg = saf_long[!is.na(individual_id_wife)]
toreg = saf_long
toreg = saf_long[gen >= 3]
toreg = saf_long[!is.na(individual_id_wife) & gen >= 3]
hist(toreg$sy)

# crucial here is that we include marriages where gen is 1 or 2, which means that we only know their pedigree 1-2 generations deep. Issue is of course that we can then never know whether they were con, so there the con=FALSE must include some con = TRUE. If con > notcon, then this lowers the gap; if con = notcon it doesn't matter?; if con < notcon, then it blows up the gap but we'd expect it to be negative which it isn't
mlist = list(
    fixest::feols(log(pca + 1) ~ con, data = toreg),
    fixest::feols(log(pca + 1) ~ con + nsib, data = toreg),
    fixest::feols(log(pca + 1) ~ con + nsib + nuncles, data = toreg),
    fixest::feols(log(pca + 1) ~ con + nsib + nuncles + mean_age + I(mean_age^2), data = toreg),
    fixest::feols(log(pca + 1) ~ con + nsib + nuncles + mean_age + I(mean_age^2) | decaded, data = toreg),
    fixest::feols(log(pca + 1) ~ con + nsib + nuncles + mean_age + I(mean_age^2) | decaded + firstrol, data = toreg, vcov = "twoway"),
    fixest::feols(log(pca + 1) ~ con + nsib + nuncles + mean_age + I(mean_age^2) + log(pca_parent + 1) | decaded + firstrol, data = toreg, vcov = "twoway")
)
ests = etable(mlist, dict = dict, digits = 3)
write.csv(ests, "~/repos/saf2opg/out/reg_couple.csv")

etable(mlist, 

# look siblings, these are siblings of parents, (in addition )we want their nubmer of children 
# do this everywhere
mlist = list(
    fixest::feols(log(pca + 1) ~ parent_of_con, data = toreg),
    fixest::feols(log(pca + 1) ~ parent_of_con + nsib, data = toreg),
    fixest::feols(log(pca + 1) ~ parent_of_con + nchild + nsib, data = toreg),
    fixest::feols(log(pca + 1) ~ parent_of_con + nchild + nsib + mean_age + I(mean_age^2), data = toreg),
    fixest::feols(log(pca + 1) ~ parent_of_con + nchild + nsib + mean_age + I(mean_age^2) | decaded, data = toreg),
    fixest::feols(log(pca + 1) ~ parent_of_con + nchild + nsib + mean_age + I(mean_age^2) | decaded + firstrol, data = toreg, vcov = "twoway")
)
ests = etable(mlist, dict = dict, digits = 3)
write.csv(ests, "~/repos/saf2opg/out/reg_parents.csv")
# out = knitr::kable(ests, format = "html")
# writeLines(out, "~/repos/saf2opg/out/reg_parents.html")

mlist = list(
    fixest::feols(log(pca + 1) ~ child_of_con, data = toreg),
    fixest::feols(log(pca + 1) ~ child_of_con + nsib, data = toreg),
    fixest::feols(log(pca + 1) ~ child_of_con + nsib + nuncles, data = toreg),
    fixest::feols(log(pca + 1) ~ child_of_con + nsib + nuncles + mean_age + I(mean_age^2), data = toreg),
    fixest::feols(log(pca + 1) ~ child_of_con + nsib + nuncles + mean_age + I(mean_age^2) | decaded, data = toreg),
    fixest::feols(log(pca + 1) ~ child_of_con + nsib + nuncles + mean_age + I(mean_age^2) | decaded + firstrol, data = toreg, vcov = "twoway"),
    fixest::feols(log(pca + 1) ~ child_of_con + nsib + nuncles + mean_age + I(mean_age^2) + log(pca_parent + 1) | decaded + firstrol, data = toreg, vcov = "twoway")
)
ests = etable(mlist, dict = dict, digits = 3)
write.csv(ests, "~/repos/saf2opg/out/reg_offspring.csv")

mlist = list(
    all = fixest::feols(log(pca + 1) ~ con + nsib + nuncles + mean_age + I(mean_age^2) + log(pca_parent + 1) | decaded + firstrol, data = toreg, vcov = "twoway"),
    rein = fixest::feols(log(pca + 1) ~ con + nsib + nuncles + mean_age + I(mean_age^2) + log(pca_parent + 1) | decaded, data = toreg[firstrol == "rein"], vcov = "twoway"),
    stel = fixest::feols(log(pca + 1) ~ con + nsib + nuncles + mean_age + I(mean_age^2) + log(pca_parent + 1) | decaded, data = toreg[firstrol == "stel"], vcov = "twoway"),
    rein = fixest::feols(log(cattle + 1) ~ con + nsib + nuncles + mean_age + I(mean_age^2) + log(pca_parent + 1) | decaded, data = toreg[firstrol == "rein"], vcov = "twoway"),
    stel = fixest::feols(log(slaves + 1) ~ con + nsib + nuncles + mean_age + I(mean_age^2) + log(pca_parent + 1) | decaded, data = toreg[firstrol == "stel"], vcov = "twoway")
)
ests = etable(mlist, dict = dict, digits = 3)
write.csv(ests, "~/repos/saf2opg/out/reg_couples_split.csv")

# clean up by focusing on two regions
# gradients by region-specific quartiles


# intergeneration picture

# merge child id into consan marriages
intergen = merge(
    x = saf_long,
    y = unique(consan[child_id != "", list(child_id, husband_id)]),
    by.x = "individual_id",
    by.y = "husband_id",
    all = TRUE,
    allow.cartesian = TRUE)
# merge grandparent into consan marriages
intergen = merge(
    x = intergen,
    y = unique(consan[grandparent_id != "", list(grandparent_id, husband_id)]),
    by.x = "individual_id",
    by.y = "husband_id",
    all = TRUE)

# opgaafrol data aggregated at individual_id
tomerge = merge(
    opg[, cbind(rol, persid, index, mfirst, mlast, wfirst, wlast, .SD, pca, reapbarley, reapwheat), .SDcols = vrbs],
    saf2opg, 
    by = "persid")
tomerge = tomerge[, 
    lapply(.SD, mean, na.rm = TRUE), 
    .SDcols = c(vrbs, "pca", "reapwheat", "reapbarley"), 
    by = list(individual_id)]

intergen = merge(
    intergen, 
    tomerge[, list(individual_id, pca_child = pca)],
    by.x = "child_id",
    by.y = "individual_id",
    all = TRUE)
intergen = merge(
    intergen, 
    tomerge[, list(individual_id, pca_grandparent = pca)],
    by.x = "grandparent_id",
    by.y = "individual_id",
    all = TRUE)

intergen[, pca := pca + 1]
intergen[, pca_grandparent := pca_grandparent + 1]
intergen[, pca_child := pca_child + 1]

toplot = merge(
    parentchild,
    tomerge[, list(individual_id, parent_pca = pca + 1)],
    by.x = "parent",
    by.y = "individual_id",
    all = TRUE)
toplot = merge(
    toplot,
    tomerge[, list(individual_id, child_pca = pca + 1)],
    by.x = "child",
    by.y = "individual_id",
    all = TRUE)
toplot[!is.na(parent_pca)]
toplot[!is.na(pca)]
toplot[!is.na(pca_child)]
toplot[!is.na(pca) & !is.na(pca_child)]
toplot[!is.na(pca) & !is.na(pca_grandparent)]
# so you have 2000, of which 10% is cm, so expect 200, so 160 is not a crazy number here

yl = range(toplot$parent_pca, na.rm = TRUE)
plot(log(child_pca) ~ log(parent_pca), data = toplot, ylim = log(yl), col = "gray70")
curve(1*x, add = TRUE)
m_all = lm(log(child_pca) ~ log(parent_pca), data = toplot)
abline(m_all, col = "gray70")

points(log(pca) ~ log(pca_grandparent), data = intergen, col = 2, pch = 19)
m_cp = lm(log(pca) ~ log(pca_grandparent), data = intergen)
abline(m_cp, col = 2)

points(log(pca_child) ~ log(pca), data = intergen, col = 4, pch = 19)
m_cc = lm(log(pca_child) ~ log(pca), data = intergen)
abline(m_cc, col = 4)
texreg::screenreg(list(m_all, m_cp, m_cc))
