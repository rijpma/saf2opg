# do notes use this for actual linking because it's now in capelinker
# here for some analysis and stats

rm(list = ls())

library("data.table")
library("bit64")
library("capelinker")
library("readxl")
library("stringi")

# training data
alabels = readxl::read_xlsx("~/data/cape/saf/saf2opg_candidates_batch2_auke.xlsx")
jlabels = readxl::read_xlsx("~/data/cape/saf/saf2opg_candidates_batch1_jeanne.xlsx")

setDT(alabels)
setDT(jlabels)

# empty is no link
alabels[is.na(link), link := 0]
jlabels[is.na(link), link := 0]
jlabels[link == 2, link := 1] # typo

# check overlap
first50a = alabels[, unique(couple_id)[1:50]]
first50j = jlabels[, unique(couple_id)[1:50]]
all.equal(first50j, first50a)

# check intercoder reliability
intercoder = merge(
    alabels[couple_id %in% first50j], 
    jlabels[couple_id %in% first50j, list(couple_id, persid, link_j = link, comment_j = comment)], 
    by = c("couple_id", "persid"))
# w/o "based on opg" because j never linked those
writexl::write_xlsx(
    intercoder[link != link_j & (comment != "based on opg" | is.na(comment))],
    "~/data/cape/saf/saf2opg_intercoder.xlsx")


alabels[comment == "based on opg", link := 0] # for now don't use these (singles inferred from opg series)

x = rbindlist(
    list(alabels,
        jlabels[!couple_id %in% first50j]), # avoid doubles
    fill = TRUE)

# linking 248 in labelled, ~50%
x[, uniqueN(couple_id[link == 1])]
x[, uniqueN(couple_id[link == 1]) / uniqueN(couple_id)]

# link about 2000 opg observations
x[, uniqueN(persid[link == 1])]
x[, uniqueN(idx[link == 1])]
# can't say yet if that's a lot

# for compatability with other pretrained models
setnames(x, "link", "correct")

# labelling blocks
x[, uniqueN(couple_id)]

x[, mfirstdist := stringdist::stringdist(firstnames, mfirst, method = "jw")]
x[, mlastdist := stringdist::stringdist(surname, mlast, method = "jw")]

x[, wfirstdist := stringdist::stringdist(spouse_firstnames_clean, wfirst, method = "jw")]
x[, wlastdist := stringdist::stringdist(spouse_surname_clean, wlast, method = "jw")]

# maybe we had this at an earlier stage?
# later merge back into original cleaned files
x[, minitals_saf := initials(firstnames)]
x[, minitals_opg := initials(mfirst)] # def. here
x[, minitialsdist_osa := 1 - stringdist::stringsim(minitals_saf, minitals_opg, method = "osa")]

x[, winitals_saf := initials(spouse_firstnames_clean)]
x[, winitals_opg := initials(wfirst)]
x[, winitialsdist_osa := 1 - stringdist::stringsim(winitals_saf, winitals_opg, method = "osa")]

# add next year dist
# ...

# add surnamedist
x[, cross_surnamedist := stringdist::stringdist(firstnames, wlast, method = "jw")]

# add "name is initials"
longest_string = function(str){
    max(nchar(stringi::stri_extract_all_regex(str, "[a-z]+", simplify = TRUE)))    
} 
x[, wfirst_longest_string :=
    apply(cbind(wfirst), 1, longest_string)]
x[, wfirst_is_initials := wfirst_longest_string == 1]
x[, mfirst_longest_string :=
    apply(cbind(mfirst), 1, longest_string)]
x[, mfirst_is_initials := mfirst_longest_string == 1]

x[, years_married := year - married_year]
# saf_cnd[, maryear_initialsdist_osa := 1 - stringdist::stringsim(as.character(married_year_ego_husb), as.character(married_year_ego_wife), method = "osa")]
# x[, implied_age_gk := gk(year, sy)]

f = formula(correct ~ 
    # maryear_initialsdist_osa +  
    mlastdist + mfirstdist + minitialsdist_osa + 
    # mlastsdx + 
    # mfirstsdx + 
    wlastdist + wfirstdist + winitialsdist_osa + 
    cross_surnamedist + 
    wfirst_is_initials + 
    # mfirst_is_initials + 
    years_married + 
    implied_age
    # wlastsdx + 
    # wfirstsdx + 
    # namefreq_from + 
    # spousenamedist_from + 
    # namefreq_to + 
    # spousenamedist_to + 
    # wifepresent_from + 
    # wifepresent_to + 
    # wifeinboth + 
    # settlerchildrengauss + 
    # nextmfirst + 
    # mfirst_uniqueness_to +
    # mfirst_uniqueness_from +
    # matches + 
    # husb_wife_surnamedist + 
    # region1
    # implied_marriage_age_wife + 
    # implied_marriage_age_husb +
    # spousal_age_gap +
    # myeardiff
)

set.seed(123654) # let's mix it up!
share_train = 0.7
x[, train := couple_id %in% sample(unique(couple_id), ceiling(length(unique(couple_id)) * share_train))]
trn_saf2opg = x[train == 1]
vld_saf2opg = x[train == 0]

# some overfitting going on her
m = xgboost::xgb.train(
    data = capelinker::xgbm_ff(trn_saf2opg, f),
    nrounds = 500,
    watchlist = list(train = xgbm_ff(trn_saf2opg, f), eval = xgbm_ff(vld_saf2opg, f)),
    params = list(
        max_depth = 6,        # default 6
        min_child_weight = 1, # default 1 larger is more consevative
        gamma = 1,            # default 0, larger is more conservative
        eta = 0.3,            # default 0.3 lower for less overfitting
        max_delta_step = 0,   # deafult 0, useful for unbalanced, higher is more conservative
        subsample = 0.8,        # default 1 lower is less overfitting
        colsample_bytree = 0.5, # default 1 
        objective = "binary:logistic"
))

pretrained = list(
    m_boost_saf2opg = list(
        model = m,
        variables = all.vars(f)[-1])
    )

save(pretrained, 
    file = "~/repos/saf2opg/pretrained.rda", 
    version = 2)

impmat = xgboost::xgb.importance(model = m)

predictions = data.table(
    correct = as.logical(vld_saf2opg$correct),
    predicted = predict(m, newdata = xgbm_ff(vld_saf2opg, f)))
conf = table(actual = predictions$correct, predicted = predictions$predicted > 0.5)
conf
predictions[, Metrics::precision(correct, predicted > 0.5)]
predictions[, Metrics::recall(correct, predicted > 0.5)]
predictions[, Metrics::fbeta_score(correct, predicted > 0.5)]

vld_saf2opg[, predicted := predict(m, newdata = xgbm_ff(.SD, f))]
vld_saf2opg[predicted > 0.5, .N, by = implied_age]
vld_saf2opg[predicted > 0.5, .N, by = years_married]

# previously
# 87%
# 81%
# 84%

toplot = predictions[, .N, by = list(actual = correct, predicted = predicted > 0.5)]
steps = seq(0.01, 1, 0.01)
toplot = predictions[, list(
        prec = sapply(steps, function(x) Metrics::precision(correct, predicted = predicted > x)),
        rec = sapply(steps, function(x) Metrics::recall(correct, predicted = predicted > x)),
        fbeta = sapply(steps, function(x) Metrics::fbeta_score(correct, predicted = predicted > x)),
        tr = steps)]

pdf("~/repos/saf2opg/precrec.pdf")
plot(rec ~ prec, data = toplot, type = 'b', pch = 19,
    xlab = "precision", ylab = "recal")
points(rec ~ prec, data = toplot[tr == 0.5], type = 'b', col = 2, pch = 19)
dev.off()

plot(ecdf(predictions$predicted))
predictions[order(predicted), .(predicted)]
predictions[between(predicted, 0.1, 0.9)]
predictions[between(predicted, 0.9, 1)]