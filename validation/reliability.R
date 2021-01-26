rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(psych) # tr()
library(epmr) # faststudy

setwd(sprintf("~%s/validation", setpath))
dm1 <- haven::read_sav("RAMP_DM_1st.sav")
dm2 <- haven::read_sav("RAMP_DM_2nd.sav")
ht1 <- haven::read_sav("RAMP-HT 1st.sav")
ht2 <- haven::read_sav("RAMP-HT 2nd.sav")
wc1 <- haven::read_sav("WC 1st.sav")
wc2 <- haven::read_sav("WC 2nd.sav")
cc1 <- haven::read_sav("CC 1st.sav")
cc2 <- haven::read_sav("CC 2nd.sav")

names(dm2)[names(dm2) %in% names(select(dm2, ends_with("_R")))] <- tolower(names(select(dm2, ends_with("_R"))))
names(ht2)[names(ht2) %in% names(select(ht2, ends_with("_R")))] <- tolower(names(select(ht2, ends_with("_R"))))
names(wc2)[names(wc2) %in% names(select(wc2, ends_with("_R")))] <- tolower(names(select(wc2, ends_with("_R"))))
names(cc2)[names(cc2) %in% names(select(cc2, ends_with("_R")))] <- tolower(names(select(cc2, ends_with("_R"))))
dm12 <- plyr::rbind.fill(dm1, dm2)
ht12 <- plyr::rbind.fill(ht1, ht2)
wc12 <- plyr::rbind.fill(wc1, wc2)
cc12 <- plyr::rbind.fill(cc1, cc2)

# descriptive statistics ----


# DM ----
dm12 %>% 
  select(dm3, dm4, dm5, dm8) %>%
  alpha(title = "Accessibility", check.keys=TRUE) %>% .[c("title", "total")]

dm12 %>% 
  select(dm9, dm10_r, dm11_r, dm12, dm13, dm14_r, dm15_r, dm16, dm17, dm18, dm29, dm30_r, dm44, dm45) %>%
  alpha(title = "Communication", check.keys=TRUE) %>% .[c("title", "total")]

dm12 %>% 
  select(dm19_r, dm20_r, dm21_r, dm22_r, dm24, dm25, dm26, dm27, dm28_r, dm31, dm33, dm35, dm36, dm38, dm39_r) %>%
  alpha(title = "Quality of Care", check.keys=TRUE) %>% .[c("title", "total")]

dm12 %>% 
  select(dm40, dm41, dm42, dm43_r) %>%
  alpha(title = "General'", check.keys=TRUE) %>% .[c("title", "total")]

dm12 %>% 
  select(dm3, dm4, dm5, dm8, 
         dm9, dm10_r, dm11_r, dm12, dm13, dm14_r, dm15_r, dm16, dm17, dm18, 
         dm19_r, dm20_r, dm21_r, dm22_r, 
         # dm23_r, 
         dm24, dm25, dm26, dm27, dm28_r, dm29, dm30_r, dm31, 
         # dm32_r, dm34_r,
         dm33, dm35, dm36, 
         # dm37_r, 
         dm38, dm39_r, dm40, 
         dm41, dm42, dm43_r, dm44, dm45, dm46_r
         ) %>% 
  # cor(, use = "pairwise") # check correlation matrix for problems
  alpha(title = "DM12"
               , check.keys=TRUE
               , use = "pairwise.complete.obs"
               )# %>% .[c("title", "total")]

# HT ----
ht12 %>% 
  select(ht3, ht4, ht5, ht8) %>%
  alpha(title = "Accessibility", check.keys=TRUE) %>% .[c("title", "total")]

ht12 %>% 
  select(ht9, ht10_r, ht11_r, ht12, ht13, ht14_r, ht15_r, ht16, ht17, ht18, ht29, ht30_r, ht44, ht45) %>%
  alpha(title = "Communication", check.keys=TRUE) %>% .[c("title", "total")]

ht12 %>% 
  select(ht19_r, ht20_r, ht21_r, ht22_r, ht24, ht25, ht26, ht27, ht28_r, ht31, ht33, ht35, ht36, ht38, ht39_r) %>%
  alpha(title = "Quality of Care", check.keys=TRUE) %>% .[c("title", "total")]

ht12 %>% 
  select(ht40, ht41, ht42, ht43_r) %>%
  alpha(title = "General'", check.keys=TRUE) %>% .[c("title", "total")]

ht12 %>% 
  select(ht3, ht4, ht5, ht8, 
         ht9, ht10_r, ht11_r, ht12, ht13, ht14_r, ht15_r, ht16, ht17, ht18, 
         ht19_r, ht20_r, 
         ht21_r, ht22_r, 
         # ht23_r, 
         ht24, ht25, ht26, ht27, ht28_r, ht29, ht30_r, ht31, 
         # ht32_r, ht34_r,
         ht33, ht35, ht36, 
         # ht37_r, 
         ht38, ht39_r, ht40, 
         ht41, ht42, ht43_r, ht44, ht45, ht46_r
  ) %>% 
  # cor(, use = "pairwise") # check correlation matrix for problems
  alpha(title = "ht12"
        , check.keys=TRUE
        , use = "pairwise.complete.obs"
  ) %>% .[c("title", "total")]

# WC ----
wc12 %>% 
  select(wc5, wc6, wc7, wc10) %>%
  alpha(title = "Accessibility", check.keys=TRUE) %>% .[c("title", "total")]

wc12 %>% 
  select(wc11, wc12_r, wc13_r, wc14, wc15, wc16_r, wc17_r, wc18, wc19, wc20, wc28, wc29_r, wc39, wc40) %>% 
  alpha(title = "Communication", check.keys=TRUE) %>% .[c("title", "total")]

wc12 %>% 
  select(wc21_r, wc23,	wc24,	wc25,	wc26, wc27_r, wc30, wc32, wc33_r) %>% 
  alpha(title = "Quality of Care", check.keys=TRUE) %>% .[c("title", "total")]

wc12 %>% 
  select(wc34, wc35, wc36, wc37, wc38_r) %>%
  alpha(title = "General'", check.keys=TRUE) %>% .[c("title", "total")]

wc12 %>% 
  select(wc5, wc6, wc7, wc10, 
         wc11, wc12_r, wc13_r, wc14, wc15, wc16_r, wc17_r, wc18, wc19, wc20, 
         wc21_r, 
         # wc22_r, 
         wc23,	wc24,	wc25,	wc26, wc27_r, 
         wc28, wc29_r, 
         # wc31_r,
         wc30, wc32, wc33_r,
         wc34, wc35, wc36, wc37, wc38_r,
         wc39, wc40, wc41_r
  ) %>% 
  # cor(, use = "pairwise") # check correlation matrix for problems
  alpha(title = "wc2"
        , check.keys=TRUE
        , use = "pairwise.complete.obs"
  ) %>% .[c("title", "total")]

# cc ----
cc12 %>% 
  select(cc5, cc6, cc7, cc10) %>%
  alpha(title = "Accessibility", check.keys=TRUE) %>% .[c("title", "total")]

cc12 %>% 
  select(cc11, cc12_r, cc13_r, cc14, cc15, cc16_r, cc17_r, cc18, cc19, cc20, cc28, cc29_r, cc39, cc40) %>% 
  alpha(title = "Communication", check.keys=TRUE) %>% .[c("title", "total")]

cc12 %>% 
  select(cc21_r, cc23,	cc24,	cc25,	cc26, cc27_r, cc30, cc32, cc33_r) %>% 
  alpha(title = "Quality of Care", check.keys=TRUE) %>% .[c("title", "total")]

cc12 %>% 
  select(cc34, cc35, cc36, cc37, cc38_r) %>%
  alpha(title = "General'", check.keys=TRUE) %>% .[c("title", "total")]

cc12 %>% 
  select(cc5, cc6, cc7, cc10, 
         cc11, cc12_r, cc13_r, cc14, cc15, cc16_r, cc17_r, cc18, cc19, cc20, 
         cc21_r, 
         # cc22_r, 
         cc23,	cc24,	cc25,	cc26, cc27_r, 
         cc28, cc29_r, 
         # cc31_r,
         cc30, cc32, cc33_r,
         cc34, cc35, cc36, cc37, cc38_r,
         cc39, cc40, cc41_r
  ) %>% 
  # cor(, use = "pairwise") # check correlation matrix for problems
  alpha(title = "cc2"
        , check.keys=TRUE
        , use = "pairwise.complete.obs"
  ) %>% .[c("title", "total")]

# factor analysis ----
normalize <- function(x){
  return((x- min(x, na.rm = TRUE)) /(max(x,na.rm = TRUE)-min(x,na.rm = TRUE)))
}
cfa_dm12 <- fastudy(dm12 %>% 
                      select(dm3, dm4, dm5, dm8, 
                             dm9, dm10_r, dm11_r, dm12, dm13, dm14_r, dm15_r, dm16, dm17, dm18, 
                             dm19_r, dm20_r, 
                             dm21_r, dm22_r, 
                             # dm23_r, 
                             dm24, dm25, dm26, dm27, dm28_r, dm29, dm30_r, dm31, 
                             # dm32_r, dm34_r,
                             dm33, dm35, dm36, 
                             # dm37_r, 
                             dm38, dm39_r, dm40, 
                             dm41, dm42, dm43_r, dm44, dm45, dm46_r
                      ) 
                      , factors = 6)
plot(cfa_dm12, ylim = c(0, 4))
print(cfa_dm12, digits = 2, cutoff = 0.25)

cfa_wc12 <- fastudy(wc12 %>% 
                      select(wc5, wc6, wc7, wc10, 
                             wc11, wc12_r, wc13_r, wc14, wc15, wc16_r, wc17_r, wc18, wc19, wc20, 
                             wc21_r, 
                             # wc22_r, 
                             wc23,	wc24,	wc25,	wc26, wc27_r, 
                             wc28, wc29_r, 
                             # wc31_r,
                             wc30, wc32, wc33_r,
                             wc34, wc35, wc36, wc37, wc38_r,
                             wc39, wc40, wc41_r
                      ) 
                    , factors = 6)
plot(cfa_wc12, ylim = c(0, 4))
print(cfa_wc12, digits = 2, cutoff = 0.25)

cfa_cc12 <- fastudy(cc12 %>% 
                      select(cc5, cc6, cc7, cc10, 
                             cc11, cc12_r, cc13_r, cc14, cc15, cc16_r, cc17_r, cc18, cc19, cc20, 
                             cc21_r, 
                             # cc22_r, 
                             cc23,	cc24,	cc25,	cc26, cc27_r, 
                             cc28, cc29_r, 
                             # cc31_r,
                             cc30, cc32, cc33_r,
                             cc34, cc35, cc36, cc37, cc38_r,
                             cc39, cc40, cc41_r
                      ) 
                    , factors = 4)
plot(cfa_cc12, ylim = c(0, 4))
print(cfa_cc12, digits = 2, cutoff = 0.10)



# Alpha adjusted for NA values ----
# reference - https://www.researchgate.net/publication/328567140_Calculating_the_Cronbach's_alpha_coefficients_for_measurement_scales_with_not_applicable_option
c_alpha = function(data, digits = 3) {
  # raw scale score
  raw_scale = rowSums(data, na.rm = T)
  # k = number of items in a scale
  k = dim(data)[2]
  # adjusted scale score, scaled-up to the number of items in a scale
  adjusted_scale = k * (raw_scale / rowSums(!is.na(data)))
  # variances
  item_variance = mapply(var, data, na.rm = T)
  scale_variance = var(adjusted_scale, na.rm = T)
  6
  # alpha
  alpha = k / (k - 1) * (1 - sum(item_variance, na.rm = T)/scale_variance)
  # additional output:
  # alpha if item removed; item-scale, item-rest, and average inter-item
  # correlations
  alpha_r = itc = irc = aic = aic_n = rep(0, k)
  itc = cor(data, adjusted_scale, use = "pairwise.complete.obs")
  n_cor = mapply(function(x) length(na.omit(x)), data)
  for(i in 1:k) {
    raw_scale_r = rowSums(data[-i], na.rm = T)
    adjust_r = k_r = dim(data[-i])[2]
    adjusted_scale_r = adjust_r * (raw_scale_r / rowSums(!is.na(data[-i])))
    item_variance_r = mapply(var, data[-i], na.rm = T)
    scale_variance_r = var(adjusted_scale_r, na.rm = T)
    alpha_r[i] = k_r / (k_r - 1) *
      (1 - sum(item_variance_r, na.rm = T)/scale_variance_r)
    irc[i] = cor(data[i], adjusted_scale_r, use = "pairwise.complete.obs")
    aic[i] = mean(cor(data[i], data[-i],
                      use = "pairwise.complete.obs"), na.rm = T)
    aic_n[i] = rowSums(!is.na(cor(data[i], data[-i],
                                  use = "pairwise.complete.obs")))
  }
  # output
  cat(c("Cronbach's alpha = ", round(alpha, digits), "\n\n"), sep = "")
  matrix(round(c(alpha_r, itc, irc, n_cor, aic, aic_n), digits),
         nrow = length(alpha_r), ncol = 6,
         dimnames = list(names(data),
                         c("Alpha w/o item", "Item-scale cor.", "Item-rest cor.",
                           "n cor.", "Av. item-item cor", "n item")))
}
c_alpha1 = function(data, digits = 3) {
  # raw scale score
  raw_scale = rowSums(data, na.rm = T)
  # k = number of items in a scale
  k = dim(data)[2]
  # adjusted scale score, scaled-up to the number of items in a scale
  adjusted_scale = k * (raw_scale / rowSums(!is.na(data)))
  # variances
  # -- variance-covariance matrix, C
  C = cov(data, use = "pairwise")
  # -- item variances
  item_variance = mapply(var, data, na.rm = T)
  # sum of item variances
  # -- sum(item_variance) == tr(C)
  # variance of scale
  # -- var(raw_scale, na.rm = T) =/= var(adjusted_scale, na.rm = T) =/= sum(C)
  # alpha
  alpha = k / (k - 1) * (1 - tr(C)/sum(C))
  alpha1 = k / (k - 1) * (1 - sum(item_variance)/var(adjusted_scale, na.rm = T))
  alpha2 = k / (k - 1) * (1 - sum(item_variance)/var(raw_scale, na.rm = T))
  # output
  cat(c("Cronbach's alpha (psych) = ", round(alpha, digits), "\n"), sep = "")
  cat(c("Cronbach's alpha (adjusted) = ", round(alpha1, digits), "\n"), sep = "")
  cat(c("Cronbach's alpha (raw) = ", round(alpha2, digits), "\n"), sep = "")
}

