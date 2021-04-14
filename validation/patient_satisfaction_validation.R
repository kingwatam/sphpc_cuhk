rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(psych) # tr()

setwd(sprintf("~%s/validation", setpath))
dm1 <- haven::read_sav("RAMP_DM_1st.sav")
dm2 <- haven::read_sav("RAMP_DM_2nd.sav")
ht1 <- haven::read_sav("RAMP-HT 1st.sav")
ht2 <- haven::read_sav("RAMP-HT 2nd.sav")

jmv::reliability(
  data = dm1,
  vars = vars(dm3, dm4, dm5, dm8, dm9, dm10, dm11, dm12, dm13, dm14, dm15, dm16, dm17, dm18, dm19, dm20, dm21, dm22, dm23, dm24, dm25, dm26, dm27, dm28, dm29, dm30, dm31, dm32, dm33, dm34, dm35, dm36, dm37, dm38, dm39, dm40, dm41, dm42, dm43, dm44, dm45, dm46),
  revItems = vars(dm10, dm11, dm14, dm15, dm19, dm20, dm21, dm22, dm23, dm28, dm30, dm32, dm34, dm37, dm39, dm43))

jmv::reliability(
  data = dm1,
  vars = vars(dm3, dm4, dm5, dm8, dm9, dm10, dm11, dm12, dm13, dm14, dm15, dm16, dm17, dm18, dm19, dm20, dm21, dm22, dm24, dm25, dm26, dm27, dm28, dm29, dm30, dm31, dm33, dm35, dm36, dm38, dm39, dm40, dm41, dm42, dm43, dm44, dm45, dm46),
  revItems = vars(dm10, dm11, dm14, dm15, dm19, dm20, dm21, dm22, dm28, dm30, dm39, dm43),
  alphaItems = TRUE,
  # meanItems = TRUE,
  # sdItems = TRUE,
  itemRestCor = TRUE)


dm1 %>% 
  select(dm3, dm4, dm5, dm8, dm9, dm12, 
         dm13, dm16, dm17, dm18, 
         dm24, dm25, dm26, 
         dm27, dm29, dm31, dm33, 
         dm35, dm36, dm38, dm40, 
         dm41, dm42, dm44, dm45, dm46, 
         dm10_r, dm11_r, dm14_r, dm15_r, dm19_r, dm20_r, 
         dm21_r, dm22_r, dm23_r, dm28_r, dm30_r, dm32_r, dm34_r, dm37_r, dm39_r, dm43_r) %>%
  c_alpha1()

dm2 %>% select(ends_with("_R")) %>% colnames(.) <- tolower(dm2 %>% select(ends_with("_R")) %>% colnames(.))


psych::testRetest(dm1 %>% 
                    select(dm3, dm4, dm5, dm8, dm9, dm12, 
                           dm13, dm16, dm17, dm18, 
                           dm24, dm25, dm26, 
                           dm27, dm29, dm31, dm33, 
                           dm35, dm36, dm38, dm40, 
                           dm41, dm42, dm44, dm45, dm46, 
                           dm10_r, dm11_r, dm14_r, dm15_r, dm19_r, dm20_r, 
                           dm21_r, dm22_r, dm23_r, dm28_r, dm30_r, dm32_r, dm34_r, dm37_r, dm39_r, dm43_r),
                  dm2 %>% 
                    select(dm3, dm4, dm5, dm8, dm9, dm12, 
                           dm13, dm16, dm17, dm18, 
                           dm24, dm25, dm26, 
                           dm27, dm29, dm31, dm33, 
                           dm35, dm36, dm38, dm40, 
                           dm41, dm42, dm44, dm45, dm46, 
                           dm10_R, dm11_R, dm14_R, dm15_R, dm19_r, dm20_R, 
                           dm21_R, dm22_R, dm23_r, dm28_R, dm30_R, dm32_r, dm34_r, dm37_r, dm39_R, dm43_R))

c_alpha1(df)
c_alpha(df)

dm1 %>% 
  select(dm3, dm4, dm5, dm8) %>%
  psych::alpha(title = "Accessibility")

dm1 %>% 
  select(dm9, dm10_r, dm11_r, dm13, dm14_r, dm15_r, dm16, dm17, dm18) %>%
  psych::alpha(title = "Communication")

dm1 %>% 
  select(dm9, dm10_r, dm11_r, dm13, dm14_r, dm15_r, dm16, dm17, dm18) %>%
  c_alpha1()

dm1 %>% 
  select(dm33, dm34_r, dm35, dm36) %>%
  psych::alpha(title = "Communication about prescriptions")


psych::alpha(df, use = "complete.obs")

df %>% psych::alpha(title = "")

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
