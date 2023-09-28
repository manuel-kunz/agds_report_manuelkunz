setwd("C:/Users/kunzm/OneDrive - Universitaet Bern/Studium/Master/Courses/HS23/AGSII/AGSI/R_code/introduction")
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)


half_hourly_fluxes <- readr::read_csv("./data/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006.csv")
half_hourly_fluxes

half_hourly_fluxes <- select(
  half_hourly_fluxes,
  starts_with("TIMESTAMP"),
  ends_with("_F"),
  GPP_NT_VUT_REF,
  NEE_VUT_REF_QC,
  starts_with("SWC_F_MDS_"),
  -contains("JSB"),
  NIGHT
)

class(half_hourly_fluxes$TIMESTAMP_START[[1]])
as.character(half_hourly_fluxes$TIMESTAMP_START[[1]])

dates <- ymd_hm(half_hourly_fluxes$TIMESTAMP_START)
month(dates[1])

half_hourly_fluxes$TIMESTAMP_START <- ymd_hm(half_hourly_fluxes$TIMESTAMP_START)

half_hourly_fluxes <- half_hourly_fluxes |> 
  mutate(TIMESTAMP_START = ymd_hm(TIMESTAMP_START))

half_hourly_fluxes <- half_hourly_fluxes |> 
  mutate(across(starts_with("TIMESTAMP_"), ymd_hm))

volcano[1:5, 1:5]

nrow(half_hourly_fluxes)/(2*24*365)


plot(
  half_hourly_fluxes[1:(2*24),]$TIMESTAMP_START,
  half_hourly_fluxes[1:(2*24),]$SW_IN_F,
  type = "l"
)

plot(
  half_hourly_fluxes[1:(365*2*24),]$TIMESTAMP_START,
  half_hourly_fluxes[1:(365*2*24),]$SW_IN_F,
  type = "l"
)

half_hourly_fluxes |>
  mutate(year = year(TIMESTAMP_START),
         month = month(TIMESTAMP_START),
         doy = yday(TIMESTAMP_START)     # day of year
  ) |>
  select(TIMESTAMP_START, TIMESTAMP_END, year, month, doy)

daily_fluxes <- half_hourly_fluxes |>  
  mutate(date = as_date(TIMESTAMP_START)) |>  # converts the ymd_hm-formatted date-time object to a date-only object (ymd)
  group_by(date) |> 
  summarise(SW_IN_F = mean(SW_IN_F))

plot(daily_fluxes[1:365,]$date, daily_fluxes[1:365,]$SW_IN_F, type = "l")


daily_fluxes <- half_hourly_fluxes |> 
  mutate(date = as_date(TIMESTAMP_START)) |>   # converts time object to a date object
  group_by(date) |> 
  summarise(GPP_NT_VUT_REF = mean(GPP_NT_VUT_REF, na.rm = TRUE),
            n_datapoints = n(), # counts the number of observations per day
            n_measured = sum(NEE_VUT_REF_QC == 0), # counts the number of actually measured data (excluding gap-filled and poor quality data)
            SW_IN_F = mean(SW_IN_F, na.rm = TRUE),  # we will use this later
            .groups = 'drop' # to un-group the resulting data frame
  ) |> 
  mutate(f_measured = n_measured / n_datapoints) # calculate the fraction of measured values over total observations
write_csv(daily_fluxes, file = "data/daily_fluxes.csv")
daily_fluxes

half_hourly_fluxes$GPP_NT_VUT_REF |> 
  table() |> 
  sort(decreasing = TRUE) |> 
  head()

half_hourly_fluxes |> 
  select(TIMESTAMP_START, starts_with("SWC_F_MDS_")) |> 
  head()

half_hourly_fluxes <- half_hourly_fluxes |>  
  mutate(across(where(is.numeric), ~na_if(., -9999)))

half_hourly_fluxes |> 
  select(TIMESTAMP_START, starts_with("SWC_F_MDS_")) |> 
  head()

visdat::vis_miss(
  half_hourly_fluxes |> slice(1:10000),
  cluster = FALSE, 
  warn_large_data = FALSE
)

half_hourly_fluxes |>
  filter(NEE_VUT_REF_QC == 0 | NEE_VUT_REF_QC == 1)


half_hourly_fluxes |> 
  mutate(GPP_NT_VUT_REF = ifelse(NEE_VUT_REF_QC %in% c(0,1), GPP_NT_VUT_REF, NA))

write_csv(half_hourly_fluxes, file = "data/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006_CLEAN.csv")



# read and format data from Ch 3
half_hourly_fluxes <- readr::read_csv("./data/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006_CLEAN.csv")

set.seed(2023)
plot_1 <- half_hourly_fluxes |>
  sample_n(2000) |>  # to reduce the dataset
  ggplot(aes(x = SW_IN_F, y = GPP_NT_VUT_REF)) +
  geom_point(size = 0.75, alpha = 0.4) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(x = expression(paste("Shortwave radiation (W m"^-2, ")")), 
       y = expression(paste("GPP (gC m"^-2, "s"^-1, ")"))) +
  theme_classic()

segment_points <- data.frame(x0 = 332, y0 = 3.65, y_regr = 8.77)

plot_1 +
  geom_segment(aes(x = x0, y = y0, xend = x0, yend = y_regr), 
               data = segment_points,
               color = "blue", lwd = 1.2, alpha = 0.8)


# numerical variables only, remove NA
df <- half_hourly_fluxes |>
  dplyr::select(-starts_with("TIMESTAMP")) |>
  tidyr::drop_na()

# fit univariate linear regression
linmod1 <- lm(GPP_NT_VUT_REF ~ SW_IN_F, data = df)

# fit multivariate linear regression
linmod2 <- lm(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, data = df)

linmod3 <- lm(GPP_NT_VUT_REF ~ ., data = df)

ls(linmod1)


summary(linmod1)

coef(linmod1)

sum(residuals(linmod1)^2)

broom::tidy(linmod1)



# create month category
df_cat <- half_hourly_fluxes |>
  mutate(MONTH = lubridate::month(TIMESTAMP_START)) |>
  tidyr::drop_na() |>
  dplyr::select(MONTH, GPP_NT_VUT_REF, SW_IN_F)


# fix class of categorical variables
df_cat <- df_cat |>
  mutate(MONTH = as.factor(MONTH))

linmod_cat <- lm(GPP_NT_VUT_REF ~ MONTH + SW_IN_F, data = df_cat)
summary(linmod_cat)

df_cat |>
  mutate(MONTH_NAME = lubridate::month(as.integer(MONTH), label = TRUE)) |>
  ggplot(aes(x = SW_IN_F, y = GPP_NT_VUT_REF)) +
  geom_point(alpha = 0.2) +
  geom_smooth(formula = y ~ x + 0, method = "lm", color = "red", se = FALSE) +
  labs(x = "SW", y = "GPP") +
  facet_wrap(~MONTH_NAME) +
  theme_classic()


linmod_inter <- lm(GPP_NT_VUT_REF ~ MONTH + SW_IN_F + MONTH:SW_IN_F, data = df_cat)
# equivalently: lm(GPP_NT_VUT_REF ~ MONTH * SW_IN_F, data = df_cat)
summary(linmod_inter)


quadmod <- lm(GPP_NT_VUT_REF ~ poly(SW_IN_F, 2), 
              data = df_cat |>
                filter(MONTH == 8))
summary(quadmod)

df_cat |>
  filter(MONTH == 8) |>
  ggplot(aes(x = SW_IN_F, y = GPP_NT_VUT_REF)) +
  geom_point(alpha = 0.4) +
  geom_smooth(formula = y ~ x, method = "lm", aes(color = "lm"), se = FALSE) +
  geom_smooth(formula = y ~ poly(x, 2), method = "lm", 
              aes(color = "poly2"), se = FALSE) +
  geom_smooth(formula = y ~ poly(x, 3), method = "lm",
              aes(color = "poly3"), se = FALSE) +
  labs(x = "SW", y = "GPP", color = "Regression") +
  theme_classic()


# generate correlated random data 
set.seed(1982)
df <- tibble(x = rnorm(100)) |> 
  mutate(y = x + rnorm(100)) |>
  mutate(y_fitted = lm(y ~ x)$fitted.values)

# implementations using Pearson's correlation
summary(lm(y ~ x, data = df))$r.squared

cor(df$y, df$x)^2 # remember: location and scale invariant

yardstick::rsq(df, y, x) |> pull(.estimate)

(sum((df$x - mean(df$x))*(df$y - mean(df$y))))^2/
  (sum((df$y - mean(df$y))^2)*sum((df$x - mean(df$x))^2))

# implementations using coefficient of determination definition
1 - sum((df$x - df$y)^2) / sum((df$y - mean(df$y))^2) # should be \hat{y}, not x

yardstick::rsq_trad(df, y, x) |> pull(.estimate) # incorrect

yardstick::rsq_trad(df, y, y_fitted) |> pull(.estimate) # correct

coef(lm(y ~ y_fitted, data = df))[2]



compute_regr_metrics <- function(mod){
  
  p <- length(mod$coefficients)
  n <- length(mod$residuals)
  
  tibble(
    mse = mean(mod$residuals^2),
    R2 = summary(mod)$r.squared,
    R2_adj = summary(mod)$adj.r.squared,
    AIC = extractAIC(mod)[2],
    AIC_adj = extractAIC(mod)[2] + 2*(p+2)*(p+3)/(n-p-3),
    BIC = BIC(mod) # this implementation is based on log-likelihood
  )
}

list_metrics <- purrr::map(
  list(linmod1, linmod2, linmod_cat, quadmod), 
  ~compute_regr_metrics(.))
names(list_metrics) <- c("Linear model", 
                         "Linear model 2", 
                         "Linear + categories",
                         "Quadratic model")
bind_rows(list_metrics, .id = "type")



set.seed(2023)
half_hourly_fluxes_small <- half_hourly_fluxes |>
  sample_n(100) |> # reduce dataset
  select(SW_IN_F, GPP_NT_VUT_REF)

plot_3 <- half_hourly_fluxes_small |>
  ggplot(aes(x = SW_IN_F, y = GPP_NT_VUT_REF)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", color = "red", fullrange = TRUE) +
  labs(x = expression(paste("Shortwave radiation (W m"^-2, ")")), 
       y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
  theme_classic() +
  ylim(-20, 40) + 
  xlim(0, 1100)

plot_4 <- half_hourly_fluxes_small |>
  add_row(SW_IN_F = 1100, GPP_NT_VUT_REF = -20) |> # add outlier
  ggplot(aes(x = SW_IN_F, y = GPP_NT_VUT_REF)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", color = "red", fullrange = TRUE) +
  labs(x = expression(paste("Shortwave radiation (W m"^-2, ")")), 
       y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
  theme_classic() +
  geom_point(aes(x = 1100, y = -20), colour = 'red', shape = 1, size = 3) +
  ylim(-20, 40) + 
  xlim(0, 1100)

cowplot::plot_grid(plot_3, plot_4)




# create an outlier for demonstration purposes
half_hourly_fluxes_outlier <- half_hourly_fluxes_small |>
  add_row(SW_IN_F = 1100, GPP_NT_VUT_REF = -20)

# Various ways to identify the outlier using graphs
plot_5 <- ggplot(
  data = half_hourly_fluxes_outlier,
  aes(x = GPP_NT_VUT_REF, y = after_stat(density))) +
  geom_histogram(fill = "grey70", color = "black") +
  geom_density(color = 'red')+
  labs(title = 'Histogram, density and boxplot', 
       x = expression(paste("GPP (gC m"^-2, "s"^-1, ")"))) +
  theme_classic()

plot_6 <- ggplot(
  data = half_hourly_fluxes_outlier,
  aes(x = "", y = GPP_NT_VUT_REF)) +
  geom_boxplot(fill = "grey70", color = "black") +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(y = expression(paste("GPP (gC m"^-2, "s"^-1, ")")))

plot_7 <- ggplot(
  data = half_hourly_fluxes_outlier,
  aes(x = SW_IN_F, y = after_stat(density))) +
  geom_histogram(fill = "grey70", color = "black") +
  geom_density(color = 'red')+
  labs(title = 'Histogram, density and boxplot', 
       x = expression(paste("Shortwave radiation (W m"^-2, ")"))) +
  theme_classic()

plot_8 <- ggplot(
  data = half_hourly_fluxes_outlier,
  aes(x = "", y = SW_IN_F)) +
  geom_boxplot(fill = "grey70", color = "black") +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(y = expression(paste("Shortwave radiation (W m"^-2, ")")))

cowplot::plot_grid(plot_5, plot_7, plot_6, plot_8,
                   ncol = 2, rel_heights = c(2,1),
                   align = 'v', axis = 'lr')


plot_9 <- ggplot(
  data = half_hourly_fluxes_outlier,
  aes(sample = GPP_NT_VUT_REF)) +
  geom_qq() +
  geom_qq_line() +
  labs(y = expression(paste("GPP (gC m"^-2, "s"^-1, ")")),
       x = "Theoretical normal quantiles") +
  theme_classic()

plot_10 <- ggplot(
  data = half_hourly_fluxes_outlier,
  aes(sample = SW_IN_F)) +
  geom_qq() +
  geom_qq_line() +
  labs(y = expression(paste("Shortwave radiation (W m"^-2, ")")),
       x = "Theoretical normal quantiles") +
  theme_classic()

cowplot::plot_grid(plot_9, plot_10, ncol = 2)


# Fit regression with outlier
linmod_outlier <- lm(GPP_NT_VUT_REF ~ SW_IN_F, 
                     data = add_row(half_hourly_fluxes_small, 
                                    SW_IN_F = 1100, 
                                    GPP_NT_VUT_REF = -20))

plot(linmod_outlier, 5)

