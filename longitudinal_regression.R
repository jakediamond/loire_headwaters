# -------------------------------------
# Author: 
# Purpose: 
# Date:
# -------------------------------------

x = df %>%
  # filter(
  #        site_code %in% c("cha053", "cha057")) %>%
  select(site_code, date, datetime, DO) %>%
  pivot_wider(names_from = site_code, values_from = DO) %>%
  mutate(year = year(date),
         month = month(date),
         hour = hour(datetime))

x %>%
  filter(between(month, 6, 10)) %>%
  ggplot(data = .,
         aes(x = viz062,
             y = viz221,
             color = hour,
             group = date)) +
  geom_point() + 
  # geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  theme_bw() +
  scale_color_viridis_c() +
  facet_wrap(~month)

y <- x %>%
  filter(between(month, 3, 3)) %>%
  select(coi061) %>%
  mutate(t = row_number())
ssp <- spectrum(y)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(coi061 ~ sin(2*pi/per*t)+cos(2*pi/per*t), data = y)
summary(reslm)
rg <- diff(range(y$coi061))
plot(y$coi061~y$t,ylim=c(min(y$coi061)-0.1*rg,max(y$coi061)+0.1*rg))
lines(fitted(reslm)~y$t,col=4,lty=2)   # dashed blue line is sin fit


# including 2nd harmonic really improves the fit
reslm2 <- lm(coi061 ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t), data = y)
summary(reslm2)
lines(fitted(reslm2)~y$t,col=3)    # solid green line is periodic with second harmonic


library(WaveletComp)
z <- x %>%
  filter(between(month, 3, 6)) %>%
  select(coi071, cha057, date) %>%
  arrange(date) %>%
  # select(-date) %>%
  drop_na() %>%
  as.data.frame()
wc <- analyze.coherency(z, loess.span = 0,
                        dt = 1/24, dj = 1/20,
                        window.size.t = 1, window.size.s = 1/2,
                        lowerPeriod = 1/4,
                        upperPeriod = 2,
                        make.pval = TRUE, n.sim = 10)
## (with color breakpoints according to quantiles):
wc.image(wc, main = "cross-wavelet power spectrum, coi071 over cha057",
         legend.params = list(lab = "cross-wavelet power levels"),
         periodlab = "period (days)",
         # time axis:
         label.time.axis = TRUE, show.date = TRUE,
         timetcl = -0.5) # outward ticks)

wc.avg(wc, siglvl = 0.05, sigcol = 'red',
       periodlab = "period (days)")
## Plot of wavelet coherence
## (with color breakpoints according to quantiles):
wc.image(wc, which.image = "wc", main = "wavelet coherence, x over y",
         legend.params = list(lab = "wavelet coherence levels",
                              lab.line = 3.5, label.digits = 3),
         periodlab = "period (days)")



test <- df %>%
  filter(site_code == "coi061",
         period %in% c("leafoff", "leafon")) %>%
  select(date, datetime, DO_per, DO_temp, period) %>%
  group_by(period) %>%
  mutate(t = row_number()) %>%
  ungroup() %>%
  select(-date, -datetime) %>%
  group_by(t) %>%
  pivot_wider(names_from = period, values_from = c(DO_per, DO_temp)) %>%
  arrange(t) %>%
  # select(-date) %>%
  drop_na() %>%
  as.data.frame()
wc <- analyze.coherency(test, my.pair = c(2, 3), loess.span = 0,
                        dt = 1/24, dj = 1/20,
                        window.size.t = 1, window.size.s = 1/2,
                        lowerPeriod = 1/4,
                        upperPeriod = 2,
                        make.pval = TRUE, n.sim = 10)
## (with color breakpoints according to quantiles):
wc.image(wc, main = "cross-wavelet power spectrum, x over y",
         legend.params = list(lab = "cross-wavelet power levels"),
         periodlab = "period (days)",
         # time axis:
         label.time.axis = TRUE, show.date = TRUE,
         timetcl = -0.5) # outward ticks)

wc.avg(wc, siglvl = 0.05, sigcol = 'red',
       periodlab = "period (days)")
## Plot of wavelet coherence
## (with color breakpoints according to quantiles):
wc.image(wc, which.image = "wc", main = "wavelet coherence, x over y",
         legend.params = list(lab = "wavelet coherence levels",
                              lab.line = 3.5, label.digits = 3),
         periodlab = "period (days)")
