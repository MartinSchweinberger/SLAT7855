## # install
## install.packages("Boruta")
## install.packages("car")
## install.packages("emmeans")
## install.packages("effects")
## install.packages("flextable")
## install.packages("ggplot2")
## install.packages("ggpubr")
## install.packages("Hmisc")
## install.packages("knitr")
## install.packages("lme4")
## install.packages("MASS")
## install.packages("mclogit")
## install.packages("MuMIn")
## install.packages("nlme")
## install.packages("ordinal")
## install.packages("rms")
## install.packages("robustbase")
## install.packages("sjPlot")
## install.packages("stringr")
## install.packages("tibble")
## install.packages("dplyr")
## install.packages("vcd")
## install.packages("vip")
## # install klippy for copy-to-clipboard button in code chunks
## install.packages("remotes")
## remotes::install_github("rlesur/klippy")

# set options
options(stringsAsFactors = F)          # no automatic data transformation
options("scipen" = 100, "digits" = 12) # suppress math annotation
# load packages
library(dplyr)
library(Boruta)
library(car)
library(effects)
library(emmeans)
library(flextable)
library(ggfortify)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(knitr)
library(lme4)
library(MASS)
library(mclogit)
library(MuMIn)
library(nlme)
library(ordinal)
library(rms)
library(robustbase)
library(sjPlot)
library(stringr)
library(tibble)
library(vcd)
library(vip)
# activate klippy for copy-to-clipboard button
klippy::klippy()

# load data
mlrdata  <- base::readRDS(url("https://slcladal.github.io/data/mld.rda", "rb"))

# inspect data
mlrdata %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 15 rows of the mlrdata.")  %>%
  flextable::border_outer()

# create plots
p1 <- ggplot(mlrdata, aes(status, money)) +   # data + x/y-axes
  geom_boxplot(fill=c("grey30", "grey70")) +  # def. col.
  theme_bw(base_size = 8)+   # black and white theme
  labs(x = "") +                              # x-axis label
  labs(y = "Money spent on present (AUD)", cex = .75) +   # y-axis label
  coord_cartesian(ylim = c(0, 250)) +         # y-axis range
  guides(fill = FALSE) +                      # no legend
  ggtitle("Status")                           # title
# plot 2
p2 <- ggplot(mlrdata, aes(attraction, money)) +
  geom_boxplot(fill=c("grey30", "grey70")) +
  theme_bw(base_size = 8) +
  labs(x = "") +                              # x-axis label
  labs(y = "Money spent on present (AUD)") +  # y-axis label
  coord_cartesian(ylim = c(0, 250)) +
  guides(fill = FALSE) +
  ggtitle("Attraction")
# plot 3
p3 <- ggplot(mlrdata, aes(x = money)) +
  geom_histogram(aes(y=..density..),          # add density statistic
                 binwidth = 10,               # def. bin width
                 colour = "black",            # def. bar edge colour
                 fill = "white") +            # def. bar col.
  theme_bw() +                                # black-white theme
  geom_density(alpha=.2, fill = "gray50") +   # def. col. of overlay
  labs(x = "Money spent on present (AUD)") +
  labs(y = "Density of frequency")
# plot 4
p4 <- ggplot(mlrdata, aes(status, money)) +
  geom_boxplot(notch = F, aes(fill = factor(status))) + # create boxplot
  scale_fill_manual(values = c("grey30", "grey70")) +   # def. col. palette
  facet_wrap(~ attraction, nrow = 1) +        # separate panels for attraction
  labs(x = "") +
  labs(y = "Money spent on present (AUD)") +
  coord_cartesian(ylim = c(0, 250)) +
  guides(fill = FALSE) +
  theme_bw(base_size = 8)
# show plots
vip::grid.arrange(grobs = list(p1, p2, p3, p4), widths = c(1, 1), layout_matrix = rbind(c(1, 2), c(3, 4)))

m1.mlr = lm(                      # generate lm regression object
  money ~ 1 + attraction*status,  # def. regression formula (1 = intercept)
  data = mlrdata)                 # def. data
m1.glm = glm(                     # generate glm regression object
  money ~ 1 + attraction*status,  # def. regression formula (1 = intercept)
  family = gaussian,              # def. linkage function
  data = mlrdata)                 # def. data

# automated AIC based model fitting
step(m1.mlr, direction = "both")

m2.mlr = lm(                       # generate lm regression object
  money ~ (status + attraction)^2, # def. regression formula
  data = mlrdata)                  # def. data
m2.glm = glm(                      # generate glm regression object
  money ~ (status + attraction)^2, # def. regression formula
  family = gaussian,               # def. linkage function
  data = mlrdata)                  # def. data
# inspect final minimal model
summary(m2.mlr)

#intercept  Single  NotInterested  Single:NotInterested
99.15     + 57.69  + 0           + 0     # 156.8 single + interested
99.15     + 57.69  - 47.66       - 63.18 # 46.00 single + not interested
99.15     - 0      + 0           - 0     # 99.15 relationship + interested
99.15     - 0      - 47.66       - 0     # 51.49 relationship + not interested

# make prediction based on the model for original data
prediction <- predict(m2.mlr, newdata = mlrdata)
# inspect predictions
table(round(prediction,2))

# extract confidence intervals of the coefficients
confint(m2.mlr)
# create and compare baseline- and minimal adequate model
m0.mlr <- lm(money ~1, data = mlrdata)
anova(m0.mlr, m2.mlr)

# compare baseline- and minimal adequate model
Anova(m0.mlr, m2.mlr, type = "III")

# generate plots
autoplot(m2.mlr) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw()

# determine a cutoff for data points that have D-values higher than 4/(n-k-1)
cutoff <- 4/((nrow(mlrdata)-length(m2.mlr$coefficients)-2))
# start plotting
par(mfrow = c(1, 2))           # display plots in 3 rows/2 columns
qqPlot(m2.mlr, main="QQ Plot") # create qq-plot
plot(m2.mlr, which=4, cook.levels = cutoff); par(mfrow = c(1, 1))

# extract influence statistics
infl <- influence.measures(m2.mlr)
# add infl. statistics to data
mlrdata <- data.frame(mlrdata, infl[[1]], infl[[2]])
# annotate too influential data points
remove <- apply(infl$is.inf, 1, function(x) {
  ifelse(x == TRUE, return("remove"), return("keep")) } )
# add annotation to data
mlrdata <- data.frame(mlrdata, remove)
# number of rows before removing outliers
nrow(mlrdata)
# remove outliers
mlrdata <- mlrdata[mlrdata$remove == "keep", ]
# number of rows after removing outliers
nrow(mlrdata)

# recreate regression models on new data
m0.mlr = lm(money ~ 1, data = mlrdata)
m0.glm = glm(money ~ 1, family = gaussian, data = mlrdata)
m1.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m1.glm = glm(money ~ status * attraction, family = gaussian,
             data = mlrdata)
# automated AIC based model fitting
step(m1.mlr, direction = "both")

# create new final models
m2.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m2.glm = glm(money ~ status * attraction, family = gaussian,
             data = mlrdata)
# inspect final minimal model
summary(m2.mlr)

# extract confidence intervals of the coefficients
confint(m2.mlr)

# compare baseline with final model
anova(m0.mlr, m2.mlr)

# compare baseline with final model
Anova(m0.mlr, m2.mlr, type = "III")

# generate plots
autoplot(m2.mlr) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw()

# determine a cutoff for data points that have
# D-values higher than 4/(n-k-1)
cutoff <- 4/((nrow(mlrdata)-length(m2.mlr$coefficients)-2))
# start plotting
par(mfrow = c(1, 2))           # display plots in 1 row/2 columns
qqPlot(m2.mlr, main="QQ Plot") # create qq-plot
plot(m2.mlr, which=4, cook.levels = cutoff); par(mfrow = c(1, 1))

# add model diagnostics to the data
mlrdata <- mlrdata %>%
  dplyr::mutate(residuals = resid(m2.mlr),
                standardized.residuals = rstandard(m2.mlr),
                studentized.residuals = rstudent(m2.mlr),
                cooks.distance = cooks.distance(m2.mlr),
                dffit = dffits(m2.mlr),
                leverage = hatvalues(m2.mlr),
                covariance.ratios = covratio(m2.mlr),
                fitted = m2.mlr$fitted.values)

# plot 5
p5 <- ggplot(mlrdata,
             aes(studentized.residuals)) +
  theme(legend.position = "none")+
  geom_histogram(aes(y=..density..),
                 binwidth = .2,
                 colour="black",
                 fill="gray90") +
  labs(x = "Studentized Residual", y = "Density") +
  stat_function(fun = dnorm,
                args = list(mean = mean(mlrdata$studentized.residuals, na.rm = TRUE),
                            sd = sd(mlrdata$studentized.residuals, na.rm = TRUE)),
                colour = "red", size = 1) +
  theme_bw(base_size = 8)
# plot 6
p6 <- ggplot(mlrdata, aes(fitted, studentized.residuals)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "Red")+
  theme_bw(base_size = 8)+
  labs(x = "Fitted Values",
       y = "Studentized Residual")
# plot 7
p7 <- qplot(sample = mlrdata$studentized.residuals, stat="qq") +
  theme_bw(base_size = 8) +
  labs(x = "Theoretical Values",
       y = "Observed Values")
vip::grid.arrange(p5, p6, p7, nrow = 1)

# 1: optimal = 0
# (listed data points should be removed)
which(mlrdata$standardized.residuals > 3.29)

# 2: optimal = 1
# (listed data points should be removed)
stdres_258 <- as.vector(sapply(mlrdata$standardized.residuals, function(x) {
ifelse(sqrt((x^2)) > 2.58, 1, 0) } ))
(sum(stdres_258) / length(stdres_258)) * 100

# 3: optimal = 5
# (listed data points should be removed)
stdres_196 <- as.vector(sapply(mlrdata$standardized.residuals, function(x) {
ifelse(sqrt((x^2)) > 1.96, 1, 0) } ))
(sum(stdres_196) / length(stdres_196)) * 100

# 4: optimal = 0
# (listed data points should be removed)
which(mlrdata$cooks.distance > 1)

# 5: optimal = 0
# (data points should be removed if cooks distance is close to 1)
which(mlrdata$leverage >= (3*mean(mlrdata$leverage)))

# 6: checking autocorrelation:
# Durbin-Watson test (optimal: high p-value)
dwt(m2.mlr)

# 7: test multicollinearity 1
vif(m2.mlr)

# 8: test multicollinearity 2
1/vif(m2.mlr)

# 9: mean vif should not exceed 1
mean(vif(m2.mlr))

# load functions
source("https://slcladal.github.io/rscripts/SampleSizeMLR.r")
source("https://slcladal.github.io/rscripts/ExpR.r")
# check if sample size is sufficient
smplesz(m2.mlr)
# check beta-error likelihood
expR(m2.mlr)

# tabulate model results
sjPlot::tab_model(m0.glm, m2.glm)

summary(m2.mlr)

report::report(m2.mlr)

sessionInfo()
