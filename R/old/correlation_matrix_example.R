## ---
##
## Script name: thayer_stevens_2020_rcode.R
##
## Purpose of script: This script analyzes the data investigating the effect of human-animal interaction on affect and cognition.
##
## Authors: Elise Thayer (eliserthayer@gmail.com) and Jeffrey R. Stevens (jeffrey.r.stevens@gmail.com)
##
## Date Created: 2020-01-13
##
## Date Finalized: 2020-07-23
##
## License: All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0).
##  You are free to:
##   Share â€” copy and redistribute the material in any medium or format
##   Adapt â€” remix, transform, and build upon the material for any purpose, even commercially.
##  Under the following terms:
##   Attribution â€” You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
##   No additional restrictions â€” You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
##
## ---
##
## Notes:
## Instructions: Place this file and the data files in the main directory.
## 	Create a folder called "figures". Set the R working directory to the main directory.
##  At the R command prompt, type
## 	> source("thayer_stevens_2020_rcode.R")
## 	This will run the script, adding all of the calculated variables to the workspace and
##  saving figures in the figures directory. If packages do not load properly, install them
##  with install.packages("package_name").
## Data files:
## ---
## thayer_stevens_2020_data1.csv (primary affective and cognitive data set)
##  experiment - experiment number (1 or 2)
##  date - date participant completed experiment
##  participant - participant number
##  condition - experimental condition (hai = human-animal interaction, control)
##  age_num - participant age
##  gender - participant gender
##  race - participant race/ethnicity
##  parent_income - participant's parental income
##  pas - Pet Attitude Scale mean score
##  pets_now - response to whether participant currently has pet (1 = yes, 0 = now)
##  pets_child - response to whether participant had pet as child (1 = yes, 0 = now)
##  dog_discomfort_revkey - response to Gee et al.'s discomfort toward dog (reverse coded)
##  dog_ambivalence_revkey - response to Gee et al.'s ambivalence toward dogreverse coded)
##  dog_desire_to_interact - response to Gee et al.'s desire to interact with dog
##  dog_comfort - response to Gee et al.'s comfort toward dogreverse coded)
##  duration_interaction - duration of interaction with dog (in seconds)
##  panas_pre_neg - pre-condition PANAS score for negative affect
##  panas_pre_pos - pre-condition PANAS score for positive affect
##  panas_post_neg - post-condition PANAS score for negative affect
##  panas_post_pos - post-condition PANAS score for positive affect
##  panas_pos_diff - pre-post difference for PANAS score for positive affect
##  panas_neg_diff - pre-post difference for PANAS score for negative affect
##  vas_anxiety_pre - pre-condition visual analog scale for anxiety
##  vas_anxiety_post - post-condition visual analog scale for anxiety
##  vas_stress_pre - pre-condition visual analog scale for stress
##  vas_stress_post - post-condition visual analog scale for stress
##  stai_trait - trait score of State-Trait Anxiety Index
##  stai_state - state score of State-Trait Anxiety Index
##  drm_d_prime - d' score for Deese-Roedinger-McDermott long-term memory task
##  ncpc_pre_diff - pre-condition difference between second and first trial of Necker Cube Pattern Control Test
##  ncpc_post_diff - post-condition difference between second and first trial of Necker Cube Pattern Control Test
##  ncpc_diff - pre-post difference for difference between second and first trial of Necker Cube Pattern Control Test
##  bds_index_pre - pre-condition backwards digit span index
##  bds_index_post - post-condition backwards digit span index
##  bds_index_diff - pre-post difference for backwards digit span index
##  nback_d_prime_pre - pre-condition d' for n-back task
##  nback_d_prime_post - post-condition d' for n-back task
##  nback_d_prime_diff - pre-post difference for d' for n-back task
##
## thayer_stevens_2020_data2.csv (item-specifc data for calculating Cronbach's alpha)
##  item_1 - item_20 - individual items (surveys differ on number of items, so NAs represent no items)
##  survey - name of survey
##
## ---


# Load libraries ---------------------------------------------------------------

library(BayesFactor)  # calculate Bayes factors
library(ggbeeswarm)  # plot beeswarm plots
library(ggcorrplot)  # plot correlation plots
library(Hmisc)  # calculate correlation matrix r and p
library(emmeans)  # calculate estimated marginal means for models
library(papaja)  # format for APA
library(psych)  # calculate descriptive statistics
library(rcompanion)  # calculate Wilcoxon rank sum test effect size
library(tidyverse)  # wrangle and plot data
library(patchwork)  # plot subfigures
# library(here)  # set working directory


# Define options and functions ----------------------------------------------------------

# Disable scientific notation
options(scipen = 6, digits = 4)

# Define color-blind safe colors
cb_palette_black <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

## Paired analyses

# Check assumptions of paired tests
assumption_check <- function(df, measure) {
  # Substitute variable names to manipulate in function
  var <- eval(substitute(measure), df)
  var_name <- substitute(var)

  # Conduct Shapiro-Wilk test of normality on HAI and control data
  normal_hai <- with(df, shapiro.test(var_name[condition == "hai"]))
  normal_control <- with(df, shapiro.test(var_name[condition == "control"]))
  normal_hai_p <- normal_hai$p.value
  normal_control_p <- normal_control$p.value

  # Conduct F-test to compare variances
  var_hai_control <- var.test(var_name ~ condition, data = df)
  var_hai_control_p <- var_hai_control$p.value

  # Check for NAs
  var_na <- with(df, any(is.na(var_name)))  # check for NAs

  # Print assumption check output
  if (normal_hai_p > 0.05 && normal_control_p > 0.05 && var_hai_control_p > 0.05 && var_na == FALSE) {
    print("Both HAI and control are normally distributed with equal variance.")
  } else if (normal_hai_p > 0.05 && normal_control_p > 0.05 && var_hai_control_p > 0.05 && var_na == TRUE) {
    print("Both HAI and control are normally distributed with equal variance; however, the dataframe has NAs.")
  } else {
    cat(paste0("Check assumptions:", "\n", "HAI normally distributed? ", round(normal_hai_p, digits = 4), "\n", "Control normally distributed? ", round(normal_control_p, digits = 4), "\n", "HAI and control equal variance? ", round(var_hai_control_p, digits = 4), "\n", "Data frame has NAs? ", var_na))
  }
}

# Conduct analysis testing differences between HAI and control groups
paired_analysis <- function(df, measure, measure_name, test_type, experiment, pre_diff, apa_freq, apa_bayes, effsize) {
  # Substitute variable names to manipulate in function
  var <- eval(substitute(measure), df)
  var_name <- substitute(var)

  # Check test type
  if (test_type == "w") {  # if Wilcoxon rank sum test
    # Calculate Wilcoxon rank sum test
    wtest <- with(df, wilcox.test(var_name[condition == "hai"], var_name[condition == "control"], conf.int = TRUE))
    assign(apa_freq,  with(df, apa_print(wilcox.test(var_name[condition == "hai"], var_name[condition == "control"], conf.int = TRUE))), envir = .GlobalEnv)  # assign variable
    wtest_statistic <- round(wtest$statistic, digits = 0)  # extract test statistic
    wtest_n <- nrow(df)  # extract N
    wtest_df <- 1  # assign degrees of freedom
    hai_mean <- with(df, mean(var_name[condition == "hai"]))  # calculate HAI mean
    control_mean <- with(df, mean(var_name[condition == "control"]))  # calculate control mean
    ci <- str_c("[", round(wtest$conf.int[1], digits = 2), ", ", round(digits = 2, wtest$conf.int[2]), "]", sep = "")  # extract 95% CIs
    wtest_p <- wtest$p.value  # extract p-value

    # HAI data
    hai <- filter(df, condition == "hai")  # extract HAI data
    var <- eval(substitute(measure), hai)  # select measure column from
    hai_var <- substitute(var)  # substitute variable

    # Control data
    control <- filter(df, condition == "control")  # extract control data
    var <- eval(substitute(measure), control)  # select measure column
    control_var <- substitute(var)  # substitute variable

    # Compute Bayes factor for condition difference
    mysamples <- rankSumGibbsSampler(hai_var[!is.na(hai_var)], control_var[!is.na(control_var)])  # create samples for rank sum test
    wtest_bf <- computeBayesFactorOneZero(mysamples)  # calculate Bayes factor
    assign(apa_bayes, wtest_bf, envir = .GlobalEnv)  # assign variable

    # Calculate effect size
    assign(effsize, abs(with(df, wilcoxonR(var_name, condition))), envir = .GlobalEnv)  # r

    # Summarize and print results
    summary <- c(measure_name, toupper(test_type), wtest_n, wtest_df, wtest_statistic, hai_mean, control_mean, ci, wtest_p, wtest_bf)
    print(wtest)
    print(table(df$condition))
  } else {  # if t-test
    # Calculate t-test
    ttest <- with(df, t.test(var_name[condition == "hai"], var_name[condition == "control"]))
    assign(apa_freq,  with(df, apa_print(t.test(var_name[condition == "hai"], var_name[condition == "control"]))), envir = .GlobalEnv)
    ttest_statistic <- ttest$statistic  # extract test statistic
    ttest_n <- nrow(df)  # extract sample size
    ttest_df <- (nrow(df)) - 2  # extract degrees of freedom
    hai_mean <- with(df, mean(var_name[condition == "hai"]))  # calculate HAI mean
    control_mean <- with(df, mean(var_name[condition == "control"]))  # calculate control mean
    ci <- str_c("[", round(ttest$conf.int[1], digits = 2), ", ", round(ttest$conf.int[2], digits = 2), "]", sep = "")  # extract 95% CIs
    ttest_p <- ttest$p.value  # extract p-value

    # Computer Bayes factor
    ttest_bf <- with(df, extractBF(ttestBF(var_name[condition == "hai"], var_name[condition == "control"]), logbf = FALSE, onlybf = TRUE)[1])  # calculate Bayes factor for t-test
    assign(apa_bayes, ttest_bf, envir = .GlobalEnv)  # assign variable

    # Effect size
    assign(effsize, with(df, effsize::cohen.d(var_name[condition == "hai"], var_name[condition == "control"]))$estimate, envir = .GlobalEnv)  # Cohen's d

    # Summarize and print results
    summary <- c(measure_name, test_type, ttest_n, ttest_df, ttest_statistic, hai_mean, control_mean, ci, ttest_p, ttest_bf)
    print(ttest)
    print(table(df$condition))
  }
  # Direct output to correct data frame
  if (experiment == 1 && pre_diff == "pre") {
    baseline_1 <<- rbind(baseline_1, summary)
  } else if (experiment == 1 && pre_diff == "diff") {
    summary_1 <<- rbind(summary_1, summary)
  } else if (experiment == 2 && pre_diff == "pre") {
    baseline_2 <<- rbind(baseline_2, summary)
  } else {
    summary_2 <<- rbind(summary_2, summary)
  }
}

# Calculate Bayes factors for Wilcoxon rank sum test
# From van Doorn, J., Ly, A., Marsman, M., & Wagenmakers, E.-J. (2020). Bayesian rank-based hypothesis testing for the rank sum test, the signed rank test, and Spearmanâ€™s Ï. Journal of Applied Statistics. https://doi.org/10.1080/02664763.2019.1709053
# Using the code from https://osf.io/gny35/.

truncNormSample <- function(lBound = -Inf, uBound = Inf, mu = 0, sd = 1) {
  lBoundUni <- pnorm(lBound, mean = mu, sd = sd)
  uBoundUni <- pnorm(uBound, mean = mu, sd = sd)
  mySample <- qnorm(runif(1, lBoundUni, uBoundUni), mean = mu, sd = sd)
  return(mySample)
}

upperLowerTruncation <- function(ranks, values, currentRank) {
  if (currentRank == min(ranks)) {
    under <- -Inf
  } else {
    under <- max(values[ranks < currentRank])
  }
  if (currentRank == max(ranks)) {
    upper <- Inf
  } else {
    upper <- min(values[ranks > currentRank])
  }
  return(list(under = under, upper = upper))
}

computeBayesFactorOneZero <- function(posteriorSamples, priorParameter = 1, oneSided = FALSE, whichTest = "Wilcoxon") {
  postDens <- logspline::logspline(posteriorSamples)
  densZeroPoint <- logspline::dlogspline(0, postDens)
  corFactorPosterior <- logspline::plogspline(0, postDens)
  # priorParameter should be the Cauchy scale parameter
  priorDensZeroPoint <- dcauchy(0, scale = priorParameter)
  corFactorPrior <-  pcauchy(0, scale = priorParameter, lower.tail = (oneSided != "right"))
  bf10 <- priorDensZeroPoint / densZeroPoint
  return(bf10)
}

rankSumGibbsSampler <- function(xVals, yVals, nSamples = 1e3, cauchyPriorParameter = 1 / sqrt(2),  progBar = TRUE, nBurnin = 1, nGibbsIterations = 10, nChains = 5) {
  if (progBar) {
    myBar <- txtProgressBar(min = 1, max = nSamples * nChains, initial = 1, char = "*", style = 3, width = 50)
  }
  n1 <- length(xVals)
  n2 <- length(yVals)
  allRanks <- rank(c(xVals, yVals))
  xRanks <- allRanks[1:n1]
  yRanks <- allRanks[(n1 + 1):(n1 + n2)]
  deltaSamples <- numeric(nSamples)
  deltaSamplesMatrix <- matrix(ncol = nChains, nrow = nSamples - nBurnin)
  totalIterCount <- 0
  for (thisChain in 1:nChains) {
    currentVals <- sort(rnorm((n1 + n2)))[allRanks] # initial values
    oldDeltaProp <- 0
    for (j in 1:nSamples) {
      for (i in sample(1:(n1 + n2))) {
        currentRank <- allRanks[i]
        currentBounds <- upperLowerTruncation(ranks = allRanks, values = currentVals, currentRank = currentRank)
        if (i <= n1) {
          oldDeltaProp <- -0.5 * oldDeltaProp
        } else {
          oldDeltaProp <- 0.5 * oldDeltaProp
        }
        currentVals[i] <- truncNormSample(currentBounds[["under"]], currentBounds[["upper"]], mu = oldDeltaProp, sd = 1)
      }
      decorStepResult <- decorrelateStepTwoSample(currentVals[1:n1], currentVals[(n1 + 1):(n1 + n2)], oldDeltaProp, sigmaProp = 0.5)
      xVals <- decorStepResult[[1]]
      yVals <- decorStepResult[[2]]
      gibbsResult <- sampleGibbsTwoSampleWilcoxon(x = xVals, y = yVals, nIter = nGibbsIterations, rscale = cauchyPriorParameter)
      deltaSamples[j] <- oldDeltaProp <- gibbsResult
      if (progBar) setTxtProgressBar(myBar, j + ((thisChain - 1) * nSamples))
    }
    if (nBurnin > 0) {
      deltaSamples <- -deltaSamples[-(1:nBurnin)]
    } else {
      deltaSamples <- -deltaSamples
    }
    deltaSamplesMatrix[, thisChain] <- deltaSamples
  }
  betweenChainVar <- (nSamples / (nChains - 1)) * sum((apply(deltaSamplesMatrix, 2, mean)  - mean(deltaSamplesMatrix))^2)
  withinChainVar <- (1 / nChains) * sum(apply(deltaSamplesMatrix, 2, var))
  fullVar <- ((nSamples - 1) / nSamples) * withinChainVar + (betweenChainVar / nSamples)
  rHat <- sqrt(fullVar / withinChainVar)
  return(list(deltaSamples = as.vector(deltaSamplesMatrix), rHat = rHat))
}

sampleGibbsTwoSampleWilcoxon <- function(x, y, nIter = 10, rscale = 1 / sqrt(2)) {
  meanx <- mean(x)
  meany <- mean(y)
  n1 <- length(x)
  n2 <- length(y)
  sigmaSq <- 1 # Arbitrary number for sigma
  g <- 1
  for (i in 1:nIter) {
    #sample mu
    varMu <- (4 * g * sigmaSq) / (4 + g * (n1 + n2))
    meanMu <- (2 * g * (n2 * meany - n1 * meanx)) / ((g * (n1 + n2) + 4))
    mu <- rnorm(1, meanMu, sqrt(varMu))
    # sample g
    betaG <- (mu^2 + sigmaSq * rscale^2) / (2 * sigmaSq)
    g <- 1 / rgamma(1, 1, betaG)
    # convert to delta
    delta <- mu / sqrt(sigmaSq)
  }
  return(delta)
}

decorrelateStepTwoSample <- function(x, y, muProp, sigmaProp = 1) {
  thisZ <- rnorm(1, 0, sigmaProp)
  newX <- x + thisZ
  newY <- y + thisZ
  denom <- sum(dnorm(x, (muProp - thisZ) * -0.5, log = TRUE) + dnorm(y, (muProp - thisZ) * 0.5, log = TRUE))
  num <- sum(dnorm(newX, muProp * -0.5, log = TRUE) + dnorm(newY, muProp * 0.5, log = TRUE))
  if (runif(1) < exp(num - denom)) {
    return(list(x = newX, y = newY, accept = TRUE))
  } else {
    return(list(x = x, y = y, accept = FALSE))
  }
}

## ANCOVA analyses

# Check assumptions of ANCOVA tests
assumption_check_ancova <- function(df, pre_measure, post_measure) {
  # Substitute variable names to manipulate in function
  pre_prep <- eval(substitute(pre_measure), df)
  pre <- substitute(pre_prep)
  post_prep <- eval(substitute(post_measure), df)
  post <- substitute(post_prep)

  # Fit models
  aovmod <- with(df, summary(aov(post ~ pre * condition)))  #ANCOVA
  lmmod <- with(df, lm(post ~ pre + condition))  # linear model, no interaction
  lmmodint <- with(df, lm(post~ post * condition))  # linear model, with interaction
  intp <- aovmod[[1]]$`Pr(>F)`[3]  # extract p-value

  # Generate histogram of data
  p0 <- df %>%
    ggplot(aes(x = post, fill = condition)) +
    geom_histogram(alpha = 0.6, position = "identity")

  # Generate scatterplot to look for linear relationships
  p1 <- df %>%
    ggplot(aes(x = pre, y = post, color = condition)) +
    theme_classic() +
    geom_point() +
    geom_smooth(aes(group = condition), method = "lm", se = FALSE)

  # Check homogeneity of variance
  vartest <- var.test(with(df, post ~ condition))  # conduct variance test
  print(vartest)
  vartestp <- vartest$p.value  # extract p-value
  eqcov <- with(df, t.test(pre ~ condition))  # check the equality of the two groups on the covariate
  print(eqcov)
  eqcovp <- eqcov$p.value  # extract p-value

  # Summarize and print results
  if (intp > 0.05) {  # if the p-value for the interaction term is not significant
    # Generate qqplot
    p2 <- plot(lmmod, 2)  # inspect qqplot for normality of residuals

    # Test normality of residuals
    shap <- shapiro.test(resid(lmmod))  # conduct Shapiro-Wilks test
    print(shap)
    shapp <- shap$p.value  # extract p-value

    # Plot residuals and fitted values
    p3 <- plot(lmmod$residuals~lmmod$fitted.values)  # inspect for homogeneity of variance

    # Print results
    cat(paste0("Check assumptions:", "\n", "Interaction signigificant? ", round(intp, digits = 4), " (see correlation fig)", "\n", "Normally distributed residuals? ", round(shapp, digits = 4), " (qqplot)", "\n", "HAI and control equal variance? ", round(vartestp, digits = 4), " (look for pattern in mod$residuals)", "\n", "Groups equal on covariate? ", eqcovp))
  } else {   # if interaction p-value <= 0.05
    # Generate qqplot with no interaction
    p2 <- plot(lmmod, 2)  # inspect qqplot for normality of residuals

    # Test normality of residuals with no interaction
    shap1 <- shapiro.test(resid(lmmod))  # conduct Shapiro-Wilks test
    print(shap1)
    shap1p <- shap1$p.value  # extract p-value

    # Plot residuals and fitted values with no interaction
    p4 <- plot(lmmod$residuals~lmmod$fitted.values)  # inspect for homogeneity of variance

    # Generate qqplot with interaction
    p3 <- plot(lmmodint, 2)  # inspect qqplot for normality of residuals

    # Test normality of residuals with interaction
    shap2 <- shapiro.test(resid(lmmodint))  # p value for normality of residuals (interaction)
    print(shap2)
    shap2p <- shap2$p.value  # extract p-value

    # Plot residuals and fitted values with interaction
    p5 <- plot(lmmodint$residuals~lmmodint$fitted.values)  # inspect for homogeneity of variance

    # Print results
    cat(paste0("Check assumptions:", "\n", "Interaction signigificant? ", round(intp, digits = 4), " (see correlation fig)", "\n", "Normally distributed residuals (NO INTERACTION)? ", round(shap1p, digits = 4),  " (qqplot)",  "\n", "Normally distributed residuals (INTERACTION)? ", round(shap2p, digits = 4), "\n", "HAI and control equal variance? ", round(vartestp, digits = 4), " (look for pattern in mod$residuals)", "\n", "Groups equal on covariate? ", eqcovp))
  }
  print(summary(lmmod))
}

# Conduct ANCOVA analyses
ancova_analysis <- function(df, measure_name, post_measure, pre_measure, apa_freq, apa_means, apa_bf, experiment) {
  # Substitute variable names to manipulate in function
  pre_prep <- eval(substitute(pre_measure), df)
  pre <- substitute(pre_prep)
  post_prep <- eval(substitute(post_measure), df)
  post <- substitute(post_prep)

  # Run ANCOVA model
  mod <- with(df, aov(post ~ pre + condition))  # run interaction model
  modsum <- summary(mod)  # summarize model
  print(modsum)
  modextract <- modsum[[1]]  # extract model table
  modp <- modextract$`Pr(>F)`[2]  # extract p-value
  assign(apa_freq, with(df, apa_print(aov(post ~ pre + condition))), envir = .GlobalEnv)  # create manuscript-ready output with apa_print

  # Calculate model-adjusted means
  calcmeans <- emmeans::emmeans(mod, specs = "condition")  # calculate least-squares means from models
  summeans <- summary(calcmeans)  # summarize output
  print(summeans)
  control_mean <- summeans$emmean[1]  # extract control means
  hai_mean <- summeans$emmean[2]  # extract HAI means
  assign(apa_means, apa_print(emmeans(mod, specs = "condition")), envir = .GlobalEnv)  # create manuscript-ready object with apa_print

  # Compute Bayes factor
  df_bf <- df %>%
    mutate(postBF = post, # re-naming vars as BF issue workaround
           preBF = pre)
  full <- BayesFactor::lmBF(formula = postBF ~ preBF + condition, data = df_bf)  # calculate Bayes factor for full linear model
  cov_only <- BayesFactor::lmBF(formula = postBF ~ preBF, data = df_bf)  # calculate Bayes factor for model with pre only
  cond_bf <- full / cov_only  # calculate Bayes factor for adding condition
  print(cond_bf)
  extract_bf <- extractBF(cond_bf, logbf = FALSE, onlybf = TRUE)[1]  # extract Bayes factor
  assign(apa_bf, extract_bf, envir = .GlobalEnv)  # create manuscript-ready object with apa_print

  # Create table
  test_type <- "ANCOVA"
  test_n <- as.numeric(base::nrow(df))
  test_stat <- dfm <- ci <- "-"
  summary <- c(measure_name, test_type, test_n, dfm, test_stat, hai_mean, control_mean, ci, modp, extract_bf)
  # Bind to existing table
  if (experiment == 1) {
    summary_1 <<- rbind(summary_1, summary)
  } else {
    summary_2 <<- rbind(summary_2, summary)
  }
}

# Generate plot of pre-post data
prepost_plot <- function(df, pre, post, y_title, plot_name) {
  # Generate plot
  p <- df %>%
    select(participant, condition, Pre =  pre, Post = post) %>%  # select and rename columns
    pivot_longer(-c(participant, condition), names_to = "time", values_to = "score") %>%  # pivot data longer
    ggplot(aes(x = time, y = score, shape = condition)) +
    geom_beeswarm(aes(color = condition), alpha = 0.4, size = 3.1, cex = 3, dodge.width = 0.2) +  # plot individual points
    stat_summary(fun = mean, geom = "line", lwd = 0.9, aes(group = condition, color = condition)) +  # plot line connecting group means
    stat_summary(fun.data = mean_cl_normal, aes(group = condition, color = condition, shape = condition), shape = c(17, 17, 19, 19), position = position_dodge(width = 0.1)) + # plot means and CIs
    scale_x_discrete(limits = c("Pre", "Post")) +  # change condition labels
    scale_shape_manual(values = c(2, 1)) +  # change data point shape
    scale_color_manual(breaks = c("control", "hai"), labels = c("Control", "HAI"), values = cb_palette_black[c(5, 6)]) +  # change x-axis tick labels and colors
    labs(y = y_title) +  # label y-axis
    theme_minimal() +  # set theme
    theme(legend.position = "none",  # remove legend
          axis.title.x = element_blank(),  # remove x-axis title
          axis.title.y = element_text(size = 20),  # y-axis title text size
          axis.text.x = element_text(size = 20),  # x-axis tick mark text size
          axis.text.y = element_text(size = 15))  # x-axis tick mark text size
  assign(plot_name, p, envir = .GlobalEnv)  # create global variable
}

# Generate plot of post data
predicted_plot <- function(df, pre_measure, post_measure, y_title, plot_name) {
  # Substitute variable names to manipulate in function
  pre_prep <- eval(substitute(pre_measure), df)
  pre <- substitute(pre_prep)
  post_prep <- eval(substitute(post_measure), df)
  post <- substitute(post_prep)

  # Run ANCOVA
  mod <- aov(post ~ pre + condition, data = df)
  df$Post <- predict(mod)  # calculate model predictions

  # Extract number of data points per condition
  num_control <- table(df$condition)[1]
  num_hai <- table(df$condition)[2]

  # Generate plot
  p <- df %>%
    ggplot(aes(x = condition, y = Post, color = condition, shape = condition)) +
    geom_beeswarm(size = 3.1, alpha = 0.4, shape = c(rep(2, num_control), rep(1, num_hai)), cex = 3, dodge.width = 0.2) +  # plot individual points
    stat_summary(fun.data = mean_cl_normal, color = cb_palette_black[c(5, 6)]) +  # plot means and CIs
    scale_color_manual(breaks = c("control", "hai"), values = cb_palette_black[c(5, 6)]) +  # set group color
    scale_x_discrete(breaks = c("control", "hai"), labels = c("Control", "HAI")) +  # change x-axis tick labels
    scale_shape_manual(values = c(17, 19)) +  # change data point shape
    labs(y = y_title) +  # label y-axis
    theme_minimal() +  # set theme
    theme(legend.position = "none",  # remove legend
          axis.title.x = element_blank(),  # remove x-axis title
          axis.title.y = element_text(size = 20),  # y-axis title text size
          axis.text.x = element_text(size = 20),  # x-axis tick mark text size
          axis.text.y = element_text(size = 15))  # x-axis tick mark text size
  assign(plot_name, p, envir = .GlobalEnv)  # create global variable
}

## Other analyses

# Conduct moderation analyses
moderation_analysis <- function(df, moderator, apa_freq, apa_bf) {
  # Substitute variable names to manipulate in function
  var <- eval(substitute(moderator), df)
  var_name <- substitute(var)

  # Calculate means
  mean <- with(df, mean(var_name))  # moderator mean
  dfmc <- df %>%
    mutate(mc = var_name - mean) %>%  # mean center moderator
    drop_na(cog_diff_comp)  # drop NA cases from the outcome variable
  cat(paste0("Rows in df: ", nrow(dfmc), "\n", "Moderator mean: ", round(mean(dfmc$mc), digits = 4)))  # diagnostic check on the model

  # Computer moderation model
  mod <- with(dfmc, lm(cog_diff_comp ~ condition * mc))  # run interaction model
  print(summary(mod))
  assign(apa_freq, with(dfmc, apa_print(lm(cog_diff_comp ~ condition * mc))), envir = .GlobalEnv)  # create manuscript-ready object with apa_print

  # Computer Bayes factor
  full <- lmBF(cog_diff_comp ~ condition + mc + condition:mc, data = dfmc)  # calculate Bayes factor for full linear model
  no_interaction <- lmBF(cog_diff_comp ~ condition + mc, data = dfmc)  # calculate Bayes factor for no interaction model
  condition_only <- lmBF(cog_diff_comp ~ condition, data = dfmc)  # calculate Bayes factor for condition-only model
  mod_only <- lmBF(cog_diff_comp ~ mc, data = dfmc)  # calculate Bayes factor for moderator-only model

  # Assemble and print results
  all_bfs <- c(full, no_interaction, condition_only, mod_only)
  print(all_bfs)
  assign(apa_bf, all_bfs, envir = .GlobalEnv)  # create manuscript-ready object with apa_print
  print(full / no_interaction)  # print Bayes factor of interaction (moderation)
}

# Calculate Cronbach's alpha
output_alpha <- function(measure, apa_alpha) {
  # Substitute variable names to manipulate in function
  var <- eval(substitute(measure), survey_item_data)
  var_name <- substitute(var)

  # Filter data to extract survey data of interest
  df <- survey_item_data %>%
    filter(survey == var_name)

  # Calculate Cronbach's alpha
  raw_alpha <- as.numeric(psych::alpha(df)$total[1])
  assign(apa_alpha, raw_alpha, envir = .GlobalEnv)  # assign to global environment
}


# Load and prepare data ----------------------------------------------------

all_data <- read_csv("thayer_stevens_2020_data1.csv")  # import data

# Experiment 1
all_data_1 <- all_data %>%
  filter(experiment == 1) %>%  # extract experiment 1 data
  mutate(condition = as_factor(condition))  # convert to factor

hai_data_1 <- all_data_1 %>%  # tibble for HAI data
  filter(condition == "hai")

control_data_1 <- all_data_1 %>%  # tibble for control data
  filter(condition == "control")

# Experiment 2
all_data_2_all <- all_data %>%
  filter(experiment == 2) %>%  # extract experiment 1 data
  mutate(condition = as_factor(condition))  # convert to factor
all_data_2 <- all_data_2_all %>%
  filter(participant != 132) # exclude participant who did not receive intervention

hai_data_2 <- all_data_2 %>%  # tibble for HAI data
  filter(condition == "hai")

control_data_2 <- all_data_2 %>%  # tibble for control data
  filter(condition == "control")

# Survey item data
survey_item_data <- read_csv("thayer_stevens_2020_data2.csv")


# EXPERIMENT 1 ----
# Summary table that populates while running the script; these will not look as expected at outset if running analysis functions out of order (i.e., any order that would deviate from Source/run all)
summary_1 <- data.frame(matrix(NA, ncol = 10))  # initialize data frame
names(summary_1) <- c("measure", "test_type_1", "n_1", "df_1", "test_statistic_1", "hai_mean_1", "control_mean_1", "ci_1", "p_value_1", "bf_1")  # rename columns

# Baseline table that populates while running the script
baseline_1 <- data.frame(matrix(NA, ncol = 10))  # initialize data frame
names(baseline_1) <- c("measure", "test_type_1", "n_1", "df_1", "test_statistic_1", "hai_mean_1", "control_mean_1", "ci_1", "p_value_1", "bf_1")  # rename columns

# Pet-related measures ----
# Pet attitude scale
output_alpha("pas_1", "alpha_pas_1")  # calculate Cronbach's alpha

assumption_check(all_data_1, pas)  # check assumptions of HAI/control baseline comparison: fail; no NAs; use Wilcoxon rank sum test
paired_analysis(all_data_1, pas, "pas", "w", 1, "pre", "apa_pas_1", "apa_pas_1_BF", "NA")  # conduct HAI/control baseline comparison

# Pets own now
pets_now_1 <- c("pets_now", NA, NA, NA, NA, sum(hai_data_1$pets_now), sum(control_data_1$pets_now), NA, NA, NA, NA)  # find number and percent 'yes'
baseline_1 <- rbind(baseline_1, pets_now_1)  # bind to summary table

# Pets own as a child
pets_child_1 <- c("pets_child", NA, NA, NA, NA, sum(hai_data_1$pets_child), sum(control_data_1$pets_child), NA, NA, NA, NA)  # find number and percent 'yes'
baseline_1 <- rbind(baseline_1, pets_child_1)  # bind to summary table

# Affect measures ----
# _Positive PANAS ----
output_alpha("panas_pos_1", "alpha_panas_pos_1")  # calculate Cronbach's alpha

## Baseline
assumption_check(all_data_1, panas_pre_pos)  # check assumptions of HAI/control baseline comparison: conforms to model assumptions; no NAs
paired_analysis(all_data_1, panas_pre_pos, "panas_pre_pos", "t", 1, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

## ANCOVA
assumption_check_ancova(all_data_1, panas_pre_pos, panas_post_pos)  # check ANCOVA assumptions: interaction term is significant at p = .026 (i.e., violates assumption of homogeneity of regression slopes) but model fit worsens with interaction term in model
ancova_analysis(all_data_1, "panas_pos_post", panas_post_pos, panas_pre_pos, "panas_pos_res_1", "panas_pos_means_1", "panas_pos_bf_1", 1)  # calculate ANCOVA

# _Negative PANAS ----
output_alpha("panas_neg_1", "alpha_panas_neg_1")  # calculate Cronbach's alpha

## Baseline
assumption_check(all_data_1, panas_pre_neg)  # check assumptions of HAI/control baseline comparison: fail; no NAs; use Wilcoxon rank sum test
paired_analysis(all_data_1, panas_pre_neg, "panas_pre_neg", "w", 1, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

## ANCOVA
assumption_check_ancova(all_data_1, panas_pre_neg, panas_pre_neg)  # check ANCOVA assumptions: data extremely violate normally distributed residuals and equal variance assumptions and require transformation

# Log transform data
neg_1 <- all_data_1 %>%
  mutate(panas_pre_log = log(panas_pre_neg),  # log transform
         panas_post_log = log(panas_post_neg))

assumption_check_ancova(neg_1, panas_pre_log, panas_post_log)  # check ANCOVA assumptions: now conforms to model assumptions
ancova_analysis(neg_1, "panas_neg_post", panas_post_log, panas_pre_log, "panas_neg_res_1", "panas_neg_means_1", "panas_neg_bf_1", 1)  # calculate ANCOVA

# Cognition ----
# _DRM (long-term memory) ----
drm_1 <- all_data_1 %>%  # prepare data
  select(participant, condition, drm_d_prime) %>%  # select columns
  drop_na()  # assumption check shows NAs, remove here
assumption_check(all_data_1, drm_d_prime)  # check assumptions of HAI/control baseline comparison: fail; dataframe has NAs; use Wilcoxon rank sum test
paired_analysis(drm_1, drm_d_prime, "drm_d_prime", "w", 1, "diff", "apa_drm_d_prime_1", "apa_drm_d_prime_1_BF", "apa_drm_d_prime_1_es")  # conduct HAI/control baseline comparison

# _Necker cube (attentional control) ----
## Baseline
ncpc_1 <- all_data_1 %>%  # prepare data
  select(participant, condition, ncpc_pre_diff, ncpc_post_diff) %>%  # select columns
  drop_na(ncpc_pre_diff)  # remove NAs

assumption_check(all_data_1, ncpc_pre_diff)  # check assumptions of HAI/control baseline comparison: conforms to model assumptions; dataframe has NAs
paired_analysis(ncpc_1, ncpc_pre_diff, "ncpc_pre_diff", "t", 1, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

## ANCOVA
assumption_check_ancova(all_data_1, ncpc_pre_diff, ncpc_post_diff)  # check ANCOVA assumptions: data extremely violate normally distributed residuals and slightly violand equal variance assumptions, so extreme outliers  (>3 x SD) must be removed

# Remove outliers
ncpc_post_mean_1 <- mean(all_data_1$ncpc_post_diff, na.rm = TRUE)  # calculate mean
ncpc_post_sd_1 <- sd(all_data_1$ncpc_post_diff, na.rm = TRUE)  # calculate SD
ncpc_1_trimmed <- all_data_1 %>%  # prepare data
  select(participant, condition, ncpc_pre_diff, ncpc_post_diff) %>%  # select columns
  filter(ncpc_post_diff > ncpc_post_mean_1 - (ncpc_post_sd_1 * 3)) %>%  # filter out outliers (>3xSD)
  drop_na(ncpc_pre_diff)  # remove NAs

assumption_check_ancova(ncpc_1_trimmed, ncpc_pre_diff, ncpc_post_diff)  # check ANCOVA assumptions: conforms to model assumptions
ancova_analysis(ncpc_1, "ncpc_diff_post", ncpc_post_diff, ncpc_pre_diff, "ncpc_res_1", "ncpc_means_1", "ncpc_bf_1", 1)  # calculate ANCOVA

# _Backwards digit span (working memory) ----
bds_1 <- all_data_1 %>%  # prepare data
  select(participant, condition, bds_index_pre, bds_index_post) %>%  # select columns
  drop_na()  # remove NAs

## Baseline
assumption_check(all_data_1, bds_index_pre)  # check assumptions of HAI/control baseline comparison: fails; dataframe has NAs; use Wilcoxon rank sum test
paired_analysis(bds_1, bds_index_pre, "bds_pre", "w", 1, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

## ANCOVA
assumption_check_ancova(all_data_1, bds_index_pre, bds_index_post)  # check ANCOVA assumptions: visual inspection of mod$residuals plot indicates moderate violation that model should be robust to
ancova_analysis(bds_1, "bds_post", bds_index_post, bds_index_pre, "bds_res_1", "bds_means_1", "bds_bf_1", 1)  # calculate ANCOVA

# _N-back  (working memory) ----
nback_1 <- all_data_1 %>%  # prepare data
  select(participant, condition, nback_d_prime_pre, nback_d_prime_post) %>%  # select columns
  drop_na()  # remove NAs

## Baseline
assumption_check(all_data_1, nback_d_prime_pre)  # check assumptions of HAI/control baseline comparison: fails; dataframe has NAs; run Wilcoxon rank sum test
paired_analysis(all_data_1, nback_d_prime_pre, "nback_d_prime_pre", "w", 1, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

## ANCOVA
assumption_check_ancova(all_data_1, nback_d_prime_pre, nback_d_prime_post)  # check ANCOVA assumptions: visual inspection of qqplot indicates moderate violation that model should be robust to
ancova_analysis(nback_1, "nback_d_prime_post", nback_d_prime_post, nback_d_prime_pre, "nback_res_1", "nback_means_1", "nback_bf_1", 1)  # calculate ANCOVA

# Moderators ----
# Prepare data for moderation analysis
prep_cog_comp_1 <- all_data_1 %>%
  select(participant, condition, panas_pos_diff, panas_neg_diff, ncpc_pre_diff, ncpc_post_diff, ncpc_diff, nback_d_prime_pre, nback_d_prime_post, nback_d_prime_diff, bds_index_pre, bds_index_post, bds_index_diff, pas) %>%  # select columns
  drop_na()  # remove NAs

# Calculate grand means/sds across condition and time
cog_comp_means_1 <- prep_cog_comp_1 %>%
  select(-participant, -condition) %>%  # remove unnecessary columns
  pivot_longer(everything(), names_to = "measure") %>%  # pivot data longer
  group_by(measure) %>%   # for each measure
  summarise(mean = mean(value), sd = sd(value))  # calculate mean and sd

# Generate composite cognitive score by averaging z scores
cog_comp_1 <- prep_cog_comp_1 %>%
  mutate(nback_z_diff = (nback_d_prime_diff - mean(nback_d_prime_diff)) / sd(nback_d_prime_diff), # calculate z scores
         ncpc_z_diff = (ncpc_diff - mean(ncpc_diff)) / sd(ncpc_diff),
         bds_z_diff = (bds_index_diff - mean(bds_index_diff)) / sd(bds_index_diff),
         cog_diff_comp = (nback_z_diff + ncpc_z_diff + bds_z_diff) / 3)  # calculate mean for composite cognitive score

# Conduct moderation analyses
moderation_analysis(cog_comp_1, pas, "apa_pas_mod_1", "apa_pas_mod_1_BF")
moderation_analysis(cog_comp_1, panas_pos_diff, "apa_pos_mod_1", "apa_pos_mod_1_BF")
moderation_analysis(cog_comp_1, panas_neg_diff, "apa_neg_mod_1", "apa_neg_mod_1_BF")

# HAI Exploratory ----
# Prepare data
animal_mat_1 <- hai_data_1 %>%
  select(participant, pas, dog_discomfort_revkey, dog_ambivalence_revkey, dog_desire_to_interact, dog_comfort, pets_now, pets_child, panas_pos_diff, panas_neg_diff, nback_d_prime_diff, ncpc_diff, bds_index_diff) %>%  # select HAI data
  left_join(cog_comp_1) %>%  # add composite cognitive score
  select("Pet attitude" = pas, "HAI-Discomfort" = dog_discomfort_revkey, "HAI-Ambivalence" = dog_ambivalence_revkey, "HAI-Desire to interact" = dog_desire_to_interact, "HAI - Comfort" = dog_comfort, "Cognitive composite" = "cog_diff_comp",  "Pos. affect change" = panas_pos_diff, "Neg. affect change" = panas_neg_diff)  # rename columns

# Calculate correlation matrix
hai_corr_1 <- Hmisc::rcorr(as.matrix(animal_mat_1))  # calculate r and p for correlation matrix
hai_corr_1_r <- hai_corr_1$r  # extract r
hai_corr_1_p <- hai_corr_1$P  # extract p

# Plot correlation matrix
hai_corrplot_1 <- ggcorrplot(hai_corr_1_r, type = "upper", hc.order = FALSE, p.mat = hai_corr_1_p, lab = TRUE, insig  = "blank", ggtheme = ggplot2::theme_classic(base_size = 12))


# PLOTS ----
## Affect ----
# _Positive PANAS ----
## Plot predicted post data
predicted_plot(all_data_1, panas_pre_pos, panas_post_pos, "Positive affect rating", "panas_pos_post_1")
## Plot pre-post data
prepost_plot(all_data_1, "panas_pre_pos", "panas_post_pos", "Positive affect rating", "panas_pos_prepost_1")

# _Negative PANAS ----
## Plot predicted post data
predicted_plot(neg_1, panas_pre_log, panas_post_log, "Negative affect rating", "panas_neg_post_1")
## Plot pre-post data
prepost_plot(all_data_1, "panas_pre_neg", "panas_post_neg", "Negative affect rating", "panas_neg_prepost_1")

# Generate combined affect figure
panas_pos_post_1 + panas_neg_post_1 +   # combine plots into figure
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &  # add subfigure labels
  theme(plot.tag = element_text(size = 20))  # set font size
ggsave("figures/affect_1.png", width = 10, height = 5)  # save figure
panas_pos_prepost_1 + panas_neg_prepost_1 +   # combine plots into figure
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &  # add subfigure labels
  theme(plot.tag = element_text(size = 20))  # set font size
ggsave("figures/affect_prepost_1.png", width = 10, height = 5)  # save figure


# Cognition ----
# _DRM (long-term memory) ----
# Find number of HAI and control data points
num_hai <- table(drm_1$condition)[2]
num_control <- table(drm_1$condition)[1]

# Generate post plot
drm_1_plot <- drm_1 %>%
  ggplot(aes(x = condition, y = drm_d_prime, color = condition, shape = condition)) +
  geom_beeswarm(size = 3.1, alpha = 0.4, shape = c(rep(2, num_control), rep(1, num_hai)), cex = 3, dodge.width = 0.2) +  # plot individual points
  stat_summary(fun.data = mean_cl_normal, color = cb_palette_black[c(5, 6)]) +  # plot means and CIs
  labs(y = "Long-term memory d'") +  # label y-axis title
  scale_color_manual(breaks = c("control", "hai"), values = cb_palette_black[c(5, 6)]) +  # set group color
  scale_shape_manual(values = c(17, 19)) +  # change data point shape
  scale_x_discrete(breaks = c("control", "hai"), labels = c("Control", "HAI")) +  # set x-axis tick labels
  theme_minimal() +  # set theme
  theme(legend.position = "none",  # remove legend
        axis.title.x = element_blank(),  # remove x-axis title
        axis.title.y = element_text(size = 20),  # y-axis title text size
        axis.text.x = element_text(size = 20),  # x-axis tick mark text size
        axis.text.y = element_text(size = 15))  # x-axis tick mark text size

# _Necker cube (attentional control) ----
## Plot predicted post data
predicted_plot(ncpc_1, ncpc_pre_diff, ncpc_post_diff, "Difference in number of shifts", "ncpc_diff_post_1")
## Plot pre-post data
prepost_plot(ncpc_1, "ncpc_pre_diff", "ncpc_post_diff", "Difference in number of shifts", "ncpc_diff_prepost_1")

# _Backwards digit span (working memory) ----
## Plot predicted post data
predicted_plot(bds_1, bds_index_pre, bds_index_post, "Backwards digit span index", "bds_post_1")
## Plot pre-post data
prepost_plot(bds_1, "bds_index_pre", "bds_index_post", "Backwards digit span index", "bds_prepost_1")

# _N-back (working memory) ----
## Plot predicted post data
predicted_plot(nback_1, nback_d_prime_pre, nback_d_prime_post, "N-back d'", "nback_d_prime_post_1")
## Plot pre-post data
prepost_plot(nback_1, "nback_d_prime_pre", "nback_d_prime_post", "N-back d'", "nback_d_prime_prepost_1")

# Generate combined cognitive figure
ncpc_diff_post_1 + bds_post_1 + nback_d_prime_post_1 +  # combine plots into figure
  plot_layout(ncol = 2) +  # use two columns
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &  # add subfigure labels
  theme(plot.tag = element_text(size = 20))  # set font size
ggsave("figures/cognitive_1.png", width = 10, height = 10)  # save figure
drm_1_plot + ncpc_diff_prepost_1 + bds_prepost_1 + nback_d_prime_prepost_1 +  # combine plots into figure
  plot_layout(ncol = 2) +   # use two columns
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &  # add subfigure labels
  theme(plot.tag = element_text(size = 20))  # set font size
ggsave("figures/cognitive_prepost_1.png", width = 10, height = 10)  # save figure


# EXPERIMENT 2 ----
# Summary table
summary_2 <- data.frame(matrix(NA, ncol = 10))  # initialize data frame
names(summary_2) <- c("measure", "test_type_2", "n_2", "df_2", "test_statistic_2", "hai_mean_2", "control_mean_2", "ci_2", "p_value_2", "bf_2")  # rename columns

# Baseline table
baseline_2 <- data.frame(matrix(NA, ncol = 10))  # initialize data frame
names(baseline_2) <- c("measure", "test_type_2", "n_2", "df_2", "test_statistic_2", "hai_mean_2", "control_mean_2", "ci_2", "p_value_2", "bf_2")  # rename columns

# Pet-related measures ----
# Pet attitude
output_alpha("pas_2", "alpha_pas_2")  # calculate Cronbach's alpha
assumption_check(all_data_2, pas)  # check assumptions of HAI/control baseline comparison: fails; use Wilcoxon rank sum test
paired_analysis(all_data_2, pas, "pas", "w", 2, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

# Pets own now
pets_now_2 <- c("pets_now", NA, NA, NA, NA, sum(hai_data_2$pets_now), sum(control_data_2$pets_now), NA, NA, NA, NA)  # find number and percent 'yes'
baseline_2 <- rbind(baseline_2, pets_now_2)  # bind to summary table

# Pets own as a child
pets_child_2 <- c("pets_child", NA, NA, NA, NA, sum(hai_data_2$pets_child), sum(control_data_2$pets_child), NA, NA, NA, NA)  # find number and percent 'yes'
baseline_2 <- rbind(baseline_2, pets_child_2)  # bind to summary table

# Affect measures ----
# _Positive PANAS ----
output_alpha("panas_pos_2", "alpha_panas_pos_2")  # calculate Cronbach's alpha

## Baseline
assumption_check(all_data_2, panas_pre_pos)  # check assumptions of HAI/control baseline comparison: pass; no NAs
paired_analysis(all_data_2, panas_pre_pos, "panas_pre_pos", "t", 2, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

## ANCOVA
assumption_check_ancova(all_data_2, panas_pre_pos, panas_post_pos)  # check ANCOVA assumptions: conforms to model assumptions
ancova_analysis(all_data_2, "panas_pos_post", panas_post_pos, panas_pre_pos, "panas_pos_res_2", "panas_pos_means_2", "panas_pos_bf_2", 2)  # calculate ANCOVA

# _Negative PANAS -----
output_alpha("panas_neg_2", "alpha_panas_neg_2")  # calculate Cronbach's alpha

## Baseline
assumption_check(all_data_2, panas_pre_neg)  # check assumptions of HAI/control baseline comparison: fail; no NAs; use Wilcoxon rank sum test
paired_analysis(all_data_2, panas_pre_neg, "panas_pre_neg", "w", 2, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

## ANCOVA
assumption_check_ancova(all_data_2, panas_pre_neg, panas_post_neg)  # check ANCOVA assumptions: data extremely violate normally distributed residuals and equal variance assumptions and require transformation and removal of extreme outlier  (>3 x SD)

# Remove outliers
panas_post_mean_2 <- mean(all_data_2$panas_post_neg)  # calculate overall mean
panas_post_sd_2 <- sd(all_data_2$panas_post_neg)  # calculate overall SD
neg_2 <- all_data_2 %>%
  filter(panas_post_neg < panas_post_mean_2 + panas_post_sd_2 * 3) %>%   # filter out outliers (>3 x SD)
  mutate(panas_pre_log = log(panas_pre_neg),  # log transform
         panas_post_log = log(panas_post_neg))

assumption_check_ancova(neg_2, panas_pre_log, panas_post_log)  # check ANCOVA assumptions: visual inspection of qqplot and mod$residuals plot indicate moderate violations that model should be robust to
ancova_analysis(neg_2, "panas_neg_post", panas_post_log, panas_pre_log, "panas_neg_res_2", "panas_neg_means_2", "panas_neg_bf_2", 2)  # calculate ANCOVA

# _Anxiety ----
## Baseline
assumption_check(all_data_2, vas_anxiety_pre)  # check assumptions of HAI/control baseline comparison: fail; no NAs; use Wilcoxon rank sum test
paired_analysis(all_data_2, vas_anxiety_pre, "vas_anxiety_pre", "w", 2, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

## ANCOVA
assumption_check_ancova(all_data_2, vas_anxiety_pre, vas_anxiety_post)  # check ANCOVA assumptions: interaction term is significant p = .0018 (i.e., violates assumption of homogeneity of regression slopes) but model fit worsens with interaction term in model
ancova_analysis(all_data_2, "vas_anx_post", vas_anxiety_post, vas_anxiety_pre, "vas_anx_res_2", "vas_anx_means_2", "vas_anx_bf_2", 2)  # calculate ANCOVA

# _Stress ----
## Baseline
assumption_check(all_data_2, vas_stress_pre)  # check assumptions of HAI/control baseline comparison: fail; no NAs; use Wilcoxon rank sum test
paired_analysis(all_data_2, vas_stress_pre, "vas_stress_pre", "w", 2, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

## ANCOVA
assumption_check_ancova(all_data_2, vas_stress_pre, vas_stress_post)  # check ANCOVA assumptions: conforms to model assumptions
ancova_analysis(all_data_2, "vas_stress_post", vas_stress_post, vas_stress_pre, "vas_stress_res_2", "vas_stress_means_2", "vas_stress_bf_2", 2)  # calculate ANCOVA

# _STAI ----
## State
output_alpha("stai_state", "alpha_state")  # calculate Cronbach's alpha
assumption_check(all_data_2, stai_state)  # check assumptions of HAI/control baseline comparison: fail short of extremely; no NAs; use Wilcoxon rank sum test
paired_analysis(all_data_2, stai_state, "stai_state", "w", 2, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

## Trait
output_alpha("stai_trait", "alpha_trait")  # calculate Cronbach's alpha
assumption_check(all_data_2, stai_trait)  # check assumptions of HAI/control baseline comparison: fail; no NAs; use Wilcoxon rank sum test
paired_analysis(all_data_2, stai_trait, "stai_trait", "w", 2, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison


# Cognition ----
# _DRM (long-term memory) ----
drm_2 <- all_data_2 %>%  # prepare data
  select(participant, condition,  drm_d_prime) %>%  # select columns
  drop_na()  # remove NAs
assumption_check(all_data_2, drm_d_prime)  # check assumptions of HAI/control baseline comparison: fail; dataframe has NAs; use Wilcoxon rank sum test
paired_analysis(drm_2, drm_d_prime, "drm_d_prime", "w", 2, "diff", "apa_drm_d_prime_2", "apa_drm_d_prime_2_BF", "apa_drm_d_prime_2_es")  # conduct HAI/control baseline comparison

# _Necker cube (attentional control) ----
ncpc_2 <- all_data_2 %>%   # prepare data
  select(participant, condition, ncpc_pre_diff, ncpc_post_diff) %>%  # select columns
  drop_na()  # remove NAs

## Baseline
assumption_check(all_data_2, ncpc_pre_diff)  # check assumptions of HAI/control baseline comparison: fail; dataframe has NAs; use Wilcoxon sign rank test
paired_analysis(ncpc_2, ncpc_pre_diff, "ncpc_pre_diff", "w", 2, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

## ANCOVA
assumption_check_ancova(all_data_2, ncpc_pre_diff, ncpc_post_diff)  # check ANCOVA assumptions: conforms to model assumptions
ancova_analysis(ncpc_2, "ncpc_diff_post", ncpc_post_diff, ncpc_pre_diff, "ncpc_res_2", "ncpc_means_2", "ncpc_bf_2", 2)  # calculate ANCOVA

# _Backwards digit span (working memory) ----
bds_2 <- all_data_2 %>%   # prepare data
  select(participant, condition, bds_index_pre, bds_index_post) %>%  # select columns
  drop_na()  # remove NAs

## Baseline
assumption_check(all_data_2, bds_index_pre)  # check assumptions of HAI/control baseline comparison: fail; dataframe has NAs; use Wilcoxon sign rank test
paired_analysis(bds_2, bds_index_pre, "bds_pre", "w", 2, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

## ANCOVA
assumption_check_ancova(all_data_2, bds_index_pre, bds_index_post)  # check ANCOVA assumptions: conforms to model assumptions
ancova_analysis(bds_2, "bds_post", bds_index_post, bds_index_pre, "bds_res_2", "bds_means_2", "bds_bf_2", 2)  # calculate ANCOVA

# _N-back (working memory) ----
nback_2 <- all_data_2 %>%   # prepare data
  select(participant, condition, nback_d_prime_pre, nback_d_prime_post) %>%  # select columns
  drop_na()  # remove NAs

## Baseline
assumption_check(all_data_2, nback_d_prime_pre)  # check assumptions of HAI/control baseline comparison: fail; dataframe has NAs; use Wilcoxon sign rank test
paired_analysis(nback_2, nback_d_prime_pre, "nback_d_prime_pre", "t", 2, "pre", "NA", "NA", "NA")  # conduct HAI/control baseline comparison

## ANCOVA
assumption_check_ancova(all_data_2, nback_d_prime_pre, nback_d_prime_post)  # check ANCOVA assumptions: violate normally distributed residuals assumption but square-root transformation not useful
ancova_analysis(nback_2, "nback_d_prime_post", nback_d_prime_post, nback_d_prime_pre, "nback_res_2", "nback_means_2", "nback_bf_2", 2)  # calculate ANCOVA


# Moderators ----
# Prepare data for moderation analysis
prep_cog_comp_2 <- all_data_2 %>%
  select(participant, condition, ncpc_diff, nback_d_prime_diff, bds_index_diff, pas, panas_pos_diff, panas_neg_diff, stai_state, stai_trait, vas_anxiety_pre, vas_anxiety_post, vas_stress_pre, vas_stress_post) %>%  # select columns
  mutate(anx_cond_diff = vas_anxiety_post - vas_anxiety_pre, stress_cond_diff = vas_stress_post - vas_stress_pre) %>%  # calculate difference scores
  drop_na()  # remove NAs

# Calculate grand means/sds across condition and time
cog_comp_means_2 <- prep_cog_comp_2 %>%  # prep Z scores with grand means/sds across condition and time
  select(-participant, -condition) %>%  # remove unnecessary columns
  pivot_longer(everything(), names_to = "measure") %>%  # pivot data longer
  group_by(measure) %>%   # for each measure
  summarise(mean = mean(value), sd = sd(value))  # calculate mean and sd
# Generate composite cognitive score by averaging z scores
cog_comp_2 <- prep_cog_comp_2 %>%
  mutate(nback_z_diff = (nback_d_prime_diff - mean(nback_d_prime_diff)) / sd(nback_d_prime_diff), # calculate z scores
         ncpc_z_diff = (ncpc_diff - mean(ncpc_diff)) / sd(ncpc_diff),
         bds_z_diff = (bds_index_diff - mean(bds_index_diff)) / sd(bds_index_diff),
         cog_diff_comp = (nback_z_diff + ncpc_z_diff + bds_z_diff) / 3)  # calculate mean for composite cognitive score

# Conduct moderation analyses
moderation_analysis(cog_comp_2, pas, "apa_pas_mod_2", "apa_pas_mod_2_BF")
moderation_analysis(cog_comp_2, panas_pos_diff, "apa_pos_mod_2", "apa_pos_mod_2_BF")
moderation_analysis(cog_comp_2, panas_neg_diff, "apa_neg_mod_2", "apa_neg_mod_2_BF")
moderation_analysis(cog_comp_2, stai_state, "apa_stai_state_mod_2", "apa_stai_state_mod_2_BF")
moderation_analysis(cog_comp_2, stai_trait, "apa_stai_trait_mod_2", "apa_stai_trait_mod_2_BF")
moderation_analysis(cog_comp_2, anx_cond_diff, "apa_anx_pre_end_diff_mod_2", "apa_anx_pre_end_diff_mod_2_BF")
moderation_analysis(cog_comp_2, stress_cond_diff, "apa_stress_pre_end_diff_mod_2", "apa_stress_pre_end_diff_mod_2_BF")

# HAI Exploratory ----
# Prepare data
animal_mat_2 <- hai_data_2 %>%
  select(participant, pas, dog_discomfort_revkey, dog_ambivalence_revkey, dog_desire_to_interact, dog_comfort, duration_interaction, pets_now, pets_child, panas_pos_diff, panas_neg_diff,  nback_d_prime_diff, ncpc_diff, bds_index_diff) %>%  # select HAI data
  left_join(cog_comp_2) %>%  # add composite cognitive score
  select("Pet attitude" = pas, "HAI-Discomfort" = dog_discomfort_revkey, "HAI-Ambivalence" = dog_ambivalence_revkey, "HAI-Desire to interact" = dog_desire_to_interact, "HAI-Comfort" = dog_comfort, "HAI-Duration" = duration_interaction, "Cognitive composite" = "cog_diff_comp",  "Pos. affect change" = panas_pos_diff, "Neg. affect change" = panas_neg_diff, "STAI-State" = stai_state, "STAI-Trait" = stai_trait, "VAS-Anxiety change" = anx_cond_diff, "VAS-Stress change" = stress_cond_diff)  # rename columns

# Calculate correlation matrix
hai_corr_2 <- Hmisc::rcorr(as.matrix(animal_mat_2))  # calculate r and p for correlation matrix
hai_corr_2_r <- round(hai_corr_2$r, 1)  # extract r
hai_corr_2_p <- hai_corr_2$P  # extract p

# Plot correlation matrix
hai_corrplot_2 <- ggcorrplot(hai_corr_2_r, type = "upper", hc.order = FALSE, p.mat = hai_corr_2_p, lab = TRUE, insig = "blank", show.legend = FALSE, ggtheme = ggplot2::theme_classic(base_size = 4))

# Generate combined HAI figure
hai_corrplot_1 + hai_corrplot_2 +  # combine plots into figure
  plot_layout(ncol = 1) +  # use one column
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &  # add subfigure labels
  theme(plot.tag = element_text(size = 20))  # set font size
ggsave("figures/correlation_plots.png", height = 12, width = 8)  # save figure


# PLOT ----
## Affect ----
# _Positive PANAS ----
## Plot predicted post data
predicted_plot(all_data_2, panas_pre_pos, panas_post_pos, "Positive affect rating", "panas_pos_post_2")
## Plot pre-post data
prepost_plot(all_data_2, "panas_pre_pos", "panas_post_pos", "Positive affect rating", "panas_pos_prepost_2")

# _Negative PANAS ----
## Plot predicted post data
predicted_plot(neg_2, panas_pre_log, panas_post_log, "Negative affect rating", "panas_neg_post_2")
## Plot pre-post data
prepost_plot(all_data_2, "panas_pre_neg", "panas_post_neg", "Negative affect rating", "panas_neg_prepost_2")

# _Anxiety ----
## Plot predicted post data
predicted_plot(all_data_2, vas_anxiety_pre, vas_anxiety_post, "Anxiety rating", "vas_anx_post_2")
## Plot pre-post data
prepost_plot(all_data_2, "vas_anxiety_pre", "vas_anxiety_post", "Anxiety rating", "vas_anx_prepost_2")

# _Stress ----
## Plot predicted post data
predicted_plot(all_data_2, vas_stress_pre, vas_stress_post, "Stress rating", "vas_stress_post_2")
## Plot pre-post data
prepost_plot(all_data_2, "vas_stress_pre", "vas_stress_post", "Stress rating", "vas_stress_prepost_2")

# Generate combined affect figure
panas_pos_post_2 + panas_neg_post_2 + vas_anx_post_2 + vas_stress_post_2 +   # combine plots into figure
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &  # add subfigure labels
  theme(plot.tag = element_text(size = 20))  # set font size
ggsave("figures/affect_2.png", width = 10, height = 10)  # save figure
panas_pos_prepost_2 + panas_neg_prepost_2 + vas_anx_prepost_2 + vas_stress_prepost_2 + # combine plots into figure
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &  # add subfigure labels
  theme(plot.tag = element_text(size = 20))  # set font size
ggsave("figures/affect_prepost_2.png", width = 10, height = 10)  # save figure


# Cognition ----
# _DRM (long-term memory) ----
# Find number of HAI and control data points
num_hai <- table(drm_2$condition)[2]
num_control <- table(drm_2$condition)[1]

# Generate post plot
drm_2_plot <- drm_2 %>%  # manually plot
  ggplot(aes(x = condition, y = drm_d_prime, color = condition, shape = condition)) +
  geom_beeswarm(size = 3.1, alpha = 0.4, shape = c(rep(2, num_control), rep(1, num_hai)), cex = 3, dodge.width = 0.2) +  # plot individual points
  stat_summary(fun.data = mean_cl_normal, color = cb_palette_black[c(5, 6)]) +  # plot means and CIs
  labs(y = "Long-term memory d'") +  # label y-axis title
  scale_color_manual(breaks = c("control", "hai"), values = cb_palette_black[c(5, 6)]) +  # set group color
  scale_shape_manual(values = c(17, 19)) +  # change data point shape
  scale_x_discrete(breaks = c("control", "hai"), labels = c("Control", "HAI")) +  # set x-axis tick labels
  theme_minimal() +  # set theme
  theme(legend.position = "none",  # remove legend
        axis.title.x = element_blank(),  # remove x-axis title
        axis.title.y = element_text(size = 20),  # y-axis title text size
        axis.text.x = element_text(size = 20),  # x-axis tick mark text size
        axis.text.y = element_text(size = 15))  # x-axis tick mark text size

# _Necker cube (attentional control) ----
## Plot predicted post data
predicted_plot(ncpc_2, ncpc_pre_diff, ncpc_post_diff, "Difference in number of shifts", "ncpc_diff_post_2")
## Plot pre-post data
prepost_plot(ncpc_2, "ncpc_pre_diff", "ncpc_post_diff", "Difference in number of shifts", "ncpc_diff_prepost_2")

# _Backwards digit span (working memory) ----
## Plot predicted post data
predicted_plot(bds_2, bds_index_pre, bds_index_post, "Backwards digit span index", "bds_post_2")
## Plot pre-post data
prepost_plot(bds_2, "bds_index_pre", "bds_index_post", "Backwards digit span index", "bds_prepost_2")

# _N-back (working memory) ----
## Plot predicted post data
predicted_plot(nback_2, nback_d_prime_pre, nback_d_prime_post, "N-back d'", "nback_d_prime_post_2")
## Plot pre-post data
prepost_plot(nback_2, "nback_d_prime_pre", "nback_d_prime_post", "N-back d'", "nback_d_prime_prepost_2")

# Generate combined cognitive figure
ncpc_diff_post_2 + bds_post_2 + nback_d_prime_post_2 +   # combine plots into figure
  plot_layout(ncol = 2) +  # use two columns
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &  # add subfigure labels
  theme(plot.tag = element_text(size = 20))  # set font size
ggsave("figures/cognitive_2.png", width = 10, height = 10)  # save figure
drm_2_plot + ncpc_diff_prepost_2 + bds_prepost_2 + nback_d_prime_prepost_2 +   # combine plots into figure
  plot_layout(ncol = 2) +  # use two columns
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &  # add subfigure labels
  theme(plot.tag = element_text(size = 20))  # set font size
ggsave("figures/cognitive_prepost_2.png", width = 10, height = 10)  # save figure

# TABLES ----
# Descriptives ----
# Experiment 1
## Gender
all_data_1$gender <- as.factor(all_data_1$gender)  # convert to factor
all_data_1$gender <- factor(all_data_1$gender, levels = c(levels(all_data_1$gender), "Other"))  # add level
describe_gender_n_1 <- as.data.frame(table(all_data_1$gender))  # calculate number of participants within levels
describe_gender_prop_1 <- as.data.frame(prop.table(table(all_data_1$gender))) %>%  # calculate proportions
  select(Var1, percent = Freq)
describe_gender_1 <- describe_gender_n_1 %>%  # combine frequency and proportion
  left_join(describe_gender_prop_1)

## Income
all_data_1$parent_income <- factor(all_data_1$parent_income, levels = c("< $25000", "$25000-$50000", "$50000-$75000", "$75000-$100000", "> $100000", "Preferred not to answer"))  # convert to factor and reorder levels
describe_income_n_1 <- as.data.frame(table(all_data_1$parent_income))  # calculate number of participants within levels
describe_income_prop_1 <- as.data.frame(prop.table(table(all_data_1$parent_income))) %>%   # calculate proportions
  select(Var1, percent = Freq)
describe_income_1 <- describe_income_n_1 %>%   # combine frequency and proportion
  left_join(describe_income_prop_1) %>%
  mutate(Var1 = gsub("$", "\\$", Var1, fixed = TRUE))  # escape $

## Race
all_data_1$race <- as.factor(all_data_1$race)  # convert to factor
all_data_1$race <- factor(all_data_1$race, levels = c(levels(all_data_1$race), "Native American", "Pacific Islander"))  # add levels
all_data_1$race <- factor(all_data_1$race, levels = c("Asian", "Black", "Native American",  "Pacific Islander", "White/Caucasian", "Other"), ordered = TRUE)  # reorder levels
describe_race_n_1 <- as.data.frame(table(all_data_1$race))  # calculate number of participants within levels
describe_race_prop_1 <- as.data.frame(prop.table(table(all_data_1$race))) %>%   # calculate proportions
  select(Var1, percent = Freq)
describe_race_1 <- describe_race_n_1 %>%   # combine frequency and proportion
  left_join(describe_race_prop_1)

## Age
age_1 <- c("Age (M(SD))", format(round(mean(all_data_1$age_num)), nsmall = 1), round(sd(all_data_1$age_num), digits = 1))

## Combine descriptive statistics for demographics
descriptives_1 <- describe_gender_1 %>%
  full_join(describe_race_1) %>%
  full_join(describe_income_1) %>%
  mutate(percent = ifelse(percent == 0, "0", round(percent * 100, digits = 1)))
descriptives_1 <- rbind(age_1, descriptives_1)

# Experiment 2
## Gender
all_data_2$gender <- as.factor(all_data_2$gender)  # convert to factor
all_data_2$gender <- factor(all_data_2$gender, levels = c(levels(all_data_2$gender), "Other"))  # add level
describe_gender_n_2 <- as.data.frame(table(all_data_2$gender))  # calculate number of participants within levels
describe_gender_prop_2 <- as.data.frame(prop.table(table(all_data_2$gender))) %>%   # calculate proportions
  select(Var1, percent = Freq)
describe_gender_2 <- describe_gender_n_2 %>%   # combine frequency and proportion
  left_join(describe_gender_prop_2)

## Income
all_data_2$parent_income <- factor(all_data_2$parent_income, levels = c("< $25000", "$25000-$50000", "$50000-$75000", "$75000-$100000", "> $100000", "Preferred not to answer"))  # convert to factor and reorder levels
describe_income_n_2 <- as.data.frame(table(all_data_2$parent_income))  # calculate number of participants within levels
describe_income_prop_2 <- as.data.frame(prop.table(table(all_data_2$parent_income))) %>%   # calculate proportions
  select(Var1, percent = Freq)
describe_income_2 <- describe_income_n_2 %>%   # combine frequency and proportion
  left_join(describe_income_prop_2) %>%
  mutate(Var1 = gsub("$", "\\$", Var1, fixed = TRUE))

## Race
all_data_2$race <- as.factor(all_data_2$race)  # convert to factor
all_data_2$race <- factor(all_data_2$race, levels = c(levels(all_data_2$race),  "Pacific Islander"))  # add levels
all_data_2$race <- factor(all_data_2$race, levels = c("Asian", "Black", "Native American",  "Pacific Islander", "White/Caucasian", "Other"), ordered = TRUE)  # reorder levels
describe_race_n_2 <- as.data.frame(table(all_data_2$race))  # calculate number of participants within levels
describe_race_prop_2 <- as.data.frame(prop.table(table(all_data_2$race))) %>%   # calculate proportions
  select(Var1, percent = Freq)
describe_race_2 <- describe_race_n_2 %>%   # combine frequency and proportion
  left_join(describe_race_prop_2)

## Age
age_2 <- c("Age (M(SD))", format(round(mean(all_data_2$age_num)), nsmall = 1), round(sd(all_data_2$age_num), digits = 1))

## Combine descriptive statistics for demographics
descriptives_2 <- describe_gender_2 %>%
  full_join(describe_race_2) %>%
  full_join(describe_income_2) %>%
  mutate(percent = ifelse(percent == 0, "0", round(percent * 100, digits = 1))) %>%
  select(Var1, Freq2 = Freq, percent2 = percent)
descriptives_2 <- rbind(age_2, descriptives_2)

# Combine Experiment 1 and 2 descriptive statistics
## Create ID colun
descriptives_1$ID <- seq.int(nrow(descriptives_1))
descriptives_2$ID <- seq.int(nrow(descriptives_2))

## Combine data sets
descriptives <- descriptives_1 %>%
  inner_join(descriptives_2, by = c("Var1", "ID")) %>%
  select(-ID) %>%  # remove column
  mutate("freq_n_1" = str_c(Freq, " (", percent, ")")) %>%  # create columns with percentages
  mutate("freq_n_2" = str_c(Freq2, " (", percent2, ")")) %>%
  select(-Freq, -Freq2, -percent, -percent2) %>%   # remove old columns
  rename(Measure = Var1)  # rename column

# Pre-condition/baseline scores ----
# Combine Experiment 1 and 2 baseline data
baseline_prep <- baseline_2 %>%
  left_join(baseline_1)
baseline_numeric <- c("n_2", "df_2", "test_statistic_2", "hai_mean_2", "control_mean_2", "p_value_2", "bf_2", "n_1", "df_1", "test_statistic_1", "hai_mean_1", "control_mean_1", "p_value_1", "bf_1")  # create vector of numeric columns
baseline_prep <- baseline_prep[-1, ]  # remove initial row
baseline_prep[baseline_numeric] <- lapply(baseline_prep[baseline_numeric], as.numeric)  # convert appropriate rows to numeric
baseline_prep <- baseline_prep %>%  # tidy columns
  mutate_at(vars(-c(measure, test_type_1, test_type_2, ci_1, ci_2, p_value_1, p_value_2)), funs(round(., 2))) %>%  # round non-p, numeric columns
  mutate(p_value_1 = ifelse(p_value_1 < 0.001, "< 0.001", round(p_value_1, 2)),  # round p values
         p_value_2 = ifelse(p_value_2 < 0.001, "< 0.001", round(p_value_2, 2))) %>%
  select(measure, n_1, test_1 = test_type_1, df_1, hai_mean_1, control_mean_1, ci_1, p_1 = p_value_1, bf_1, n_2, test_2 = test_type_2, df_2, hai_mean_2, control_mean_2, ci_2, p_2 = p_value_2, bf_2)  # reorder columns

# Add percentages to pet-related measures
baseline_prep$control_mean_1[baseline_prep$measure == "pets_child"] <- paste(baseline_prep$control_mean_1[baseline_prep$measure == "pets_child"], " (", printnum((sum(control_data_1$pets_child)) / (nrow(control_data_1)) * 100, digits = 1), ")", sep = "")
baseline_prep$control_mean_1[baseline_prep$measure == "pets_now"] <- paste(baseline_prep$control_mean_1[baseline_prep$measure == "pets_now"], " (", printnum((sum(control_data_1$pets_now)) / (nrow(control_data_1)) * 100, digits = 1), ")", sep = "")
baseline_prep$hai_mean_1[baseline_prep$measure == "pets_child"] <- paste(baseline_prep$hai_mean_1[baseline_prep$measure == "pets_child"], " (", printnum((sum(hai_data_1$pets_child)) / (nrow(hai_data_1)) * 100, digits = 1), ")", sep = "")
baseline_prep$hai_mean_1[baseline_prep$measure == "pets_now"] <- paste(baseline_prep$hai_mean_1[baseline_prep$measure == "pets_now"], " (", printnum((sum(hai_data_1$pets_now)) / (nrow(hai_data_1)) * 100, digits = 1), ")", sep = "")
baseline_prep$control_mean_2[baseline_prep$measure == "pets_child"] <- paste(baseline_prep$control_mean_2[baseline_prep$measure == "pets_child"], " (", printnum((sum(control_data_2$pets_child)) / (nrow(control_data_2)) * 100, digits = 1), ")", sep = "")
baseline_prep$control_mean_2[baseline_prep$measure == "pets_now"] <- paste(baseline_prep$control_mean_2[baseline_prep$measure == "pets_now"], " (", printnum((sum(control_data_2$pets_now)) / (nrow(control_data_2)) * 100, digits = 1), ")", sep = "")
baseline_prep$hai_mean_2[baseline_prep$measure == "pets_child"] <- paste(baseline_prep$hai_mean_2[baseline_prep$measure == "pets_child"], " (", printnum((sum(hai_data_2$pets_child)) / (nrow(hai_data_2)) * 100, digits = 1), ")", sep = "")
baseline_prep$hai_mean_2[baseline_prep$measure == "pets_now"] <- paste(baseline_prep$hai_mean_2[baseline_prep$measure == "pets_now"], " (", printnum((sum(hai_data_2$pets_now)) / (nrow(hai_data_2)) * 100, digits = 1), ")", sep = "")

# Generate table
baseline_man <- baseline_prep %>%
  mutate(measure = recode(measure, "pas" = "Pet Attitude (PAS)", "pets_child" = "Pets as a child (N(\\%))", "pets_now" = "Pets now (N(\\%))", "panas_pre_pos" = "Positive affect (PANAS)", "panas_pre_neg" = "Negative affect (PANAS)", "vas_anxiety_pre" = "Anxiety (VAS)", "vas_stress_pre" = "Stress (VAS)", "stai_state" = "State anxiety (STAI)", "stai_trait" = "Trait anxiety (STAI)", "ncpc_pre_diff" = "Average shifts (NCPC)", "bds_pre" = "Index (Digit span)", "nback_d_prime_pre" = "$d'$ (N-back)"))  # recode measures
baseline <- data.frame(baseline_man)  # convert to dataframe
baseline_slim <- baseline %>%  # remove test and df information
  select(-test_1, -df_1, -ci_1, -test_2, -df_2, -ci_2)

# Post-condition/summary scores ----
# Combine Experiment 1 and 2 post-condition data
summary_prep <- summary_2 %>%
  left_join(summary_1, by = "measure") %>%
  dplyr::filter(!measure == "drm_false_recall")
summary_numeric <- c("n_2", "df_2", "test_statistic_2", "hai_mean_2", "control_mean_2", "p_value_2", "bf_2", "n_1", "df_1", "test_statistic_1", "hai_mean_1", "control_mean_1", "p_value_1", "bf_1")  # create vector of numeric columns
summary_prep[summary_numeric] <- lapply(summary_prep[summary_numeric], as.numeric)  # convert appropriate rows to numeric
summary_prep %<>%   # tidy columns
  mutate_at(vars(-c(measure, test_type_1, test_type_2, ci_1, ci_2, p_value_1, p_value_2)), funs(round(., 2))) %>%  # round non-p, numeric columns
  mutate(p_value_1 = ifelse(p_value_1 < 0.01, "< 0.01", round(p_value_1, 2)), # round p-values
         p_value_2 = ifelse(p_value_2 < 0.01, "< 0.01", round(p_value_2, 2)),
         bf_1 = ifelse(bf_1 > 100, "> 100", bf_1),  # round Bayes factors
         bf_2 = ifelse(bf_2 > 100, "> 100", bf_2),
         measure = recode(measure, "panas_pos_post" = "panas_pos", "panas_neg_post" = "panas_neg", "ncpc_diff_post" = "ncpc", "bds_post" = "bds", "nback_d_prime_post" = "nback")) %>%  # recode measures
  select(measure, n_1, test_1 = test_type_1, df_1, hai_mean_1, control_mean_1, ci_1, p_1 = p_value_1, bf_1, n_2, test_2 = test_type_2, df_2, hai_mean_2, control_mean_2, ci_2, p_2 = p_value_2, bf_2) %>%  # reorder columns
  mutate(measure = recode(measure, "panas_pos" = "Positive affect (PANAS)", "panas_neg" = "Log negative affect (PANAS)", "vas_anx_post" = "Anxiety (VAS)", "vas_stress_post" = "Stress (VAS)", "drm_d_prime" = "$d'$ (DRM)", "ncpc" = "Attention shifts (NCPC)", "bds" = "Index (Digit span)", "nback" = "$d'$ (N-back)"))  # recode measuressummary <- data.frame(summary_prep)  # convert to dataframe
summary_slim <- summary_prep %>%   # remove test and df information
  select(-test_1, -df_1, -test_2, -df_2, -ci_1, -ci_2)
