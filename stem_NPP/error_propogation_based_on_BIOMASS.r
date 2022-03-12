AGBmonteCarlo_no_allometric <- function(D, WD = NULL, errWD = NULL, H = NULL, errH = NULL,
                                        HDmodel = NULL, coord = NULL, Dpropag = NULL, n = 1000,
                                        Carbon = FALSE, Dlim = NULL, plot = NULL, Carbon_sd = 2.06) {
  len <- length(D)
  myrtruncnorm <- function(n, lower = -1, upper = 1, mean = 0,
                           sd = 1) {
    qnorm(runif(n, pnorm(lower, mean = mean, sd = sd), pnorm(upper,
      mean = mean, sd = sd
    )), mean = mean, sd = sd)
  }
  if (!is.null(Dpropag)) {
    if (length(Dpropag) == 1 && tolower(Dpropag) == "chave2004") {
      fivePercent <- round(len * 5 / 100)
      chaveError <- function(x, len) {
        largeErrSample <- sample(len, fivePercent)
        D_sd <- 0.0062 * x + 0.0904
        D_sd[largeErrSample] <- 4.64
        x <- myrtruncnorm(
          n = len, mean = x, sd = D_sd,
          lower = 0.1, upper = 500
        )
        return(x)
      }
      D_simu <- suppressWarnings(replicate(n, chaveError(
        D,
        len
      )))
    } else {
      D_simu <- suppressWarnings(replicate(n, myrtruncnorm(len,
        mean = D, sd = Dpropag, lower = 0.1, upper = 500
      )))
    }
  } else {
    D_simu <- replicate(n, D)
  }
  WD_simu <- suppressWarnings(replicate(n, myrtruncnorm(
    n = len,
    mean = WD, sd = errWD, lower = 0.08, upper = 1.39
  )))
  if (!is.null(HDmodel) | !is.null(H)) {
    if (!is.null(HDmodel)) {
      H_simu <- apply(D_simu, 2, function(x) {
        predictHeight(x,
          model = HDmodel, err = TRUE, plot = plot
        )
      })
    } else {
      upper <- max(H, na.rm = TRUE) + 15
      H_simu <- suppressWarnings(replicate(n, myrtruncnorm(len,
        mean = H, sd = errH, lower = 1.3, upper = upper
      )))
    }

    #  D_simu, H_simu and WD_simu is accounting for the random error in the
    #  measurements of wood density, DBH and tree height


    #  THEN RSE BELOW ACCOUNT FOR THE SYSTEMATIC ERROR FOR THE USE OF ALLOMERIC
    #  EQUATION - EQUATION 4 IN CHAVE 2004, WE SET THIS AS 0 TO REMOVE SYSTEMATIC
    #  ERROR


    #  If you check the manual of BIOMASS, you will see AGB was calculated
    #  with one single equation AGB = 0.0673 *WD*H*D^2*0.976

    # Ebeta below is 0.976, while Ealpha = log (0.0673)

    param_4 <- BIOMASS::param_4
    selec <- sample(1:nrow(param_4), n)
    RSE <- param_4[selec, "sd"]
    RSE[RSE > 0] <- 0
    matRSE <- mapply(function(y) {
      rnorm(sd = y, n = len)
    }, y = RSE)
    Ealpha <- param_4[selec, "intercept"]

    Ebeta <- param_4[selec, "logagbt"]


    Comp <- t(log(WD_simu * H_simu * D_simu^2)) * Ebeta +
      Ealpha

    Comp <- t(log(WD_simu * H_simu * D_simu^2)) * mean(Ebeta) +
      mean(Ealpha)

    Comp <- t(Comp) + matRSE
    AGB_simu <- exp(Comp) / 1000
  }
  if (!is.null(coord)) {
    if (is.null(dim(coord))) {
      coord <- as.matrix(t(coord))
    }
    bioclimParams <- getBioclimParam(coord)
    if (nrow(bioclimParams) == 1) {
      bioclimParams <- bioclimParams[rep(1, len), ]
    }
    param_7 <- BIOMASS::param_7
    selec <- sample(1:nrow(param_7), n)
    RSE <- param_7[selec, "sd"]
    RSE[RSE > 0] <- 0
    param_7 <- colMeans(param_7, na.rm = T)

    Esim <- tcrossprod(as.matrix(param_7[selec, c(
      "temp",
      "prec", "cwd"
    )]), as.matrix(bioclimParams))
    AGB_simu <- t(t(log(WD_simu)) * param_7[selec, "logwsg"] +
      t(log(D_simu)) * param_7[selec, "logdbh"] + t(log(D_simu)^2) *
        param_7[selec, "logdbh2"] + Esim * -param_7[
        selec,
        "E"
      ] + param_7[selec, "intercept"])
    matRSE <- mapply(function(y) {
      rnorm(sd = y, n = len)
    }, y = RSE)
    AGB_simu <- AGB_simu + matRSE
    AGB_simu <- exp(AGB_simu) / 1000
  }
  if (!is.null(Dlim)) {
    AGB_simu[D < Dlim, ] <- 0
  }
  AGB_simu[which(is.infinite(AGB_simu))] <- NA
  if (Carbon == FALSE) {
    sum_AGB_simu <- colSums(AGB_simu, na.rm = TRUE)
    res <- list(
      meanAGB = mean(sum_AGB_simu), medAGB = median(sum_AGB_simu),
      sdAGB = sd(sum_AGB_simu), credibilityAGB = quantile(sum_AGB_simu,
        probs = c(0.025, 0.975)
      ), AGB_simu = AGB_simu
    )
  } else {
    AGC_simu <- AGB_simu * rnorm(
      mean = 47.13, sd = Carbon_sd,
      n = n * len
    ) / 100
    sum_AGC_simu <- colSums(AGC_simu, na.rm = TRUE)
    res <- list(
      meanAGC = mean(sum_AGC_simu), medAGC = median(sum_AGC_simu),
      sdAGC = sd(sum_AGC_simu), credibilityAGC = quantile(sum_AGC_simu,
        probs = c(0.025, 0.975)
      ), AGC_simu = AGC_simu, RSE = RSE, WD_simu = WD_simu, H_simu = H_simu, AGB_simu = AGB_simu
    )
  }
  return(res)
}
