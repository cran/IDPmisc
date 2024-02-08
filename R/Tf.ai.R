Tf.ai <-
    function(pw = NULL, T = NULL, hr = NULL, warn = TRUE) {
        ## Author: Rene Locher
        ## Version 2019-01-30
        Tf1 <- function(pw) {
            num <- 272.62*log(pw/6.112)
            den <- 22.46 - log(pw/6.112)
            return(num/den)
        } ## Tf1

        pmin <- 0.005400077
        pmax <- 6.112
        Tmin <- -65
        Tmax <- 0

        if (!((is.null(pw) & !is.null(T) & !is.null(hr)) |
                  (!is.null(pw) & is.null(T) & is.null(hr))))
            stop("Either pw, or T and hr, must be defined!")

        if (is.null(pw)) {
            x <- T
            x[is.na(x)] <- 0
            if (any(x < Tmin | x > Tmax) & warn)
                warning(paste("Some temperatures are below",
                              Tmin, "and / or above", Tmax, "deg C!"))
            return(Tf1(pw(T)*hr/100))
        } else {
            x <- pw
            x[is.na(x)] <- 1
            if (any(x < pmin | x > pmax) & warn)
                warning(paste("Some vapour pressures are below",
                              pmin,"and / or above", pmax,"hPa!"))
            return(Tf1(pw))
        }
    }
