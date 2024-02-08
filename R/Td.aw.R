Td.aw <-
    function(pw = NULL, T = NULL, hr = NULL, warn = TRUE) {
        ## Author: Rene Locher
        ## Version 2019-01-30

        Td1 <- function(pw) {
            num <- 243.12 * log(pw/6.112)
            den <- 17.62 - log(pw/6.112)
            return(num/den)
        } ## Td1

        pmin <- 0.07207549
        pmax <- 199.9329
        Tmin <- -45
        Tmax <- 60

        if (!((is.null(pw) & !is.null(T) & !is.null(hr)) |
                  (!is.null(pw) & is.null(T) & is.null(hr))))
            stop("Either pw, or T and hr, must be defined!")

        if (is.null(pw)) {
            x <- T
            x[is.na(x)] <- 0
            if (any(x < Tmin | x > Tmax) & warn)
                warning(paste("Some temperatures are below",
                              Tmin, "and / or above", Tmax, "deg C!"))
            return(Td1(pw(T)*hr/100))
        } else {
            x <- pw
            x[is.na(x)] <- 1
            if (any(x < pmin | x > pmax) & warn)
                warning(paste("Some vapour pressures are below",
                              pmin,"and / or above", pmax,"hPa!"))
            return(Td1(pw))
        }
    }
