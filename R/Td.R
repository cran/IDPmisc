Td <-
    function(pw = NULL, T = NULL, hr = NULL, warn = TRUE) {
        ## Author: Rene Locher
        ## Version 2019-01-30
        pmin <- 0.005400077
        pmax <- 199.9329
        Tmin <- -65
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
            return(ifelse(T >= 0, Td.aw(pw = pw(T)*hr/100, warn = FALSE), Tf.ai(pw = pw(T)*hr/100, warn = FALSE)))
        } else {
            x <- pw
            x[is.na(x)] <- 1
            if (any(x < pmin | x > pmax) & warn)
                warning(paste("Some vapour pressures are below",
                              pmin,"and / or above", pmax,"hPa!"))
            return(ifelse(pw > 6.112, Td.aw(pw = pw, warn = FALSE), Tf.ai(pw = pw, warn = FALSE)))
        }
    }
