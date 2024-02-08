pw.ai <-
    function(Td, warn = TRUE) {
        ## Author: Rene Locher
        ## Version 2019-01-30
        x <- Td
        x[is.na(x)] <- 0
        if (any(x < -65 | x > 0) & warn)
            warning("Some temperatures are less than -65 deg C or more than 0 deg C!")
        return(6.112*exp(22.46*Td/(272.62 + Td)))
    }
