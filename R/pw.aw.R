pw.aw <-
    function(Td, warn = TRUE) {
        ## Author: Rene Locher
        ## Version 2019-01-30
        x <- Td
        x[is.na(x)] <- 0
        if (any(x < -45 | x > 60) & warn)
            warning("Some temperatures are less than -45 deg C or more than 60 deg C!")
        return(6.112*exp(17.62*Td/(243.12 + Td)))
    }
