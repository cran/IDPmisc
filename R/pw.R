pw <-
    function(Td, warn = TRUE){
        ## Author: Rene Locher
        ## Version 2019-01-30

        x <- Td
        x[is.na(x)] <- 0
        if (any(x < -65 | x > 60) & warn)
            warning("Some temperatures are less than -65 deg C or more than 60 deg C!")

        return(ifelse(Td > 0, pw.aw(Td, warn=FALSE), pw.ai(Td, warn=FALSE)))
    }
