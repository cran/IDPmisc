hr <-
    function(T, Td, warn = TRUE) {
        ## Author: Rene Locher
        ## Version 2019-01-30
        if (any(Td > T) & warn) warning("Some dew points are higher than corresponding air temperatures!")
        100*pw(Td)/pw(T)
    }
