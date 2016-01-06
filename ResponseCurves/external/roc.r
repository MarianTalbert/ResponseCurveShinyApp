roc <-
  function (obsdat, preddat)
    {
      # code adapted from Ferrier, Pearce and Watson's code, by J.Elith
      
            if (length(obsdat) != length(preddat))
                  stop("obs and preds must be equal lengths")
          n.x <- length(obsdat[obsdat == 0])
          n.y <- length(obsdat[obsdat == 1])
          xy <- c(preddat[obsdat == 0], preddat[obsdat == 1])
          rnk <- rank(xy)
          wilc <- ((n.x * n.y) + ((n.x * (n.x + 1))/2) - sum(rnk[1:n.x]))/(n.x *  n.y)
          return(round(wilc, 4))
      }