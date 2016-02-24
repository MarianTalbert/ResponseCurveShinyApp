auc.roc.plot<-function (DATA,Thresh, threshold = 101, find.auc = TRUE, which.model = (1:(ncol(DATA) -
    2)), na.rm = FALSE, xlab = "1-Specificity (false positives)",
    ylab = "Sensitivity (true positives)", main = "ROC Plot",
    model.names = NULL, color = NULL, line.type = NULL, lwd = 1,
    mark = 0, mark.numbers = TRUE, mark.color = NULL, opt.thresholds = NULL,
    opt.methods = NULL, req.sens, req.spec, obs.prev = NULL,
    smoothing = 1, add.legend = TRUE, legend.text = model.names,
    legend.cex = 0.8, add.opt.legend = TRUE, opt.legend.text = NULL,
    opt.legend.cex = 0.7, counter.diagonal = FALSE, pch = NULL,
    FPC, FNC, cost.line = FALSE,cexMult=1.5){
#This function was from the PresenceAbsence library on CRAN but with a few minor
#modifications to improve the beauty
    if (is.data.frame(DATA) == FALSE) {
        if (is.matrix(DATA) == TRUE) {
            DATA <- as.data.frame(DATA)
        }
        else {
            stop("'DATA' must be either data frame or matrix")
        }
    }
    OBS <- DATA[, 2]
    if (length(OBS[OBS == 0]) == 0) {
        stop("no observed absences in dataset, therefore specificity does not",
            "exist, and modeling, much less Area Under the Curve, is not very",
            "meaningful")
    }
    if (length(OBS[OBS == 1]) == 0) {
        stop("no observed presences in dataset, therefore sensitivity does not",
            "exist, and modeling, much less Area Under the Curve, is not very",
            "meaningful")
    }
    if (is.logical(find.auc) == FALSE) {
        stop("'find.auc' must be of logical type")
    }
    if (is.logical(na.rm) == FALSE) {
        stop("'na.rm' must be of logical type")
    }
    if (is.logical(mark.numbers) == FALSE) {
        stop("'mark.numbers' must be of logical type!")
    }
    if (is.logical(add.legend) == FALSE) {
        stop("'add.legend' must be of logical type!")
    }
    if (is.logical(add.opt.legend) == FALSE) {
        stop("'add.opt.legend' must be of logical type")
    }
    if (is.logical(counter.diagonal) == FALSE) {
        stop("'counter.diagonal' must be of logical type")
    }
    if (length(smoothing) != 1) {
        stop("'smoothing' must be a single number greater than or equal to 1")
    }
    else {
        if (is.numeric(smoothing) == FALSE) {
            stop("'smoothing' must be a single number greater than or equal to 1")
        }
        else {
            if (smoothing < 1) {
                stop("'smoothing' must be a single number greater than or equal to 1")
            }
        }
    }
    if (sum(is.na(DATA)) > 0) {
        if (na.rm == TRUE) {
            NA.rows <- apply(is.na(DATA), 1, sum)
            warning(length(NA.rows[NA.rows > 0]), " rows ignored due to NA values")
            DATA <- DATA[NA.rows == 0, ]
        }
        else {
            return(NA)
        }
    }
    DATA[DATA[, 2] > 0, 2] <- 1
    N.models <- ncol(DATA) - 2
    if (is.null(obs.prev) == TRUE) {
        obs.prev <- sum(DATA[, 2])/nrow(DATA)
    }
    if (obs.prev < 0 || obs.prev > 1) {
        stop("'obs.prev' must be a number between zero and one")
    }
    if (obs.prev == 0) {
        warning("because your observed prevalence was zero, results may be strange")
    }
    if (obs.prev == 1) {
        warning("because your observed prevalence was one, results may be strange")
    }
    if (min(which.model) < 1 || sum(round(which.model) != which.model) !=
        0) {
        stop("values in 'which.model' must be positive integers")
    }
    if (max(which.model) > N.models) {
        stop("values in 'which.model' must not be greater than number of models in 'DATA'!")
    }
    if (is.null(model.names) == TRUE) {
        model.names <- if (is.null(names(DATA)) == FALSE) {
            names(DATA)[-c(1, 2)]
        }
        else {
            paste("Model", 1:N.models)
        }
    }
    if (N.models != length(model.names) && (length(which.model) !=
        1 || length(model.names) != 1)) {
        stop("If 'model.names' is specified it must either be a single name, or a vector",
            "of the same length as the number of model predictions in 'DATA'")
    }
    if (is.null(legend.text) == TRUE) {
        legend.text <- model.names
    }
    if (length(legend.text) != N.models) {
        stop("'opt.legend.text' must be of same length as 'opt.methods'")
    }
    DATA <- DATA[, c(1, 2, which.model + 2)]
    if (length(model.names) != 1) {
        model.names <- model.names[which.model]
    }
    
    N.dat <- ncol(DATA) - 2
    if (is.null(obs.prev) == TRUE) {
        obs.prev <- sum(DATA[, 2])/nrow(DATA)
    }
    if (obs.prev < 0 || obs.prev > 1) {
        stop("'obs.prev' must be a number between zero and one")
    }
    mark <- matrix(mark, length(mark), N.dat)
    if (!is.null(opt.methods) && is.null(opt.thresholds)) {
        opt.thresholds <- TRUE
    }
    if (is.null(opt.methods) && is.null(opt.thresholds)) {
        opt.thresholds <- FALSE
    }
    if (is.null(opt.methods)) {
        opt.methods <- c(1, 2, 4)
    }
    if (is.logical(opt.thresholds) == TRUE) {
        if (opt.thresholds == TRUE) {
            POSSIBLE.meth <- c("Default", "Sens=Spec", "MaxSens+Spec",
                "MaxKappa", "MaxPCC", "PredPrev=Obs", "ObsPrev",
                "MeanProb", "MinROCdist", "ReqSens", "ReqSpec",
                "Cost")
            N.meth <- length(opt.methods)
            if (is.numeric(opt.methods) == TRUE) {
                if (sum(opt.methods %in% (1:length(POSSIBLE.meth))) !=
                  N.meth) {
                  stop("invalid optimization method")
                }
                else {
                  opt.methods <- POSSIBLE.meth[opt.methods]
                }
            }
            if (sum(opt.methods %in% POSSIBLE.meth) != N.meth) {
                stop("invalid optimization method")
            }
            if (is.null(opt.legend.text) == TRUE) {
                opt.legend.text <- opt.methods
            }
            if (length(opt.legend.text) != N.meth) {
                stop("'opt.legend.text' must be of same length as 'opt.methods'")
            }
            if ("ReqSens" %in% opt.methods) {
                if (missing(req.sens)) {
                  warning("req.sens defaults to 0.85")
                  req.sens <- 0.85
                }
            }
            if ("ReqSpec" %in% opt.methods) {
                if (missing(req.spec)) {
                  warning("req.spec defaults to 0.85")
                  req.spec <- 0.85
                }
            }
            if ("Cost" %in% opt.methods) {
                if (missing(FPC) || missing(FNC)) {
                  warning("costs assumed to be equal")
                  FPC <- 1
                  FNC <- 1
                }
                if (FPC <= 0 || FNC <= 0) {
                  stop("costs must be positive")
                }
                if (is.logical(cost.line) == FALSE) {
                  stop("'cost.line' must be of logical type")
                }
                if (!"Cost" %in% opt.methods) {
                  cost.line <- FALSE
                }
            }
            mark <- optimal.thresholds(DATA = DATA, threshold = threshold,
                model.names = model.names, na.rm = na.rm, opt.methods = opt.methods,
                req.sens = req.sens, req.spec = req.spec, obs.prev = obs.prev,
                smoothing = smoothing, FPC = FPC, FNC = FNC)[,
                -1, drop = FALSE]
        }
    }
    if (is.logical(opt.thresholds) == FALSE) {
        if (!is.numeric(opt.thresholds)) {
            stop("'opt.thresholds' must be 'TRUE', 'FALSE', or numeric")
        }
        if (min(opt.thresholds) < 0) {
            stop("'opt.thresholds' can not be negative")
        }
        if (max(opt.thresholds) > 1) {
            if (N.thr == 1 && round(opt.thresholds) == opt.thresholds) {
                opt.thresholds <- seq(length = opt.thresholds,
                  from = 0, to = 1)
                N.thr <- length(opt.thresholds)
            }
            else {
                stop("non-interger, non-logical 'opt.thresholds' greater than 1")
            }
        }
        N.opt.thresh <- length(opt.thresholds)
        if (is.null(opt.legend.text)) {
            opt.legend.text <- rep("threshold", N.opt.thresh)
        }
        if (length(opt.legend.text) != N.opt.thresh) {
            stop("length of 'opt.legend.text' does not match number of specified thresholds")
        }

        mark <- matrix(opt.thresholds, length(opt.thresholds),
            N.dat)
        opt.thresholds = TRUE
    }

        pch <- 16
 
      op <- par(pty = "s")

df <- data.frame()
AUC<-vector()
ThreshPoints<-as.data.frame(matrix(nrow=N.dat,ncol=4))
 
   
    
    for (dat in 1:N.dat) {
        Model.dat <- roc.plot.calculate(DATA = DATA, threshold = threshold,
            which.model = dat)
        Model.dat$specificity<-1-Model.dat$specificity
        AUC[dat]<-round(auc(DATA,,which.model=dat)$AUC,2)
        Lab<-paste(model.names[dat]," (AUC = ",AUC[dat],")",sep="")
        ThreshInd<-which.min((Model.dat$threshold-Thresh[[dat]])^2)
        ThreshPoints[dat,]<-c(Lab,Thresh[[dat]],Model.dat$sensitivity[ThreshInd],Model.dat$specificity[ThreshInd])
        if(dat==1){PlotDat<-cbind(Model.dat,Model=rep(Lab,times=nrow(Model.dat)))
        }else{
          PlotDat<-rbind(PlotDat,cbind(Model.dat,Model=rep(Lab,times=nrow(Model.dat))))
        }
    }
   
    colnames(ThreshPoints)<-c("Model","Thresh","sensitivity","specificity")
    ThreshPoints$Model<-as.factor(ThreshPoints$Model)
    ThreshPoints$sensitivity<-as.numeric(ThreshPoints$sensitivity)
    ThreshPoints$specificity<-as.numeric(ThreshPoints$specificity)
    
    p<-ggplot(PlotDat,aes(x=specificity,y=sensitivity,colour=Model)) + 
      geom_line(size=1) + xlim(0, 1) + ylim(0, 1)+xlab(xlab)+ylab(ylab)+
           geom_point(data=ThreshPoints,aes(x=specificity,y=sensitivity,colour=Model),size=rel(3))+
      theme(axis.title = element_text(size = rel(1.3)))+
      ggtitle("ROC Plot")
return(p)
}
