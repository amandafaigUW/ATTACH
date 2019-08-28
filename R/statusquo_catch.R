statusquo_catch <- function(ABC.DATA,scenario) {
    
    if (scenario == 1 | scenario == 2 | scenario == 3) {
        FISH.DATA <- ABC.DATA
        FISH.DATA$ABCboth <- FISH.DATA$ABC.BSAI.202 + FISH.DATA$ABC.BS.201
        FISH.DATA$ABCboth2 <- pmin(FISH.DATA$ABCboth,1.5e6)
        FISH.DATA$ABCboth.UB.150 <- as.numeric(FISH.DATA$ABC.BS.201 + FISH.DATA$ABC.BSAI.202 >= 1.5e6)
        FISH.DATA$pollock.bs.UB <-  as.numeric(FISH.DATA$ABC.BS.201 > 1.2e6)
    }   else if (scenario == 1.1) {
        FISH.DATA <- log(ABC.DATA)
        FISH.DATA$ABCboth <- exp(FISH.DATA$ABC.BSAI.202) + exp(FISH.DATA$ABC.BS.201)
        FISH.DATA$ABCboth2 <- pmin(FISH.DATA$ABCboth,1.5e6)
        FISH.DATA$ABCboth.UB.150 <- as.numeric(exp(FISH.DATA$ABC.BS.201) + exp(FISH.DATA$ABC.BSAI.202) >= 1.5e6)
        FISH.DATA$pollock.bs.UB <-  as.numeric(exp(ABC.DATA$ABC.BS.201) > 1.2e6)
    } 
    
    # These are all the indicators required in the regressions (I honestly think some of them are no longer needed, so this could probably
    # do to be cleaned up a bit, but its not hurting anyone.)
    # 
    # 1 means active in scenarios (2018+), 0 means inactive.  So for example flatfish flex is active from 2018 onwards; the strict SSL closure (WAISSL) from 2011-2014 is not.
    
    FISH.DATA$flex <-  1  # introduction of flatfish flex
    FISH.DATA$A80 <-  1  # introduction of A80
    FISH.DATA$po10 <- 1
    FISH.DATA$pre97 <- 0
    FISH.DATA$is93 <- 0
    FISH.DATA$WAISSLadj <- 1
    FISH.DATA$solegone <-  1
    FISH.DATA$plaicegone <-  1
    FISH.DATA$kamsplit <-  1
    FISH.DATA$AFA <-  1
    FISH.DATA$pollockAIchange <-  1 
    FISH.DATA$A28 <- 1
    FISH.DATA$atkadisp <- 0
    FISH.DATA$SSL <-  1 # stellar sea lion closure
    FISH.DATA$WAISSL <- 0
    FISH.DATA$A80.ask.POP <- 1
    FISH.DATA$A82 <- 1

    
    if (scenario == 1) {
        TAC.BOTHBIND <- predict.tac.function(predictmethod = 1, model="SUR",fit=tac_BOTHBIND_loglin_sur,FISH.DATA)
        
        CATCH.BOTHBIND.SURSUR <- predict.catch.function(model="SUR",fit=catch_BOTHBIND_loglin_sur,TAC.BOTHBIND )
        CATCH.BOTHBIND.SUROLS <- predict.catch.function(model="OLS",fit = catch_BOTHBIND_loglin_ols,TAC.BOTHBIND )
        
        TAC.BOTHBINDICATOR.SUR <- predict.tac.function(predictmethod = 1, model="SUR",fit=tac_BOTHBINDICATOR_loglin_sur,FISH.DATA)
        
        CATCH.BOTHBINDICATOR.SUROLS <- predict.catch.function(model="OLS",fit=catch_BOTHBINDICATOR_loglin_ols,TAC.BOTHBINDICATOR.SUR)
        
        TAC.BOTHBIND.NOFIRSTYEAR.SUR <- predict.tac.function(predictmethod = 1, model="SUR",fit=tac_BOTHBIND_NOFIRSTYEAR_loglin_sur,FISH.DATA)
        
        CATCH.BOTHBIND.NOFIRSTYEAR.SUROLS <- predict.catch.function(model="OLS",fit=catch_BOTHBIND_NOFIRSTYEAR_loglin_ols,TAC.BOTHBIND.NOFIRSTYEAR.SUR)
        
    } else if (scenario == 2) {
        #whitefish gets more TAC
        TAC.BOTHBIND <- predict.tac.function(predictmethod = 1, model="SUR_WFDOM",fit=tac_BOTHBIND_loglin_sur,FISH.DATA)
        TAC.BOTHBIND$ABCboth.UB.150 <- as.numeric(TAC.BOTHBIND$ABC.BS.201 + TAC.BOTHBIND$ABC.BSAI.202 >= 1.50e6*1.1)
        
        CATCH.BOTHBIND.SURSUR <- predict.catch.function(model="SUR",fit=catch_BOTHBIND_loglin_sur,TAC.BOTHBIND )
        CATCH.BOTHBIND.SUROLS <- predict.catch.function(model="OLS",fit = catch_BOTHBIND_loglin_ols,TAC.BOTHBIND )
        
        TAC.BOTHBINDICATOR.SUR <- predict.tac.function(predictmethod = 1, model="SUR_WFDOM",fit=tac_BOTHBINDICATOR_loglin_sur,FISH.DATA)
        TAC.BOTHBINDICATOR.SUR$ABCboth.UB.150 <- as.numeric(TAC.BOTHBINDICATOR.SUR$ABC.BS.201 + TAC.BOTHBINDICATOR.SUR$ABC.BSAI.202 >= 1.50e6*1.1)
        
        CATCH.BOTHBINDICATOR.SUROLS <- predict.catch.function(model="OLS",fit=catch_BOTHBINDICATOR_loglin_ols,TAC.BOTHBINDICATOR.SUR)
        
        TAC.BOTHBIND.NOFIRSTYEAR.SUR <- predict.tac.function(predictmethod = 1, model="SUR_WFDOM",fit=tac_BOTHBIND_NOFIRSTYEAR_loglin_sur,FISH.DATA)
        TAC.BOTHBIND.NOFIRSTYEAR.SUR$ABCboth.UB.150 <- as.numeric(TAC.BOTHBIND.NOFIRSTYEAR.SUR$ABC.BS.201 + TAC.BOTHBIND.NOFIRSTYEAR.SUR$ABC.BSAI.202 >= 1.50e6*1.1)
        
        CATCH.BOTHBIND.NOFIRSTYEAR.SUROLS <- predict.catch.function(model="OLS",fit=catch_BOTHBIND_NOFIRSTYEAR_loglin_ols,TAC.BOTHBIND.NOFIRSTYEAR.SUR)
      
    } else if (scenario == 3) {
        # flatfish gets more TAC
        TAC.BOTHBIND <- predict.tac.function(predictmethod = 1, model="SUR_FFDOM",fit=tac_BOTHBIND_loglin_sur,FISH.DATA)
        TAC.BOTHBIND$ABCboth.UB.150 <- as.numeric(TAC.BOTHBIND$ABC.BS.201 + TAC.BOTHBIND$ABC.BSAI.202 >= 1.50e6*0.9)
        
        CATCH.BOTHBIND.SURSUR <- predict.catch.function(model="SUR",fit=catch_BOTHBIND_loglin_sur,TAC.BOTHBIND )
        CATCH.BOTHBIND.SUROLS <- predict.catch.function(model="OLS",fit = catch_BOTHBIND_loglin_ols,TAC.BOTHBIND )
        
        TAC.BOTHBINDICATOR.SUR <- predict.tac.function(predictmethod = 1, model="SUR_FFDOM",fit=tac_BOTHBINDICATOR_loglin_sur,FISH.DATA)
        TAC.BOTHBINDICATOR.SUR$ABCboth.UB.150 <- as.numeric(TAC.BOTHBINDICATOR.SUR$ABC.BS.201 + TAC.BOTHBINDICATOR.SUR$ABC.BSAI.202 >= 1.50e6*0.9)
        
        CATCH.BOTHBINDICATOR.SUROLS <- predict.catch.function(model="OLS",fit=catch_BOTHBINDICATOR_loglin_ols,TAC.BOTHBINDICATOR.SUR)
        
        TAC.BOTHBIND.NOFIRSTYEAR.SUR <- predict.tac.function(predictmethod = 1, model="SUR_FFDOM",fit=tac_BOTHBIND_NOFIRSTYEAR_loglin_sur,FISH.DATA)
        TAC.BOTHBIND.NOFIRSTYEAR.SUR$ABCboth.UB.150 <- as.numeric(TAC.BOTHBIND.NOFIRSTYEAR.SUR$ABC.BS.201 + TAC.BOTHBIND.NOFIRSTYEAR.SUR$ABC.BSAI.202 >= 1.50e6*0.9)
        
        CATCH.BOTHBIND.NOFIRSTYEAR.SUROLS <- predict.catch.function(model="OLS",fit=catch_BOTHBIND_NOFIRSTYEAR_loglin_ols,TAC.BOTHBIND.NOFIRSTYEAR.SUR)
    }
    
    
    
    # create ensemble
    # 
    CATCH.PRED <- (CATCH.BOTHBIND.SURSUR + CATCH.BOTHBIND.SUROLS + CATCH.BOTHBINDICATOR.SUROLS + CATCH.BOTHBIND.NOFIRSTYEAR.SUROLS)/4
    
    output <- CATCH.PRED[c("CATCH.BS.141",
                           "CATCH.BS.204",
                           "CATCH.BS.103",
                           "CATCH.BS.102",
                           "CATCH.BS.147",
                           "CATCH.BS.303",
                           "CATCH.BS.60",
                           "CATCH.BS.100",
                           "CATCH.BS.310",
                           "CATCH.BS.202",
                           "CATCH.BS.106",
                           "CATCH.BS.301",
                           "CATCH.BS.201",
                           "CATCH.BS.104",
                           "CATCH.BS.307",
                           "CATCH.BS.203",
                           "CATCH.BS.400",
                           "CATCH.BS.65",
                           "CATCH.BS.326",
                           "CATCH.BS.90",
                           "CATCH.BS.50",
                           "CATCH.BS.140")]
    return (output)
}