
# MakeCtxDf.R (Internal)

MakeCtxDf <- function(Actx_items){

    if(Actx_items[1] %>% nchar > 0){

        A  <- NULL
        cl <- NULL
        for(ii in Actx_items){

            tmp <- strsplit(ii, " = ") %>% unlist
            A   <- c(A, tmp[1])
            cl  <- c(cl, tmp[2])
        }
    }
    ctx_df <- cbind(A, cl) %>% data.frame %>% return
}
