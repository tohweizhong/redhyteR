
# MakeCmpDf.R (Internal)

MakeCmpDf <- function(Acmp_cl, Acmp_dist){

    # check lengths
    if(Acmp_cl %>% length != Acmp_dist %>% length)
        stop("Differing lengths")

    cmp_df <- data.frame(rep("", Acmp_cl %>% length))
    cmp_df$Acmp_cl <- Acmp_cl
    cmp_df$Acmp_dist <- Acmp_dist

    cmp_df %>% subset(., select = c(Acmp_cl, Acmp_dist)) %>% return

}
