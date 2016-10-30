
# MakeTgtDf.R (Internal)

MakeTgtDf <- function(Atgt_cl, Atgt_dist){

    # check lengths
    if(Atgt_cl %>% length != Atgt_dist %>% length)
        stop("Differing lengths")

    tgt_df <- data.frame(rep("", Atgt_cl %>% length))
    tgt_df$Atgt_cl <- Atgt_cl
    tgt_df$Atgt_dist <- Atgt_dist

    tgt_df %>% subset(., select = c(Atgt_cl, Atgt_dist)) %>% return

}
