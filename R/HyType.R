
# HyType.R (internal)

# @ Atgt_type: "num" or "cate"
# @ Atgt_num_cl: 0 if Atgt_type = numeric
# @ Acmp_num_cl
# @ Atgt_dist: e.g. c(1,1,2); length(Atgt_dist) == length(Atgt_cl)
# @ Acmp_dist: e.g. c(1,1,2); length(Acmp_dist) == length(Acmp_cl)

HyType <- function(Atgt_type,
                   Atgt_num_cl, Acmp_num_cl,
                   Atgt_dist, Acmp_dist){

    # Atgt is numeric, Atgt_num_cl = 0, Atgt_dist = 0

    return(list(Atgt_type   = Atgt_type,
                Atgt_num_cl = Atgt_num_cl,
                Acmp_num_cl = Acmp_num_cl,
                Atgt_dist   = Atgt_dist,
                Acmp_dist   = Acmp_dist))

    # if Atgt is categorical, always chi-squared
    # if Atgt is numeric, either t-test (2 grps)
    # or ANOVA (>2 grps)
}
