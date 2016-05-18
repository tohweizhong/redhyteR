
# initialHy.R

# @ df: df
# @ Atgt: target attribute
# @ Acmp: comparing attribute
# @ Atgt_cl: Atgt classes in hypothesis
# @ Acmp_cl: Acmp classes in hypothesis
# @ Atgt_dist: e.g. c(1,1,2); length(Atgt_dist) == length(Atgt_cl)
# @ Acmp_dist: e.g. c(1,1,2); length(Acmp_dist) == length(Acmp_cl)
# @ Actx_items: c("A1 = c1", "A2 = c2")

# 1. HyType()
# 2. find mean of Atgt if Atgt is numeric
# 3. Subset()
# 4. Discretize()
# 5. AddClass()

initialHy <- function(df, Atgt, Acmp,
                      Atgt_cl = NULL, Acmp_cl = NULL,
                      Atgt_dist = NULL, Acmp_dist = NULL,
                      Actx_items = ""){

    # length of Atgt_cl should be same as Atgt_dist
    if(length(Atgt_cl) != length(Atgt_dist)) stop("length(Atgt_cl) not equal length(Atgt_dist)")
    if(length(Acmp_cl) != length(Acmp_dist)) stop("length(Acmp_cl) not equal length(Acmp_dist)")

    require(magrittr)

    # 1. HyType()
    Atgt_type   <- ifelse(is.factor(df[,Atgt]),"cate", "num")
    Atgt_num_cl <- ifelse(Atgt_cl %>% is.null, 0, length(Atgt_cl))
    Acmp_num_cl <- ifelse(Acmp_cl %>% is.null, 0, length(Acmp_cl))

    ht <- HyType(Atgt_type   = Atgt_type,
                 Atgt_num_cl = Atgt_num_cl,
                 Acmp_num_cl = Acmp_num_cl,
                 Atgt_dist   = Atgt_dist,
                 Acmp_dist   = Acmp_dist)

    # 2. find mean of Atgt if Atgt is numeric
    if(ht[["Atgt_type"]] == "num") mu <- mean(df[,Atgt])
    else mu <- NULL

    # 3. Subset()
    df <- Subset(df = df, Atgt = Atgt, Acmp = Acmp,
                 Atgt_cl = Atgt_cl, Acmp_cl = Acmp_cl,
                 Actx_items = Actx_items, ht = ht)

    # 4. Discretize()
    df <- Discretize(df = df, Atgt = Atgt, Acmp = Acmp, by = "mean")

    # 5. AddClass()
    df <- AddClass(df = df, Atgt = Atgt, Acmp = Acmp,
                   Atgt_cl = Atgt_cl, Acmp_cl = Acmp_cl,
                   Atgt_dist = Atgt_dist, Acmp_dist = Acmp_dist, ht = ht, mu = mu)

    # return
    return(list(Atgt = Atgt, Acmp = Acmp,
                Atgt_cl = Atgt_cl,
                Acmp_cl = Acmp_cl,
                df = df,
                hyType = ht))

}
