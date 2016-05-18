
# initialTest.R

initialTest <- function(ih){

    Atgt <- ih[["Atgt"]]
    Acmp <- ih[["Acmp"]]
    df <- ih[["df"]]
    ht <- ih[["hyType"]]
    Atgt_type <- ht[["Atgt_type"]]

    #str(df)

    if(Atgt_type == "cate"){
        tab <- table(df$cmp_grp, df$tgt_grp)
        print(tab)
        test <- chisq.test(tab)
        spineplot(df$tgt_grp ~ df$cmp_grp)
    }
    else if(ht[["Acmp_dist"]] %>% unique %>% length == 2){
        test <- t.test(df[,Atgt] ~ df$cmp_grp)
        boxplot(df[,Atgt] ~ df$cmp_grp)
    }
    else if(ht[["Acmp_dist"]] %>% unique %>% length > 2){
        test <- aov(df[,Atgt] ~ df$cmp_grp)
        boxplot(df[,Atgt] ~ df$cmp_grp)
    }
    return(test)
}
