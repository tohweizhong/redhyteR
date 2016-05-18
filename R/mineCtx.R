
# mineCtx.R

mineCtx <- function(ih, cm_ctrl){

    # retrieve the relevant variables
    df        <- ih$df
    Atgt      <- ih$Atgt
    Acmp      <- ih$Acmp
    Atgt_type <- ih$hyType$Atgt_type
    num_mined <- cm_ctrl$num_mined
    metric    <- cm_ctrl$metric

    # some settings for context mining which I'm not sure whether to surface
    p <- 0.7

    # remove Atgt and Acmp from df, only need tgt_grp and cmp_grp
    df <- df[,-which(colnames(df) == Atgt)]
    df <- df[,-which(colnames(df) == Acmp)]

    # Load required packages
    if(!require(caret))        stop("caret pkg not installed.")
    if(!require(randomForest)) stop("randomForest pkg not installed.")
    if(!require(pROC))         stop("pROC pkg not installed.")

    # Check number of context attributes to mine for
    if(num_mined >= (ncol(df) - 4)) stop("num_mined >= (ncol(df) - 4)")

    # Preparation for cross-validation
    # Requires CV sets for both target and comparing models
    df_tgt <- df[, -which(colnames(df) == "cmp_grp")]
    df_cmp <- df[, -which(colnames(df) == "tgt_grp")]

    y_tgt <- subset(df_tgt, select = tgt_grp)[,1]
    y_cmp <- subset(df_cmp, select = cmp_grp)[,1]

    tgt_idx <- createDataPartition(y_tgt, p = p, list = FALSE)
    cmp_idx <- createDataPartition(y_cmp, p = p, list = FALSE)

    Xtrain_tgt <- df_tgt[ tgt_idx, -which(colnames(df_tgt) == "tgt_grp")]
    Xtest_tgt  <- df_tgt[-tgt_idx, -which(colnames(df_tgt) == "tgt_grp")]
    Xtrain_cmp <- df_cmp[ cmp_idx, -which(colnames(df_cmp) == "cmp_grp")]
    Xtest_cmp  <- df_cmp[-cmp_idx, -which(colnames(df_cmp) == "cmp_grp")]

    ytrain_tgt <- y_tgt[ tgt_idx]
    ytest_tgt  <- y_tgt[-tgt_idx]
    ytrain_cmp <- y_cmp[ cmp_idx]
    ytest_cmp  <- y_cmp[-cmp_idx]

    #print(colnames(Xtrain_tgt)); print(colnames(Xtrain_cmp))
    #print(table(ytrain_tgt)); print(table(ytrain_cmp))

    # Get the formulae
    predictors <- colnames(df)[-which(colnames(df) == "tgt_grp")]
    predictors <- predictors[-which(predictors == "cmp_grp")]
    frm_tgt <- paste(" ", predictors, sep = "", collapse = "+")
    frm_tgt <- as.formula(paste("tgt_grp", "~", frm_tgt))
    frm_cmp <- paste(" ", predictors, sep = "", collapse = "+")
    frm_cmp <- as.formula(paste("cmp_grp", "~", frm_cmp, sep = ""))

    # Construct target and comparing models
    print("Constructing target model...")
    mod_tgt <- randomForest(x = Xtrain_tgt, y = ytrain_tgt, importance = TRUE)
    print("Constructing comparing model...")
    mod_cmp <- randomForest(x = Xtrain_cmp, y = ytrain_cmp, importance = TRUE)

    # Make predictions on testing sets
    mod_tgt_pred_prob  <- predict(mod_tgt, newdata = Xtest_tgt, type = "prob")[,1]
    mod_tgt_pred_class <- predict(mod_tgt, newdata = Xtest_tgt, type = "response")
    mod_cmp_pred_prob  <- predict(mod_cmp, newdata = Xtest_cmp, type = "prob")[,1]
    mod_cmp_pred_class <- predict(mod_cmp, newdata = Xtest_cmp, type = "response")

    # Evaluate models
    if(metric == "auc"){
        tgt_auc <- auc(predictor = mod_tgt_pred_prob, response = ytest_tgt)
        cmp_auc <- auc(predictor = mod_cmp_pred_prob, response = ytest_cmp)
        print(tgt_auc); print(cmp_auc)
    }

    return(list(mod_tgt = mod_tgt, mod_cmp = mod_cmp))
}

