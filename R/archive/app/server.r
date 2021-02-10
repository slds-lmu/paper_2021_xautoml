server <- function(input, output) {
  
  # get datasets and model for chosen dataset and lambda
  objects <- eventReactive(list(input$dataset, input$lambda, input$iteration),{
    data = get(paste0("data_",tolower(input$dataset)))
    #browser()
    lambda = as.numeric(input$lambda)
    iteration = as.numeric(input$iteration)
    
    if (input$method == "xgboost") {
      object = get_objects(data, lambda, iteration)
      return(list(model = object$model, mbo_train = object$data_train,
                  lhs_test = object$data_test, model_best = object$model_best, data_holdout = object$test_holdout
      ))
    }
      
    else if (input$method == "mlp") {
      browser()
        object = get_objects_mlp(data, lambda, iteration)
        return(list(model = object$model, mbo_train = object$data_train,
                    lhs_test = object$data_test, model_best = object$model_best, data_holdout = object$test_holdout, surrogate = object$surrogate
        ))
      }
    
    
    
    
  })
  
  objects.all <- eventReactive(list(input$dataset, input$lambda, input$iteration.all),{
    data = get(paste0("data_",tolower(input$dataset)))
    #browser()
    lambda = as.numeric(input$lambda)
    if (input$iteration.all == "Yes") {
      model = vector("list", length(input$dataset))
      mbo_train = vector("list", length(input$dataset))
      iter.min = 1
      iter.max = 30
      for (i in iter.min:iter.max) {
        object = get_objects(data, lambda, i)
        model[[i]] = object$model
        mbo_train[[i]] = object$data_train
      }
      return(list(model = model, mbo_train = mbo_train))
    }
    # else {
    #   object = get_objects(data, lambda, iteration)
    #   return(list(model = object$model, mbo_train = object$data_train,
    #               lhs_test = object$data_test, model_best = object$model_best, data_holdout = object$test_holdout
    #   ))
    # }
    
  })
  
  # counterfactuals data
  data.counterfactual <- eventReactive(list(input$obs.counter, input$improve.counter), {
    #browser()
    x.interest = objects()$mbo_train[as.numeric(input$obs.counter),]
    data = objects()$mbo_train[-as.numeric(input$obs.counter),]
    pred = Predictor$new(model = objects()$model, data = data, conditional = FALSE)
    ctr = partykit::ctree_control(maxdepth = 5L)
    
    set.seed(1234)
    pred$conditionals = fit_conditionals(pred$data$get.x(), ctrl = ctr)
    
    ###---- Compute counterfactuals ----
    prediction = pred$predict(x.interest)$.prediction
    
    set.seed(1000)
    cf = Counterfactuals$new(predictor = pred, x.interest = x.interest,
                                    target = c(min(data$y), prediction - as.numeric(input$improve.counter)*prediction),
                                    epsilon = 0, generations = list(mosmafs::mosmafsTermStagnationHV(10),
                                                                    mosmafs::mosmafsTermGenerations(200)),
                                    # mu = best.params$mu,
                                    # p.mut = best.params$p.mut, p.rec = best.params$p.rec,
                                    # p.mut.gen = best.params$p.mut.gen,
                                    # p.mut.use.orig = best.params$p.mut.use.orig,
                                    # p.rec.gen = best.params$p.rec.gen,
                                    initialization = "icecurve"#,
                                    # p.rec.use.orig = best.params$p.rec.use.orig
    )
    
    # Number of counterfactuals
    nrow(cf$results$counterfactuals)
    id = cf$results$counterfactuals$dist.target < 0.01
    print(sum(id))
    
    # Focus counterfactuals that met target
    cf$results$counterfactuals = cf$results$counterfactuals[which(id), ]
    cf$results$counterfactuals.diff = cf$results$counterfactuals.diff[which(id), ]
    return(list(cf, prediction))
  })
  
  # define predictor 
  predictor <- reactive({
    Predictor$new(model = objects()$model, data = objects()$mbo_train)
  })
  
  # define predictor 
  predictor.test <- reactive({
    set.seed(1234)
    sample = sample(1:nrow(objects()$lhs_test), 500)
    Predictor$new(model = objects()$model, data = objects()$lhs_test[sample,])
  })
  
  
  # Render UI Elements
  output$dataset <- renderUI({
    #browser()
    if (input$method == "xgboost") {
      return(selectInput("dataset", "Choose Dataset", folder))
    }
    else if (input$method == "mlp") {
      return(selectInput("dataset", "Choose Dataset", folder_mlp))
    }
  })
  
  
  # Feature Effects
  output$params <- renderUI({
    selectInput("paramList", "Choose Feature", colnames(objects()$lhs_test)[-which(colnames(objects()$lhs_test) == "y")])
  })
  
  output$center <- renderUI({
    selectInput("center", "Center ice curves?", c("No", "Yes"))
  })
  
  output$params1 <- renderUI({
    selectInput("paramList1", "Choose 1. Feature", colnames(objects()$lhs_test)[-which(colnames(objects()$lhs_test) == "y")])
  })
  
  output$params2 <- renderUI({
    selectInput("paramList2", "Choose 2. Feature", colnames(objects()$lhs_test)[-which(colnames(objects()$lhs_test) %in% c("y",input$paramList1))])
  })
  
  #Feature Importance
  output$holdout <- renderUI({
    #browser()
    if (nrow(objects()$data_holdout) > 50) {
      c = c("Yes", "No")
    }
    else {
      c = c("No")
    }
    
    selectInput("holdout", "Choose holdout set?", choices = as.list(c), selected = "No")
  })
  
  # Local Interpretation
  output$obs1 <- renderUI({
    selectInput("obs1", "Choose first observation (default: best)", choices = c(1:nrow(objects()$mbo_train)), selected = which.min(objects()$mbo_train$y))
  })
  
  output$obs2 <- renderUI({
    selectInput("obs2", "Choose second observation (default: worst)", choices = c(1:nrow(objects()$mbo_train)), selected = which.max(objects()$mbo_train$y))
  })
  
  
  
  # Plot / Summary Output
  
  # Tab: Descriptive
  
  # Correlation Plot Train
  output$corr_train <- renderPlot({
    corrplot(cor(objects()$mbo_train), method = "color", type = "upper", order = "FPC",
             addCoef.col = "black", tl.col = "black", diag = FALSE, tl.cex = 0.9, tl.srt = 45)
    #title(main = "MBO (train set)")
  })
  
  # Correlation Plot Test
  output$corr_test <- renderPlot({
    corrplot(cor(objects()$lhs_test), method = "color", type = "upper", order = "FPC",
             addCoef.col = "black", tl.col = "black", diag = FALSE, tl.cex = 0.9, tl.srt = 45)
    #title(main = "randomLHS (test set)")
  })
  
  # Summary Statistics Train and Test
  output$sum_train <- renderPrint({
    summary(objects()$mbo_train)
  })
  
  output$sum_test <- renderPrint({
    summary(objects()$lhs_test)
  })
  
  # Boxplot Train
  output$box_train <- renderPlotly({
    dat <- as.data.frame(scale(objects()$mbo_train))
    dat.m <- melt(dat,measure.vars = 1:ncol(dat))
    #library(ggplot2)
    p <- ggplot(dat.m) + geom_boxplot(aes(x = variable, y = value)) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
    ggplotly(p)
  })
  
  output$box_test <- renderPlotly({
    #browser()
    dat <- as.data.frame(scale(objects()$lhs_test))
    dat.m <- melt(dat,measure.vars = 1:ncol(dat))
    #library(ggplot2)
    p <- ggplot(dat.m) + geom_boxplot(aes(x = variable, y = value)) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
    ggplotly(p)
  })
  
  
  # Tab: Feature Effects
  
  # ALE Plot  
  output$ale <- renderPlotly({
    #browser()
    #require(input$paramList)
    if (is.null(input$paramList)) {
      return()
    }
    else{
      # effects.ale = FeatureEffect$new(predictor = predictor(), feature = input$paramList, method = "ale", grid.size = 100)
      # if (input$ale.abs == "Yes") {
      #   effects.ale$results$.value = effects.ale$results$.value + mean(objects()$mbo_train$y)
      # }
      effects.ale = df_ale[which(df_ale$feature == input$paramList & df_ale$lambda == input$lambda),]
      data.distr = data.train[which(data.train$extrapol == input$lambda),]
      ale = ggplot(effects.ale) + 
        geom_rect(aes(xmin = hyper.q25.train, xmax = hyper.q75.train, ymin = min(mean.train) - 2*max(sd.train), ymax = max(mean.train) + 2*max(sd.train)), fill = "cadetblue2", alpha = 0.4) +
        geom_segment(aes(x = hyper.mean.train, y = min(mean.train) - 2*max(sd.train),  xend = hyper.mean.train, yend = max(mean.train) + 2*max(sd.train), color = "best.config.mean.IQR"), lwd = 1) +
        geom_ribbon(aes(x = grid.train, y = mean.train, ymin = mean.train - 1.96*sd.train, ymax = mean.train + 1.96*sd.train), alpha = 0.3, fill = "grey") + 
        geom_line(aes(x = grid.train, y = mean.train, color = "ALE.CI95"), lwd = 1) + 
        geom_rug(data = data.distr, aes(x = data.distr[,input$paramList]), sides = "b") +
        scale_color_manual(name = "", values = c(ALE.CI95 = "grey", best.config.mean.IQR = "cadetblue3")) +
        theme_bw() +
        labs(x = input$paramList, y = "Performance") +
        ggtitle(paste("ALE of", input$paramList))
      
      ggplotly(ale) %>%
        layout(legend = list(orientation = "h", x = 0.1, y = 1))
    }
    
  })
  
  
  # PDP Plot
  output$pdp <- renderPlotly({
    if (is.null(input$paramList)) {
      return()
    }
    else {
      # pdp
      if (input$iteration.all == "Yes") {
        effects.pdp = df_pdp[which(df_pdp$feature == input$paramList & df_pdp$lambda == input$lambda),]
        data.distr = data.train[which(data.train$extrapol == input$lambda),]
        pdp = ggplot(effects.pdp) + 
          geom_rect(aes(xmin = hyper.q25.train, xmax = hyper.q75.train, ymin = min(mean.train) - 2*max(sd.train), ymax = max(mean.train) + 2*max(sd.train)), fill = "cadetblue2", alpha = 0.4) +
          geom_segment(aes(x = hyper.mean.train, y = min(mean.train) - 2*max(sd.train),  xend = hyper.mean.train, yend = max(mean.train) + 2*max(sd.train), color = "best.config.mean.IQR"), lwd = 1) +
          geom_ribbon(aes(x = grid.train, y = mean.train, ymin = mean.train - 1.96*sd.train, ymax = mean.train + 1.96*sd.train), alpha = 0.3, fill = "grey") + 
          geom_line(aes(x = grid.train, y = mean.train, color = "PDP.CI95"), lwd = 1) + 
          geom_rug(data = data.distr, aes(x = data.distr[,input$paramList]), sides = "b") +
          scale_color_manual(name = "", values = c(PDP.CI95 = "grey", best.config.mean.IQR = "cadetblue3")) +
          theme_bw() +
          labs(x = input$paramList, y = "Performance") +
          ggtitle(paste("PDP of", input$paramList))
      }
      else {
        effects.pdp = FeatureEffect$new(predictor = predictor(), feature = input$paramList, method = "pdp")
        hyperparam.best = objects()$mbo_train[which.min(objects()$mbo_train$y),input$paramList]
        pdp = effects.pdp$plot() + ggtitle("PD plot") + geom_vline(aes(xintercept = hyperparam.best, color = "config.best"), alpha = 0.5, linetype = "dashed", size = 1) +
          scale_color_manual(name = "", values = c(config.best = "red")) + theme(legend.position = c(0.8, 0.8), legend.text = element_text(size = 6))
        if (input$show.obs == "Yes") {
          pdp = effects.pdp$plot() + ggtitle("PD plot") + geom_vline(aes(xintercept = hyperparam.best, color = "hyperparam.best"), alpha = 0.5, linetype = "dashed", size = 1) +
            geom_point(data = objects()$mbo_train, aes(x = objects()$mbo_train[,input$paramList], y = y, color = "observations"), alpha = 0.5) +
            scale_color_manual(name = "", values = c(hyperparam.best = "red", observations = "aquamarine2")) + theme(legend.position = c(0.8, 0.8), legend.text = element_text(size = 6))
        }
      }
      
      ggplotly(pdp) %>%
        layout(legend = list(orientation = "h", x = 0.1, y = 1))
    }
  })
  
  
  # PDP + ICE Plot
  output$ice <- renderPlotly({
    if (is.null(input$paramList)) {
      return()
    }
    else{
     
      # ice
      if (input$center == "No") effects = FeatureEffect$new(predictor = predictor(), feature = input$paramList, method = "pdp+ice")
      else if (input$center == "Yes") effects = FeatureEffect$new(predictor = predictor(), feature = input$paramList, method = "pdp+ice", center.at = min(objects()$mbo_train[,input$paramList]))
      #browser()
      edata = na.omit(data.table(
        id = effects$results$.id,
        eta = as.numeric(unlist(objects()$mbo_train[input$paramList]))[effects$results$.id],
        eta.diff = abs(as.numeric(unlist(effects$results[input$paramList])) - as.numeric(unlist(objects()$mbo_train[input$paramList]))[effects$results$.id]),
        y = effects$results$.y.hat))
      edata = edata[, list("eta" = eta[which.min(eta.diff)],
                           "y" = y[which.min(eta.diff)]), by = "id"]
      ice = effects$plot() + 
        ggtitle("ICE curves + PD plot") + 
        geom_point(data = edata, aes(x = eta, y = y), alpha = 0.5, shape = 4) #, col = nrounds)) + scale_color_viridis()
      ggplotly(ice)
    }
  })
  
  
  # same Plots with Test set
  
  # ALE Plot  
  output$ale.test <- renderPlotly({
    #browser()
    #require(input$paramList)
    if (is.null(input$paramList)) {
      return()
    }
    else{
      # effects.ale = FeatureEffect$new(predictor = predictor.test(), feature = input$paramList, method = "ale", grid.size = 100)
      # if (input$ale.abs == "Yes") {
      #   effects.ale$results$.value = effects.ale$results$.value + mean(objects()$lhs_test$y)
      # }
      effects.ale = df_ale[which(df_ale$feature == input$paramList & df_ale$lambda == input$lambda),]
      data.distr = data.test[which(data.test$extrapol == input$lambda)[test.sample],]
      ale = ggplot(effects.ale) + 
        geom_rect(aes(xmin = hyper.q25.test, xmax = hyper.q75.test, ymin = min(mean.test) - 2*max(sd.test), ymax = max(mean.test) + 2*max(sd.test)), fill = "cadetblue2", alpha = 0.4) +
        geom_segment(aes(x = hyper.mean.test, y = min(mean.test) - 2*max(sd.test),  xend = hyper.mean.test, yend = max(mean.test) + 2*max(sd.test), color = "best.config.mean.IQR"), lwd = 1) +
        geom_ribbon(aes(x = grid.test, y = mean.test, ymin = mean.test - 1.96*sd.test, ymax = mean.test + 1.96*sd.test), alpha = 0.3, fill = "grey") + 
        geom_line(aes(x = grid.test, y = mean.test, color = "ALE.CI95"), lwd = 1) + 
        geom_rug(data = data.distr, aes(x = data.distr[,input$paramList]), sides = "b") +
        scale_color_manual(name = "", values = c(ALE.CI95 = "grey", best.config.mean.IQR = "cadetblue3")) +
        theme_bw() +
        labs(x = input$paramList, y = "Performance") +
        ggtitle(paste("ALE of", input$paramList))
      
      ggplotly(ale) %>%
        layout(legend = list(orientation = "h", x = 0.1, y = 1))
    }
    
  })
  
  
  # PDP Plot
  output$pdp.test <- renderPlotly({
    if (is.null(input$paramList)) {
      return()
    }
    else{
      effects.pdp = df_pdp[which(df_pdp$feature == input$paramList & df_pdp$lambda == input$lambda),]
      data.distr = data.test[which(data.test$extrapol == input$lambda)[test.sample],]
      #browser()
      pdp = ggplot(effects.pdp) + 
        geom_rect(aes(xmin = hyper.q25.test, xmax = hyper.q75.test, ymin = min(mean.test) - 2*max(sd.test), ymax = max(mean.test) + 2*max(sd.test)), fill = "cadetblue2", alpha = 0.4) +
        geom_segment(aes(x = hyper.mean.test, y = min(mean.test) - 2*max(sd.test),  xend = hyper.mean.test, yend = max(mean.test) + 2*max(sd.test), color = "best.config.mean.IQR"), lwd = 1) +
        geom_ribbon(aes(x = grid.test, y = mean.test, ymin = mean.test - 1.96*sd.test, ymax = mean.test + 1.96*sd.test), alpha = 0.3, fill = "grey") + 
        geom_line(aes(x = grid.test, y = mean.test, color = "PDP.CI95"), lwd = 1) + 
        geom_rug(data = data.distr, aes(x = data.distr[,input$paramList]), sides = "b") +
        scale_color_manual(name = "", values = c(PDP.CI95 = "grey", best.config.mean.IQR = "cadetblue3")) +
        theme_bw() +
        labs(x = input$paramList, y = "Performance") +
        ggtitle(paste("PDP of", input$paramList))
      
      ggplotly(pdp) %>%
        layout(legend = list(orientation = "h", x = 0.1, y = 1))
    }
  })
  
  
  # PDP + ICE Plot
  output$ice.test <- renderPlotly({
    if (is.null(input$paramList)) {
      return()
    }
    else{
      # ice
      if (input$center == "No") effects = FeatureEffect$new(predictor = predictor.test(), feature = input$paramList, method = "pdp+ice")
      else if (input$center == "Yes") effects = FeatureEffect$new(predictor = predictor.test(), feature = input$paramList, method = "pdp+ice", center.at = 0)
      #browser()
      edata = na.omit(data.table(
        id = effects$results$.id,
        eta = as.numeric(unlist(objects()$lhs_test[input$paramList]))[effects$results$.id],
        eta.diff = abs(as.numeric(unlist(effects$results[input$paramList])) - as.numeric(unlist(objects()$lhs_test[input$paramList]))[effects$results$.id]),
        y = effects$results$.y.hat))
      edata = edata[, list("eta" = eta[which.min(eta.diff)],
                           "y" = y[which.min(eta.diff)]), by = "id"]
      ice = effects$plot() + 
        ggtitle("ICE curves + PD plot") + 
        geom_point(data = edata, aes(x = eta, y = y), alpha = 0.5, shape = 4) #, col = nrounds)) + scale_color_viridis()
      ggplotly(ice)
    }
  })
  
  
  
  
  # 2d interactions: HStatistics
  output$hstat <- renderPlotly({
    p <- plot(Interaction$new(predictor())) + ggtitle("H-statistics")
    ggplotly(p)
  })
  
  output$hstat2feat <- renderPlotly({
    if (is.null(input$paramList1)) {
      return()
    }
    else{
      p <- plot(Interaction$new(predictor(), feature = input$paramList1)) + ggtitle(paste0("2-way H-statistics:", input$paramList1))
      ggplotly(p)
    }
  })
  
  # 2 dim ale
  output$ale2d <- renderPlotly({
    if (input$showPdp == "No") {
      return()
    }
    else{
      
      #browser()
      ale2d = FeatureEffect$new(predictor = predictor(), feature = c(input$paramList1, input$paramList2), 
                                method = "ale", grid.size = 50)
      ale = ale2d$plot() + scale_fill_viridis() + ggtitle(paste0(input$paramList1, " vs. ", input$paramList2, ": 2d-ALE"))
      ggplotly(ale)
    }
  })
  
  # 2 dim pdp
  output$pdp2d <- renderPlotly({
    if (input$showPdp == "No") {
      return()
    }
    else{
      pdp2d = FeatureEffect$new(predictor = predictor(), feature = c(input$paramList1, input$paramList2), 
                                method = "pdp", grid.size = 50)
      pdp = pdp2d$plot() + scale_fill_viridis() + ggtitle(paste0(input$paramList1, " vs. ", input$paramList2, ": 2d-PDP"))
      ggplotly(pdp)
    }
  })
  
  
  # Tab: Feature Importance
  
  # Permutation Feature importance
  output$featImp <- renderPlotly({
    #browser()
    if (is.null(input$holdout)) return()
    else {
      if (input$holdout == "No") {
        pred.test = predictor.test()
      }
      else if (input$holdout == "Yes") {
        pred.test = Predictor$new(model = objects()$model_best, data = objects()$data_holdout, y = "y")
      }
    
      pfi = FeatureImp$new(predictor = pred.test, loss = "rmse")
      p = pfi$plot()
      ggplotly(p)
    }
  })
  
  
  # Tab: Local Interpretation
  
  # LIME
  output$lime.best <- renderPlotly({
    if (is.null(input$obs2)) {
      return()
    }
    else{
      lime.best = LocalModel$new(predictor(), x.interest = round(objects()$mbo_train[input$obs1,], 3))
      ggplotly(plot(lime.best))
    }
  })
  
  output$lime.worst <- renderPlotly({
    if (is.null(input$obs2)) {
      return()
    }
    else{
      lime.worst = LocalModel$new(predictor(), x.interest = round(objects()$mbo_train[input$obs2, ], 3))
      ggplotly(plot(lime.worst))
    }
  })
  
  # Shapley
  output$shap.best <- renderPlotly({
    if (is.null(input$obs2)) {
      return()
    }
    else{
      shap.best = Shapley$new(predictor(), x.interest = round(objects()$mbo_train[input$obs1,], 3))
      ggplotly(plot(shap.best))
    }
  })
  
  output$shap.worst <- renderPlotly({
    if (is.null(input$obs2)) {
      return()
    }
    else{
      shap.worst = Shapley$new(predictor(), x.interest = round(objects()$mbo_train[input$obs2, ], 3))
      ggplotly(plot(shap.worst))
    }
  })
  
  # Counterfactuals
  output$counter.table <- renderTable({
    as.data.frame(t(data.counterfactual()[[1]]$get_frequency()))
  })
  
  output$counter.plot1 <- renderPlotly({
    print("test")
    table = as.data.frame(t(data.counterfactual()[[1]]$get_frequency()))
    if (length(which(table[1,] > 0.5)) < 2) return()
    else{
      p = data.counterfactual()[[1]]$plot_parallel(features = colnames(table)[which(table[1,] > 0.5)], plot.x.interest = TRUE)
      p = p + scale_x_discrete(expand = c(0.1, 0.1), labels = colnames(table)[which(table[1,] > 0.5)])
      p = p + ggtitle(paste0("Actual prediction ", round(data.counterfactual()[[2]], 4)))
      ggplotly(p)
    }
    
  })
  
  output$counter.plot2 <- renderPlot({
    table = as.data.frame(t(data.counterfactual()[[1]]$get_frequency()))
    p = data.counterfactual()[[1]]$plot_surface(features = colnames(table)[1:2])
    p
  })
  
  
  
  
  
}