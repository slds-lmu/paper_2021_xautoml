server <- function(input, output) {
  
  # get datasets and model for chosen dataset and lambda
  objects <- eventReactive(list(input$dataset, input$lambda),{
    data = get(paste0("data_",tolower(input$dataset)))
    #browser()
    print(head(data))
    lambda = as.numeric(input$lambda)
    print(lambda)
    object = get_objects(data, lambda)
    print(object)
    return(list(model = object$model[[1]], mbo_train = object$data_train,
    lhs_test = object$data_test))
  })
  
  # define predictor 
  predictor <- reactive({
    Predictor$new(model = objects()$model, data = objects()$mbo_train)
  })
  
  
  # Render UI Elements
  
  # Feature Effects
  output$params <- renderUI({
    selectInput("paramList", "Choose Feature", colnames(objects()$lhs_test)[-which(colnames(objects()$lhs_test) == "y")])
  })
  
  output$center <- renderUI({
    selectInput("center", "Center ice curves?", c("No", "Yes"))
  })
  
  output$params1 <- renderUI({
    selectInput("paramList1", "Choose Feature", colnames(objects()$lhs_test)[-which(colnames(objects()$lhs_test) == "y")])
  })
  
  output$params2 <- renderUI({
    selectInput("paramList2", "Choose Feature", colnames(objects()$lhs_test)[-which(colnames(objects()$lhs_test) %in% c("y",input$paramList1))])
  })
  
  # Local Interpretation
  output$obs1 <- renderUI({
    selectInput("obs1", "Choose first observation", choices = c(1:nrow(objects()$mbo_train)), selected = which.min(objects()$mbo_train$y))
  })
  
  output$obs2 <- renderUI({
    selectInput("obs2", "Choose second observation", choices = c(1:nrow(objects()$mbo_train)), selected = which.max(objects()$mbo_train$y))
  })
  
  
  
  # Plot / Summary Output
  
  # Tab: Descriptive
  
  # Correlation Plot Train
  output$corr_train <- renderPlot({
    corrplot(cor(objects()$mbo_train), method = "color", type = "upper", order = "FPC",
             addCoef.col = "black", tl.col = "black", diag = FALSE)
    title(main = "MBO (train set)")
  })
  
  # Correlation Plot Test
  output$corr_test <- renderPlot({
    corrplot(cor(objects()$lhs_test), method = "color", type = "upper", order = "FPC",
             addCoef.col = "black", tl.col = "black", diag = FALSE)
    title(main = "randomLHS (test set)")
  })
  
  # Summary Statistics Train and Test
  output$sum_train <- renderPrint({
    summary(objects()$mbo_train)
  })
  
  output$sum_test <- renderPrint({
    summary(objects()$lhs_test)
  })
  
  
  # Tab: Feature Effects
  
  # ALE Plot  
  output$ale <- renderPlot({
    print("test")
    effects.ale = FeatureEffect$new(predictor = predictor(), feature = input$paramList, method = "ale")
    effects.ale$plot() + ggtitle("ALE plot")
  })
  
  # PDP + ICE Plot
  output$pdp <- renderPlot({
    print("test")
    # pdp
    effects.pdp = FeatureEffect$new(predictor = predictor(), feature = input$paramList, method = "pdp")
    # ice
    if (input$center == "No") effects = FeatureEffect$new(predictor = predictor(), feature = input$paramList, method = "pdp+ice")
    else if (input$center == "Yes") effects = FeatureEffect$new(predictor = predictor(), feature = input$paramList, method = "pdp+ice", center.at = 0)
    #browser()
    edata = na.omit(data.table(
      id = effects$results$.id,
      eta = as.numeric(unlist(objects()$mbo_train[input$paramList]))[effects$results$.id],
      eta.diff = abs(as.numeric(unlist(effects$results[input$paramList])) - as.numeric(unlist(objects()$mbo_train[input$paramList]))[effects$results$.id]),
      y = effects$results$.value))
    edata = edata[, list("eta" = eta[which.min(eta.diff)],
                         "y" = y[which.min(eta.diff)]), by = "id"]
    pdp = effects.pdp$plot() + ggtitle("PD plot") 
    ice = effects$plot() + 
      ggtitle("ICE curves + PD plot") + 
      geom_point(data = edata, aes(x = eta, y = y), alpha = 0.5, shape = 4) #, col = nrounds)) + scale_color_viridis()
    pdp + ice
  })
  
  # 2d interactions: HStatistics
  output$hstat <- renderPlot({
    plot(Interaction$new(predictor())) + ggtitle("H-statistics")
  })
  
  output$hstat2feat <- renderPlot({
    plot(Interaction$new(predictor(), feature = input$paramList1)) + ggtitle(paste0(input$paramList1, ": H-statistics"))
  })
  
  # 2 dim ale
  output$ale2d <- renderPlot({
    #browser()
    ale2d = FeatureEffect$new(predictor = predictor(), feature = c(input$paramList1, input$paramList2), 
                              method = "ale", grid.size = 50)
    ale = ale2d$plot() + scale_fill_viridis() + ggtitle(paste0(input$paramList1, " vs. ", input$paramList2, ": 2d-ALE"))
    ale
  })
  
  # 2 dim pdp
  output$pdp2d <- renderPlot({
    pdp2d = FeatureEffect$new(predictor = predictor(), feature = c(input$paramList1, input$paramList2), 
                              method = "pdp", grid.size = 50)
    pdp2d$plot() + scale_fill_viridis() + ggtitle(paste0(input$paramList1, " vs. ", input$paramList2, ": 2d-PDP"))
  })
  
  
  # Tab: Feature Importance
  
  # Permutation Feature importance
  output$featImp <- renderPlot({
    pred.test = Predictor$new(model = objects()$model, data = objects()$lhs_test, y = "y")
    pfi = FeatureImp$new(predictor = pred.test, loss = "rmse")
    pfi$plot()
  })
  
  
  # Tab: Local Interpretation
  
  # LIME
  output$lime <- renderPlot({
    lime.best = LocalModel$new(predictor(), x.interest = round(objects()$mbo_train[input$obs1,], 3), k = 5)
    lime.worst = LocalModel$new(predictor(), x.interest = round(objects()$mbo_train[input$obs2, ], 3), k = 5)
    objects()$mbo_train[c(input$obs1, input$obs2),]
    p = plot(lime.best) + plot(lime.worst)
    p
  })
  
  # Shapley
  output$shap <- renderPlot({
    shap.best = Shapley$new(predictor(), x.interest = round(objects()$mbo_train[input$obs1,], 3))
    shap.worst = Shapley$new(predictor(), x.interest = round(objects()$mbo_train[input$obs2, ], 3))
    p = plot(shap.best) + plot(shap.worst)
    p
  })
  
  
  
  
  
  
  
}