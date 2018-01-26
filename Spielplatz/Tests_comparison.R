# Load package
install.packages("emdi")
library(emdi)
library(reshape)


# Code in Section Data sets ----------------------------------------------------
# Load data sets
data("eusilcA_smp")
data("eusilcA_pop")


# Generate object of class "emdi","direct" -------------------------------------
emdi_direct <- direct(y = "eqIncome", smp_data = eusilcA_smp,
  smp_domains = "district", weights = "weight", threshold = 10989.28,
  var = TRUE, boot_type = "naive", B = 50, seed = 123, na.rm = TRUE)



# Generate object of class "emdi","model" --------------------------------------
emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +
  unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow + 
  house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop, 
  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
  MSE = TRUE, seed = 100, 
  custom_indicator = list(my_max = function(y, threshold){max(y)},
                          my_min = function(y, threshold){min(y)}))






evaluate <- function(direct, model, indicator = "all", color = c("red2", "red4")) {
  
  
  ind_direct <- point_emdi(object = direct, indicator = indicator)$ind 
  selected_direct <- colnames(ind_direct)[-1]
  colnames(ind_direct) <- c("Domain", paste0(colnames(ind_direct)[-1], "_Direct"))
  
  
  precisions_direct <- mse_emdi(object = direct, indicator = indicator, CV = TRUE)
  cv_direct <- precisions_direct$ind_cv  

  
  ind_model <- point_emdi(object = model, indicator = indicator)$ind 
  selected_model <- colnames(ind_model)[-1]
  colnames(ind_model) <- c("Domain", paste0(colnames(ind_model)[-1], "_Model"))
  smp_size <- as.numeric(table(direct$framework$smp_domains_vec))
  
  
  #Direct <- direct$ind[, c("Domain", indicator)]
  #names(Direct) <- c("Domain", "Direct")
  #Direct$smp_size <- as.numeric(table(direct$framework$smp_domains_vec))
  #Model <- model$ind[, c("Domain", indicator)]
  #names(Model) <- c("Domain", "EBP")
  
  Data <- merge(ind_direct, ind_model, by = "Domain")
  
  selection_indicators <- selected_model %in% selected_direct
  selected_indicators <- selected_direct[selection_indicators]
  
  
  for (ind in selected_indicators) {
    
    data_tmp <- data.frame(Direct = Data[, paste0(ind, "_Direct")],
                           Model_based = Data[, paste0(ind, "_Model")])
    
    print(ggplot(data_tmp, aes(x = Direct, y = Model_based)) + 
            geom_point() +
            geom_smooth(method = lm, color = color[1], 
                        se = FALSE
            ) + coord_fixed() + ggtitle(ind) + ylab(label = "Model-based"))
    cat("Press [enter] to continue")
    line <- readline()
    
    data_tmp <- data_tmp[order(smp_size), ]
    data_tmp$ID <- 1:length(ind_direct$Domain)
    data_shaped <- data.frame(melt(data_tmp, id.vars = "ID"))
    names(data_shaped) <- c("ID", "Method", "value")
    
    print(ggplot(data = data_shaped, aes(x = ID, 
                                         y = value, group = Method, 
                                         colour = Method)) +
            geom_line(size = 0.7) +
            geom_point(aes(color = Method), size = 2) +
            scale_color_manual(name = "Method",
                               values = c(color[1], color[2])) + 
            scale_fill_manual(name = "Method",
                              breaks = c("Direct", "Model_based"),
                              labels = c("Direct", "Model-based")) +               
            scale_linetype_discrete(name = "Method") +
            xlab("Domain") + ylab("Value") + 
            ggtitle(ind))
    #cat("Press [enter] to continue")
    #line <- readline()
    
    if (!ind == tail(selected_indicators,1)) {
      cat("Press [enter] to continue")
      line <- readline()
    }
  }
}

evaluate(emdi_direct, emdi_model, indicator = "Poverty", color = c("red3", "dodgerblue"))




brown <- function(direct, model, indicator) {
  
  Direct <- direct$ind[, c("Domain", indicator)]
  names(Direct) <- c("Domain", "Direct")
  Direct$Var <- direct$MSE[, indicator]
  
  Model <- model$ind[, c("Domain", indicator)]
  names(Model) <- c("Domain", "EBP")
  Model$MSE <- model$MSE[, indicator]
  
  
  Data <- merge(Direct, Model, by = "Domain")
  df <- length(Data$Domain)
  
  W <- sum((Data$Direct - Data$EBP)^2 / (Data$Var + Data$MSE), na.rm = TRUE)

  p_value <- 1 - pchisq(W, df)
  
  return(list(W = W, p_value = p_value, df = df))
  
  }


brown(emdi_direct, emdi_model, indicator = "Quantile_75")


