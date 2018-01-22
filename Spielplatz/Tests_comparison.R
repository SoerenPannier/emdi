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
  
  Direct <- direct$ind[, c("Domain", indicator)]
  names(Direct) <- c("Domain", "Direct")
  Direct$smp_size <- as.numeric(table(direct$framework$smp_domains_vec))
  Model <- model$ind[, c("Domain", indicator)]
  names(Model) <- c("Domain", "EBP")
  
  Data <- merge(Direct, Model, by = "Domain")
  
  
  print(summary(lm(Direct ~ EBP, data = Data)))
  print(ggplot(Data, aes(x = Direct, y = EBP)) + 
    geom_point() +
    geom_smooth(method = lm, color = color[1] 
                #se = FALSE
                ) + coord_fixed() + ggtitle(indicator))
  Data <- Data[order(Data$smp_size), ]
  Data_shaped <- data.frame(melt(Data[, c("Domain", "Direct", "EBP")], 
                                      id = "Domain"))
  # Mean_data_shaped$smp_size <- c(Mean_data$smp_size, Mean_data$smp_size)
  # names(Mean_data_shaped) <- c("Domain", "Method", "value", "smp_size")
  Data_shaped$ID <- c(1:length(Data$Domain), 1:length(Data$Domain))
  names(Data_shaped) <- c("Domain", "Method", "value", "ID")
  print(ggplot(data = Data_shaped, aes(x = ID, y = value, group = Method, colour = Method)) +
    geom_line(size = 0.7) +
    geom_point(aes(shape = Method, color = Method), size = 2) +
    scale_color_manual(name = "Method",
                       values = c(color[1], color[2])) +                 
    scale_linetype_discrete(name = "Method") +
    xlab("Domain") + ylab("Value") + 
    ggtitle(indicator))
  
}

evaluate(emdi_direct, emdi_model, indicator = "Quantile_75")




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


