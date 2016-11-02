# External documentation -------------------------------------------------------

#' Visualises regional disaggregrated estimates on a map
#'
#' Function \code{map_plot} creates spatial visualisations of the estimates
#' obtained by small area estimation methods or direct estimation.
#'
#' @param object an object of type emdi, containing the estimates to be 
#' visualised.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean" "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count", "Poverty_Gap", "Gini", 
#' "Quintile_Share" or the function name/s of "custom_indicator/s"; 
#' (ii) groups of indicators: "Quantiles", "Poverty" or 
#' "Inequality". Defaults to "all". Note, additional custom indicators can be 
#' defined as argument for model-based approaches (\code{link{ebp}}) and do not 
#' appear in groups of indicators even though these might belong to one of the 
#' groups.  
#' @param MSE optional logical. If TRUE, the MSE is also visualised.
#' @param CV optional logical. If TRUE, the CV is also visualised.
#' @param map_obj an \code{SpatialPolygonsDataFrame} object as defined by the
#' \code{sp} package on which the data should be visualised.
#' @param map_dom_id a character string containing the name of a variable in
#' \code{map_obj} that indicates the domains. 
#' @param map_tab a \code{data.frame} object with two columns that match the 
#' domain variable from the census data set (first column) with the domain 
#' variable in the map_obj (second column). This should only be used if the IDs 
#' in both objects differ.
#' @param col A \code{vector} of length 3 defining the lowest, mid and highest color in the plots
#' @return creates the plots demanded
#' @seealso \code{\link{ebp}}, \code{\link{emdiObject}},
#' \code{\link[maptools]{readShapePoly}}
#' @examples 
#' \dontrun{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_pop")
#' 
#' # generate emdi object with additional indicators; here via function ebp()
#' set.seed(100); emdi_model <- ebp( fixed = eqIncome ~ gender + eqsize + cash + 
#' self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
#' fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#' pov_line = 10722.66, transformation = "box.cox", L= 1, MSE = TRUE, B = 1,
#' custom_indicator = list( my_max = function(y, pov_line){max(y)},
#' my_min = function(y, pov_line){min(y)}), na.rm = TRUE, cpus = 1)
#' 
#' # Load shape file
#' load(system.file("shapes/shape_austria_dis.RData", package="emdi"))
#' 
#' # Create mapping table such that variables that indicate domains correspond
#' # in population data and shape file
#' mapping_table <- data.frame(unique(eusilcA_pop$district), 
#' unique(shape_austria_dis$NAME_2))
#' 
#' # when rgeos is not available, polygon geometry 	computations in 
#' # maptools depends on the package gpclib,
#' # which has a restricted licence. It is disabled by default; 
#' # to enable gpclib, type gpclibPermit()
#' gpclibPermit() 
#' # Create map plot for mean indicator - point and MSE estimates but no CV
#' map_plot(object = emdi_model, MSE = TRUE, CV = FALSE, 
#' map_obj = shape_austria_dis, indicator = c("Mean"), map_dom_id = "NAME_2", 
#' map_tab = mapping_table)
#' }
#' @export
#' @import maptools reshape2

map_plot <- function(object,
                     indicator = "all",
                     MSE = FALSE,
                     CV = FALSE,
                     map_obj = NULL,
                     map_dom_id = NULL,
                     map_tab = NULL,
                     col = c("green", "yellow", "red")){
  if(is.null(map_obj))
  {
    message("No Map Object has been provided. An artificial polygone is used for
            visualisation")
    map_pseudo(object = object , indicator = indicator, panelplot = FALSE, MSE = MSE,
               CV = CV)
  }
  else if(class(map_obj) != "SpatialPolygonsDataFrame" || attr(class(map_obj),
                                                               "package") != "sp"){
    stop("map_obj is not of class SpatialPolygonsDataFrame from the sp package")
  }
  else {
    
    if(length(col) != 3 || !is.vector(col))
    {
      stop("col needs to be a vector of length 3 defining the starting, mid and upper color of the map-plot")
    }
    
    plot_real(object,
              indicator = indicator,
              MSE = MSE,
              CV = CV,
              map_obj = map_obj,
              map_dom_id = map_dom_id,
              map_tab = map_tab,
              col = col
    )
  }
}

map_pseudo <- function(object, indicator, panelplot, MSE, CV)
{
  x <- y <- id <- value <- NULL #avoid note due to usage in ggplot
  values <-  estimators(object = object, indicator = indicator,
                                 MSE = MSE, CV = CV)$ind

  indicator <- colnames(values)[-1]

  tplot <- get_polygone(values = values)

  if(panelplot) {
    ggplot(tplot, aes(x=x, y=y)) + geom_polygon(aes(group=id, fill = value))
    + facet_wrap( ~ variable, ncol=ceiling(sqrt(length(unique(tplot$variable)))))
  } else {
    for(ind in indicator) {
      print(ggplot(tplot[tplot$variable == ind,], aes(x=x, y=y)) + 
              ggtitle(paste0(ind)) + geom_polygon(aes(group=id, fill = value)) )
      cat ("Press [enter] to continue")
      line <- readline()
    }
  }
}

plot_real <- function(object,
                      indicator = "all",
                      MSE = FALSE,
                      CV = FALSE,
                      map_obj = NULL,
                      map_dom_id = NULL,
                      map_tab = NULL,
                      col = col
) {
  if(!is.null(map_obj) && is.null(map_dom_id)) {
    stop("No Domain ID for the map object is given")
  }
  long <- lat <- group <- NULL
  
  
  
  map_data <- estimators(object = object, indicator = indicator,
                                  MSE=MSE, CV = CV)$ind

  if(!is.null(map_tab)){
    map_data <- merge(x = map_data, y = map_tab,  
                      by.x = "Domain", by.y = names(map_tab)[1])
    matcher <- match(map_obj@data[map_dom_id][,1], map_data[,names(map_tab)[2]])
    
    if(any(is.na(matcher))) {
      if(all(is.na(matcher))) {
        stop("Domains of map_tab and Map object do not match. Check map_tab")
      } else {
        warnings("Not all Domains of map_tab and Map object could be matched.
                 Check map_tab")
      }
    }
    map_data <- map_data[matcher,]
    map_data <- map_data[,!colnames(map_data) %in% c("Domain", 
                                                     map_dom_id, 
                                                     names(map_tab)), drop = F]
  } else {
    matcher <- match(map_obj@data[map_dom_id][,1], map_data[,"Domain"])

    if(any(is.na(matcher))) {
      if(all(is.na(matcher))){
        stop("Domain of EMDI and Map object do not match. Try using map_tab")
      } else {
        warnings("Not all Domains of EMDI and Map object could be matched.
                 Try using map_tab")
      }
    }
    map_data <- map_data[matcher,]
  }

  map_obj@data[colnames(map_data)] <- map_data

  map_obj.fort <- fortify(map_obj, region = map_dom_id)
  map_obj.fort <- merge(map_obj.fort, map_obj@data, by.x="id", by.y = map_dom_id)
  lookup <- c( "Mean"           = TRUE,
               "Head_Count"     = FALSE,
               "Poverty_Gap"    = FALSE,
               "Quintile_Share" = FALSE,
               "Gini"           = FALSE,
               "Quantile_10"    = TRUE,
               "Quantile_25"    = TRUE,
               "Median"         = TRUE,
               "Quantile_75"    = TRUE,
               "Quantile_90"    = TRUE
  )
  indicator <- colnames(map_data)
  for(ind in indicator)
  {
    #col <- c("green", "yellow", "red")
    if(ind %in% names(lookup) && lookup[ind])
    {
      col <- rev(col)
    }
    print(ggplot(map_obj.fort, aes(long, lat, group = group, fill = map_obj.fort[ind][,1])) +
            geom_polygon(color="azure3") + coord_equal() + labs(x = "", y = "", fill = ind) +
            ggtitle(gsub(pattern = "_",replacement = " ",x = ind)) +
            scale_fill_gradient2(low = col[1], mid=col[2], high = col[3],
                                 midpoint = (min(map_obj.fort[ind][,1], na.rm = T) +
                                 diff(range(c(map_obj.fort[ind][,1]))) / 2),
                                 limits = range(c(map_obj.fort[ind][,1]))) +
            theme(axis.ticks = element_blank(), axis.text = element_blank(), 
                  legend.title=element_blank())
    )
    if(ind %in% names(lookup) && lookup[ind])
    {
      col <- rev(col)
    }
    cat ("Press [enter] to continue")
    line <- readline()
  }
}

get_polygone <- function(values)
{
  if(is.null(dim(values)))
  {
    values = as.data.frame(values)
  }
  n <- nrow(values)
  cols <- ceiling(sqrt(n))
  n <- cols^2

  values["id"] <- 1:nrow(values)

  poly <- data.frame(id = rep(1:n, each = 4),
                     ordering = seq_len((n*4)),
                     x = c(0,1,1,0)+rep(0:(cols-1), each = (cols*4)),
                     y =rep(c(0,0,1,1)+rep(0:(cols-1), each = 4),cols)
  )

  combo <- merge(poly, values, by = "id", all = TRUE, sort = FALSE)
  melt(combo[order(combo$ordering),], id.vars = c("id","x","y","ordering"))
}
