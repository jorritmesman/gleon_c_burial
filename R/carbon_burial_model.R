#' Estimate carbon burial in lakes
#'
#' Function to calculate carbon burial in a lake
#'
#' @param input_parms list;
#' @param method_list list;
#' @param return_all boolean; 
#'
#' @export

carbon_burial_model = function(input_parms, method_list, return_all = F){
  parms = input_parms
  
  # Temperature-scaling
  parms$min_rate_pom_water = parms$min_rate_pom_water20 *
    parms$min_temp_coef^(parms$lake_water_temp - 20)
  parms$min_rate_pom_sed = parms$min_rate_pom_sed20 *
    parms$min_temp_coef^(parms$lake_sed_temp - 20)
  
  # Autochtonous carbon production (gC m-2 yr-1)
  parms$c_in_autoch = calc_c_input_autoch(parms, method_list)
  
  # Gross sedimentation and resuspension rates (gC m-2 yr-1)
  parms$gross_sedimentation = calc_gross_sedimentation(parms, method_list)
  parms$resuspension = calc_resuspension(parms, method_list)
  
  # Net sedimentation (gC m-2 yr-1)
  parms$net_sedimentation = parms$gross_sedimentation - parms$resuspension
  
  # Density sediment, water column (g m-3)
  parms$dbd_water = calc_dbd(parms$oc_fraction_water, method_list, parms)
  
  # Linear sedimentation rate, m yr-1 ; density in g/m3
  parms$lin_sed_rate = parms$net_sedimentation / parms$oc_fraction_water / parms$dbd_water
  
  # Calculate active sediment depth (m, used in calc_oc_fraction)
  parms$active_sed_depth = calc_active_sed_depth(parms, method_list)
  
  # Fraction organic carbon in the sediment, -
  parms$oc_fraction = calc_oc_fraction(parms, method_list)
  
  # Organic carbon burial efficiency (-)
  ocbe = parms$oc_fraction / parms$oc_fraction_water
  
  ### Calculate C burial from sedimentation rate and burial efficiency
  # gC m-2 yr-1
  c_burial = parms$net_sedimentation * ocbe
  
  if(return_all){
    return(c(lsr = parms$lin_sed_rate,
             ocbe = ocbe,
             bur = c_burial))
  }else{
    return(c_burial)
  }
  
}

# # Enter all inputs here. Please give it an intuitive name, and add
# #   units and a description.
# input_parms = list(
#   autoch_c_input = 0, # gC m-2 yr-1
#   trapping_efficiency_factor = 0.046, # Meaning depends on method_list$trapping_efficiency
#   # See Tan et al. (2019) and sources therein
#   lake_volume = 2E8, # m3
#   catchment_area = 140, # km2
#   lake_area = 2.3E7, # m2
#   inflow = 2.1E7, # m3 yr-1. Inflow discharge
#   c_in_alloch = 2.2E7, # gC yr-1. Allochthonous carbon input
#   mean_depth = 8, # m. Mean lake depth
#   min_rate_pom_water20 = 0.01, # d-1. Mineralisation rate of POM in water, at 20 degC
#   sink_vel_pom_water = 0.5, # m d-1. Sinking velocity of POM in water
#   resusp_fraction = 0.0, # -. A fixed percentage of gross sedimentation rate is resuspended
#   oc_fraction_water = 0.05, # -. Fraction of OC in sediment, in the water column
#   min_rate_pom_sed20 = 0.003 * 365, # yr-1. Mineralisation rate of POM in sediment, at 20 degC
#   active_sed_depth = 0.1, # m. Active sediment depth. All C below this layer is assumed to be buried. 
#   year_until_burial = 15, # yr. Year until sediment is considered "buried"
#   sed_nonmeta_c_fraction = 0.0, # -. Fraction of non-metabolisable C in the sediment (i.e. always buried)
#   
#   lake_water_temp = 20, # degC. Temperature of lake water column
#   lake_sed_temp = 20, # degC. Temperature of lake sediment
#   min_temp_coef = 1.07, # Water temperature-scaling: Maavara et al. 2017 and Hanson et al. 2014
#   
#   # time_period = 100, # yr. Time period considered in the model before C is considered "buried"
#   
#   # Universal constants
#   van_bemmelen_factor = 0.58 # -. Fraction of C in organic matter 
# )
# 
# # Add in this list what methods are used in the calculations.
# #   We can either number them ("method1") or be more specific ("method_with_O2_and_temp"),
# #   or "method_isidorova_2019"
# method_list = list(
#   autoch_c_input = "method0",
#   gross_sedimentation = "trapping_efficiency",
#   trapping_efficiency = "brown",
#   resuspension = "method0",
#   active_sed_depth = "method0",
#   oc_fraction = "santoso2017_sed_profile",
#   dbd = "keogh"
# )

# test_parms = input_parms
# test_parms$c_in_alloch = 1000
# carbon_burial_model(test_parms, method_list)
# test_parms$c_in_alloch = 1500
# carbon_burial_model(test_parms, method_list)
# test_parms$tsi_index = 20
# carbon_burial_model(test_parms, method_list)
# test_parms$lake_depth = 30
# carbon_burial_model(test_parms, method_list)
