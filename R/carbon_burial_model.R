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
  parms$min_rate_pom_alloch_water = parms$min_rate_pom_alloch_water20 *
    parms$min_temp_coef^(parms$lake_water_temp - 20)
  parms$min_rate_pom_autoch_water = parms$min_rate_pom_autoch_water20 *
    parms$min_temp_coef^(parms$lake_water_temp - 20)
  parms$min_rate_pom_alloch_sed = parms$min_rate_pom_alloch_sed20 *
    parms$min_temp_coef^(parms$lake_sed_temp - 20)
  parms$min_rate_pom_autoch_sed = parms$min_rate_pom_autoch_sed20 *
    parms$min_temp_coef^(parms$lake_sed_temp - 20)
  
  # Autochtonous carbon production (gC m-2 yr-1)
  parms$c_in_autoch = calc_c_input_autoch(parms, method_list)
  
  # Net sedimentation (gC m-2 yr-1)
  parms$net_sedimentation = calc_net_sedimentation(parms, method_list)
  
  # Density of the material when reaching the sediment (g m-3)
  parms$oc_fraction = (parms$net_sedimentation[["alloch"]] + parms$net_sedimentation[["autoch"]]) /
    sum(parms$net_sedimentation)
  parms$dbd_topsed = calc_dbd(parms, method_list)
  
  # Linear sedimentation rate, m yr-1 ; density in g/m3
  parms$lin_sed_rate = sum(parms$net_sedimentation) / parms$dbd_topsed
  
  # Calculate active sediment depth (m, used in calc_oc_fraction)
  parms$active_sed_depth = calc_active_sed_depth(parms, method_list)
  
  # Fraction organic carbon in the sediment, -
  parms$oc_fraction_bottomsed = calc_oc_fraction(parms, method_list)
  
  # Organic carbon burial efficiency (-)
  ocbe = parms$oc_fraction_bottomsed / parms$oc_fraction
  
  ### Calculate C burial from sedimentation rate and burial efficiency
  # gC m-2 yr-1
  c_burial = (parms$net_sedimentation[["alloch"]] * ocbe[["alloch"]] +
                parms$net_sedimentation[["autoch"]]) * ocbe[["autoch"]]
  
  if(return_all){
    return(c(lsr = parms$lin_sed_rate,
             ocbe = ocbe[["all"]],
             bur = c_burial))
  }else{
    return(c_burial)
  }
}

# # Enter all inputs here. Please give it an intuitive name, and add
# #   units and a description.
# input_parms = list(
#   trapping_efficiency_factor = 0.046, # Meaning depends on method_list$trapping_efficiency
#   # See Tan et al. (2019) and sources therein
#   lake_volume = 2E8, # m3
#   catchment_area = 140, # km2
#   lake_area = 2.3E7, # m2
#   inflow = 2.1E7, # m3 yr-1. Inflow discharge
#   c_in_alloch = 2.2E7, # gC yr-1. Allochthonous carbon input
#   mean_depth = 9, # m. Mean lake depth
#   tp = 40, # mg/m3. This should be the yearly average TP in the epilimnion
#   epilim_depth = 10, # m. This should be the average epilimnion depth during the stratified season. Minimum of mean_depth and epilim_depth is taken.
#   min_rate_pom_alloch_water20 = 0.01, # d-1. Mineralisation rate of allochthoous POM in water, at 20 degC
#   min_rate_pom_autoch_water20 = 0.1, # d-1. Mineralisation rate of autochthonous POM in water, at 20 degC
#   sink_vel_pom_water = 0.5, # m d-1. Sinking velocity of POM in water
#   oc_fraction_inflow = 0.05, # -. Mass fraction of OC in inflow (i.e. allochthonous)
#   min_rate_pom_alloch_sed20 = 0.003 * 365, # yr-1. Mineralisation rate of allochthonous POM in sediment, at 20 degC
#   min_rate_pom_autoch_sed20 = 0.03 * 365, # yr-1. Mineralisation rate of autochthonous POM in sediment, at 20 degC
#   active_sed_depth = 0.1, # m. Active sediment depth. All C below this layer is assumed to be buried.
#   sed_nonmeta_c_fraction = 0.0, # -. Fraction of non-metabolisable C in the sediment (i.e. always buried)
# 
#   lake_water_temp = 20, # degC. Temperature of lake water column
#   lake_sed_temp = 20, # degC. Temperature of lake sediment
#   min_temp_coef = 1.07, # Water temperature-scaling: Maavara et al. 2017 and Hanson et al. 2014
# 
#   # Universal constants
#   c_dw_mass_ratio = 0.5 # -. Fraction of C in organic matter
# )
# 
# # Add in this list what methods are used in the calculations.
# #   We can either number them ("method1") or be more specific ("method_with_O2_and_temp"),
# #   or "method_isidorova_2019"
# method_list = list(
#   autoch_c_input = "hanson2008",
#   net_sedimentation = "trapping_efficiency",
#   trapping_efficiency = "brown",
#   active_sed_depth = "method0",
#   oc_fraction = "mass_balance2", # "santoso2017_sed_profile",
#   dbd = "keogh"
# )
# 
# test_parms = input_parms
# test_parms$c_in_alloch = 1E7
# carbon_burial_model(test_parms, method_list)
# test_parms$c_in_alloch = 1E8
# carbon_burial_model(test_parms, method_list)
# test_parms$tp = 80
# carbon_burial_model(test_parms, method_list)
# test_parms$mean_depth = 20
# carbon_burial_model(test_parms, method_list)
