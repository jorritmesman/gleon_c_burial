#### Function to calculate carbon burial in a lake

# Set directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load supporting functions
source("support_script.R")

# Enter all inputs here. Please give it an intuitive name, and add
#   units and a description.
input_parms = list(
  mean_depth = 8, # m. Mean lake depth
  time_period = 100, # yr. Time period considered in the model before C is considered "buried"
  c_in_alloch = 1000, # gC m-2 yr-1. External (allochtonous) carbon input from the catchment
  c_dw_ratio_in_alloch = 0.36, # -. Mass fraction of C in allochthonous input
  c_dw_ratio_in_autoch = 0.36, # -. Mass fraction of C in autochtonous input
  tsi_index = 50, # -. Carlson (1977) trophic state index
  sinking_rate_alloch = 10, # m yr-1. Sinking rate (in the water column) of allochtonous carbon
  sinking_rate_autoch = 365, # m yr-1. Sinking rate (in the water column) of autochtonous carbon
  resusp_fraction = 0.9, # -. A fixed percentage of gross sedimentation rate is resuspended
  loi_fraction = 0.17, # -. Fraction loss-on-ignition
  sed_om_density =  1400000, # g/m3. Density of sediment organic matter
  
  # Universal constants
  van_bemmelen_factor = 0.58 # -. Fraction of C in organic matter 
)

# Add in this list what methods are used in the calculations.
#   We can either number them ("method1") or be more specific ("method_with_O2_and_temp"),
#   or "method_isidorova_2019"
method_list = list(
  autoch_c_input = "method1",
  gross_sedimentation = "method0",
  resuspension = "method0",
  oc_fraction = "method_loi_known",
  sed_om_density = "method0"
)

carbon_burial_model = function(input_parms, method_list){
  parms = input_parms
  
  # Autochtonous carbon production (gC m-2 )
  parms$c_in_autoch = calc_c_input_autoch(parms, method_list)
  
  # Gross sedimentation and resuspension rates (gDW m-2 yr-1)
  parms$gross_sedimentation = calc_gross_sedimentation(parms, method_list)
  parms$resuspension = calc_resuspension(parms, method_list)
  
  # Sediment density
  parms$density = calc_sed_om_density(parms, method_list)
  
  # Linear sedimentation rate, m yr-1 ; density in g/m3
  parms$lin_sed_rate = (parms$gross_sedimentation - parms$resuspension)/parms$density
  
  # Percentage organic carbon in the sediment, -
  parms$oc_fraction = calc_oc_fraction(parms, method_list)
  
  ### Calculate C burial from sedimentation rate and sediment properties
  # gC m-2 yr-1
  c_burial = parms$lin_sed_rate * parms$oc_fraction * parms$density
  
  return(c_burial)
}

# test_parms = input_parms
# test_parms$c_in_alloch = 1000
# carbon_burial_model(test_parms, method_list)
# test_parms$c_in_alloch = 1500
# carbon_burial_model(test_parms, method_list)
# test_parms$tsi_index = 20
# carbon_burial_model(test_parms, method_list)
# test_parms$lake_depth = 30
# carbon_burial_model(test_parms, method_list)
