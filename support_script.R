# Supporting functions for the carbon_burial_model.R script

# (Nearly) all functions should take the "parms" and "method_list" arguments


# Calculate Carbon input as a result of primary production
# gC m-2 yr-1
calc_c_input_autoch = function(parms, method_list){
  
  if(method_list$autoch_c_input == "method0"){
    # Constant, provided as input
    c_input_autoch = parms$autoch_c_input
  }else if(method_list$autoch_c_input == "method1"){
    # Calculate based on Carlson Trophic State Index
    ### DUMMY RELATIONSHIP, for testing only! ###
    c_input_autoch = parms$tsi_index * 20
  }else{
    stop("Unknown method!")
  }
  
  return(c_input_autoch)
}

# Calculate gross sedimentation rate
# gDW m-2 yr-1
calc_gross_sedimentation = function(parms, method_list){
  if(method_list$gross_sedimentation == "method0"){
    # Assume no decomposition in the water column, only use sinking rates
    sedimentation_alloch = parms$c_in_alloch * parms$c_dw_ratio_in_alloch *
      parms$sinking_rate_alloch / parms$lake_depth
    sedimentation_autoch = parms$c_in_autoch * parms$c_dw_ratio_in_autoch *
      parms$sinking_rate_autoch / parms$lake_depth
    
    gross_sedimentation = sedimentation_alloch + sedimentation_autoch
  }else{
    stop("Unknown method!")
  }
  
  return(gross_sedimentation)
}

# Calculate resuspension rate
# gDW m-2 yr-1
calc_resuspension = function(parms, method_list){
  if(method_list$resuspension == "method0"){
    # Assume that a fixed percentage of the gross sedimentation rate is resuspended
    resuspension = parms$gross_sedimentation * parms$resusp_fraction
  }else{
    stop("Unknown method!")
  }
  
  return(resuspension)
}

calc_oc_fraction = function(parms, method_list){
  if(method_list$oc_fraction == "method0"){
    # Calculate from LOI data and Van Bemmelen conversion factor
    oc_fraction = parms$loi_fraction * parms$van_bemmelen_factor
  }else{
    stop("Unknown method!")
  }
  
  return(oc_fraction)
}

calc_sed_om_density = function(parms, method_list){
  if(method_list$sed_om_density == "method0"){
    sed_om_density = parms$sed_om_density
  }else{
    stop("Unknown method!")
  }
  
  return(sed_om_density)
}
