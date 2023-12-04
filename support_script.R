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
      parms$sinking_rate_alloch / parms$mean_depth
    sedimentation_autoch = parms$c_in_autoch * parms$c_dw_ratio_in_autoch *
      parms$sinking_rate_autoch / parms$mean_depth
    
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
  if(method_list$oc_fraction == "method_loi_known"){
    # Calculate from LOI data and Van Bemmelen conversion factor
    oc_fraction = parms$loi_fraction * parms$van_bemmelen_factor
  }else if(method_list$oc_fraction == "mass_balance"){
    # Change in sediment carbon content can be described as:
    # dC/dt = sedimentation - mineralisation - burial (units: mC/yr)
    # sedimentation = LSR * OCfraction_water
    # mineralisation = OCfraction * mineralisation_rate * active_sediment_depth
    # burial = LSR * OCfraction
    # Assumptions: DOC release is ignored or included in mineralisation rate
    
    # If we say that dC/dt = 0, OCfraction in the sediment can be calculated as:
    # OCfraction = (LSR * OCfraction_water) / (mineralisation_rate * active_sediment_depth + LSR)
    
    # Units: lin_sed_rate m yr-1, sed_mineralisation_rate yr-1, active_sed_depth m
    
    oc_fraction = (parms$lin_sed_rate * parms$oc_fraction_water) /
      (parms$sed_mineralisation_rate * parms$active_sed_depth + parms$lin_sed_rate)
  }else if(method_list$oc_fraction == "santoso2017_sed_profile"){
    # Santoso et al. (2017). doi:10.1007/s10750-017-3158-7
    # Equation calculates a profile of OC content in the sediment, and here we
    # assume that the content at the bottom of the active sediment layer is what
    # gets buried.
    # Note: Equation 4 in Santoso et al. probably contains a mistake: it should be
    #  *z instead of /z
    # density kg/m3; sed_mineralisation_rate yr-1; active_sed_depth m
    
    mass_acc_rate = parms$lin_sed_rate * parms$density
    
    oc_fraction = parms$oc_fraction_water * exp((-parms$sed_mineralisation_rate / mass_acc_rate) *
                                                  parms$active_sed_depth * 100) + parms$sed_nonmeta_c_fraction
  }else{
    stop("Unknown method!")
  }
  
  if(oc_fraction < 0){
    warning("OC fraction was calculated to be less than 0; set to 0 instead")
    oc_fraction = 0.0
  }else if(oc_fraction > parms$van_bemmelen_factor){
    warning("OC fraction was calculated to be higher than possible; set to ",
            parms$van_bemmelen_factor, " instead")
    oc_fraction = parms$van_bemmelen_factor
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
