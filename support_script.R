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
  }else if(method_list$gross_sedimentation == "trapping_efficiency"){
    # Calculate trapping efficiency
    if(is.null(method_list$trapping_efficiency)){
      stop("Need to set 'method_list$trapping_efficiency' to use this method!")
    }
    
    # lake_volume in m3, catchment_area in km2, inflow in m3/yr
    if(method_list$trapping_efficiency == "brown"){
      par_d = parms$trapping_efficiency_factor
      
      trap_eff = 1 - 1 / (1 + 0.0021 * par_d * parms$lake_volume / parms$catchment_area)
    }else if(method_list$trapping_efficiency == "brune"){
      alpha = parms$trapping_efficiency_factor
      res_time = parms$lake_volume / parms$inflow # yr
      
      trap_eff = 1 - 0.05 * alpha / sqrt(res_time)
    }else if(method_list$trapping_efficiency == "siyam"){
      beta = parms$trapping_efficiency_factor
      
      trap_eff = exp(-beta * parms$inflow / parms$lake_volume)
    }else if(method_list$trapping_efficiency == "jothiprakash"){
      res_time = parms$lake_volume / parms$inflow # yr
      
      trap_eff = (8000 - 36 * res_time^-0.78) /
        (78.85 + res_time^-0.78)
      trap_eff = min(1, trap_eff / 100)
    }else{
      stop("Unknown method for 'trapping_efficiency'!")
    }
    
    sinking_flux = parms$c_in_alloch / parms$lake_area * trap_eff
    
    gross_sedimentation = sinking_flux * exp(-parms$min_rate_pom_water *
                                               parms$mean_depth /
                                               parms$sink_vel_pom_water)
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
    
    # Units: lin_sed_rate m yr-1, min_rate_pom_sed yr-1, active_sed_depth m
    
    oc_fraction = (parms$lin_sed_rate * parms$oc_fraction_water) /
      (parms$min_rate_pom_sed * parms$active_sed_depth + parms$lin_sed_rate)
  }else if(method_list$oc_fraction == "santoso2017_sed_profile"){
    # Santoso et al. (2017). doi:10.1007/s10750-017-3158-7
    # Equation calculates a profile of OC content in the sediment, and here we
    # assume that the content at the bottom of the active sediment layer is what
    # gets buried.
    # Note: Equation 4 in Santoso et al. probably contains a mistake: it should be
    #  *z instead of /z
    # min_rate_pom_sed yr-1; active_sed_depth m
    
    mass_acc_rate = parms$net_sedimentation
    
    oc_fraction = parms$oc_fraction_water * exp((-parms$min_rate_pom_sed / mass_acc_rate) *
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

# DBD = Dry Bulk Density, g/m3
calc_dbd = function(oc_fraction, method_list){
  if(method_list$dbd == "method0"){
    dbd = parms$dbd
  }else if(method_list$dbd == "kastowski"){
    # Kastowski et al. (2011), doi:10.1029/2010GB003874
    # Note: the second equation uses a different unit (mg/g instead of %)
    
    if(oc_fraction < 0.06){
      dbd = 1.665*(oc_fraction * 100)^-0.887
    }else{
      dbd = 1.776 - 0.363 * log(oc_fraction * 1000)
    }
  }else{
    stop("Unknown method!")
  }
  
  # Conversion from g/cm3 to g/m3
  return(dbd * 1000000)
}
