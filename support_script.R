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
  }else if(method_list$oc_fraction == "method_hakanson_complex"){
    # Hakanson, 1995. Ecological modelling, 82, 233-245. doi:10.1016/0304-3800(94)00100-V
    # Relation fitted for 34 Swedish lakes
    # High R2 (R2 = 0.83), but complex input requirements
    # Important: only fitted and valid for "small glacial lakes"
    
    # Units: depths in m, areas in km2, catchment_altitude_range: difference
    # between the highest and lowest point in the catchment in m
    
    v_d = 3 * parms$mean_depth / parms$max_depth
    rda = parms$catchment_altitude_range / sqrt(parms$catchment_area)
    
    loi_perc = 50.378 + 0.186 * parms$till_perc + 1.641 * parms$mire_perc^0.7 -
      9.633 * log10(parms$catchment_area) - 14.104 * log10(rda) -
      7.652 * v_d
    
    oc_fraction = loi_perc / 100 * parms$van_bemmelen_factor
  }else if(method_list$oc_fraction == "method_hakanson_simple"){
    # Hakanson, 1995. Ecological modelling, 82, 233-245. doi:10.1016/0304-3800(94)00100-V
    # Relation fitted for 34 Swedish lakes
    # Less complex input requirements than "complex" method, but lower R2 (0.65)
    # Important: only fitted and valid for "small glacial lakes"
    
    # Units: depths in m, areas in km2, catchment_relief: unclear, but is it the
    # difference between the highest and lowest point in the catchment in m?
    # water_res_time in years
    
    depth_rel = (parms$max_depth * sqrt(pi)) / (20 * sqrt(parms$lake_area))
    rda = parms$catchment_altitude_range / sqrt(parms$catchment_area)
    
    loi_perc = 88.374 + 22.588 * log10(parms$lake_area / parms$catchment_area) -
      19.441 * log10(rda) + 17.306 * log10(depth_rel) -
      10.559 * log10(parms$water_res_time)
    
    oc_fraction = loi_perc / 100 * parms$van_bemmelen_factor
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
