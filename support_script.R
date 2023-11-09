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
  }else if (method_list$oc_fraction == "method_hakanson2003"){
    # Hakanson, 2003. Internat. Rev. Hydrobiol., 88(5), 539-560. doi:10.1002/iroh.200310602
    # Fitted for 39 Swedish lakes, Hakanson & Peters, 1995
    # R2 = 0.81
    # There is an even more complex version (R2 = 0.86), but it has lake colour in it
    
    # Units: depths in m, areas in km2
    depth_rel = (parms$max_depth * sqrt(pi)) / (20 * sqrt(parms$lake_area))
    
    x1 = parms$lake_ph
    x2 = sqrt(parms$catchment_area / parms$lake_area)
    x3 = depth_rel
    
    loi_perc = 114 - 13.4 * x1 - 1.57 * x2 + 2.62 * x3
    
    oc_fraction = loi_perc / 100 * parms$van_bemmelen_factor
  }else if(method_list$oc_fraction == "method_downing"){
    # Downing et al. 2008. Global Biogeochemical Cycles, 22(1), doi:10.1029/2006GB002854
    # Relation fitted for 40 impoundments in Iowa.
    
    # Units: lake and watershed area in km2
    
    loi_frac = 0.093 + (0.0096 * parms$lake_area) -
      (4.2E-5 * parms$catchment_area) +
      (3.497E-7 * parms$lake_area * parms$catchment_area)
    
    # In Downing et al., they use a factor based on local measurements: 0.47 (Dean, 1974)
    oc_fraction = loi_frac * parms$van_bemmelen_factor
    
  }else if(method_list$oc_fraction == "method_hanson_simple"){
    # Hanson et al. 2004. Global Change Biology, 10(8), doi:10.1111/j.1529-8817.2003.00805.x
    # Based on equations in Table 1, but simplified, assuming steady state and no
    # differences between stratified and non-stratified. All POC (dead/alive, epi/hypo)
    # are assumed to be the same.
    
    # Units: poc_conc_water in gC m-2, particle_diamter in um, mean_depth in m
    # c_s (carbon concentration in sediment) in gC m-2
    # dry_bulk_density = g cm-3; sed_depth (active sediment depth) in cm
    
    s_f = 0.005 # d-1, conversion of POC to DIC in sediments
    
    # Carbon concentration in the sediment
    c_s = (parms$poc_conc_water * 0.0188 * (parms$particle_diameter / 2)^2 / parms$mean_depth * 2) / s_f
    
    oc_fraction = c_s / (parms$dry_bulk_density * parms$sed_depth)
    
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
