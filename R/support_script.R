#' @export

# Supporting functions for the carbon_burial_model.R script

# (Nearly) all functions should take the "parms" and "method_list" arguments


# Calculate Carbon input as a result of primary production
# gC m-2 yr-1
calc_c_input_autoch = function(parms, method_list){
  
  if(method_list$autoch_c_input == "method0"){
    # Constant, provided as input
    c_input_autoch = parms$autoch_c_input
  }else if(method_list$autoch_c_input == "hanson2008" | method_list$autoch_c_input == "hanson2008_vollenweider"){
    # Hanson et al. 2008. doi:10.1111/j.1365-2486.2004.00805.x
    # Assume a relationship between TP and GPP and that GPP controls
    # the autochthonous C input into the lake
    
    if(method_list$autoch_c_input == "hanson2008_vollenweider"){
      res_time = parms$lake_volume / parms$inflow  # yr
      
      # Maavara et al. 2015. doi:10.1073/pnas.1511797112
      Rp = 1 - 1 / (1 + 0.801 * res_time)
      
      # Vollenweider, 1975. doi:10.1007/BF02505178
      TP = parms$p_inflow * (1 - Rp) / parms$inflow * 1000 # mg/m3 (p_inflow in g/yr)
    }else{
      TP = parms$tp
    }
    gpp = exp(0.883 * log(TP)) # mmolC/m3/d
    
    # This is assumed to only happen in the epilimnion
    c_produced = gpp * min(parms$mean_depth, parms$epilim_depth, na.rm = T) # mmolC/m2/d
    
    # Convert to gC m-2 yr-1
    c_input_autoch = c_produced * 12.01 / 1000 * 365
  }else{
    stop("Unknown method!")
  }
  
  return(c_input_autoch)
}

#' @export
# Calculate net sedimentation rate
# gC m-2 yr-1
calc_net_sedimentation = function(parms, method_list){
  if(method_list$net_sedimentation == "method0"){
    # Assume no decomposition in the water column, everything sedimentates
    alloch_sedimentation = parms$c_in_alloch / parms$lake_area
    autoch_sedimentation = parms$c_in_autoch
    other_sedimentation = (1 - parms$oc_fraction_inflow) / parms$oc_fraction_inflow * alloch_sedimentation +
      (1 - parms$c_dw_mass_ratio) / parms$c_dw_mass_ratio * autoch_sedimentation
    # Other = other_alloch + other_autoch
    
    net_sedimentation = c(alloch = sedimentation_alloch, autoch = sedimentation_autoch, other = other_sedimentation)
  }else if(method_list$net_sedimentation == "trapping_efficiency"){
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
    
    alloch_oc_sedimentation_flux = parms$c_in_alloch / parms$lake_area * trap_eff
    
    alloch_sedimentation = alloch_oc_sedimentation_flux * exp(-parms$min_rate_pom_alloch_water *
                                                                parms$mean_depth /
                                                                parms$sink_vel_pom_water)
    autoch_sedimentation = parms$c_in_autoch * exp(-parms$min_rate_pom_autoch_water *
                                                     parms$mean_depth /
                                                     parms$sink_vel_pom_water)
    other_sedimentation = (1 - parms$oc_fraction_inflow) / parms$oc_fraction_inflow * alloch_oc_sedimentation_flux +
      (1 - parms$c_dw_mass_ratio) / parms$c_dw_mass_ratio * autoch_sedimentation
    # Other = other_alloch + other_autoch
    
    net_sedimentation = c(alloch = alloch_sedimentation, autoch = autoch_sedimentation, other = other_sedimentation)
  }else{
    stop("Unknown method!")
  }
  
  return(net_sedimentation)
}


#' @export

calc_active_sed_depth = function(parms, method_list){
  if(method_list$active_sed_depth == "method0"){
    active_sed_depth = parms$active_sed_depth
  }else if(method_list$active_sed_depth == "time_and_lsr"){
    active_sed_depth = parms$lin_sed_rate * parms$year_until_burial
  }else{
    stop("Unknown method!")
  }
  
  return(active_sed_depth)
}

#' @export

calc_oc_fraction = function(parms, method_list){
  if(method_list$oc_fraction == "method_loi_known"){
    # Calculate from LOI data and C:DW mass ratio
    oc_fraction = parms$loi_fraction * parms$c_dw_mass_ratio
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
    
    oc_fraction = (parms$lin_sed_rate * parms$oc_fraction) /
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
  }else if(method_list$oc_fraction == "mass_balance2"){
    # Mass balance similar to the aquatic flux
    flux_alloch_top = parms$net_sedimentation[["alloch"]]
    flux_alloch_bottom = flux_alloch_top * exp(-parms$min_rate_pom_alloch_sed *
                                                 parms$active_sed_depth /
                                                 parms$lin_sed_rate)
    
    flux_autoch_top = parms$net_sedimentation[["autoch"]]
    flux_autoch_bottom = flux_autoch_top * exp(-parms$min_rate_pom_autoch_sed *
                                                 parms$active_sed_depth /
                                                 parms$lin_sed_rate)
    
    oc_fraction_alloch = flux_alloch_bottom / sum(parms$net_sedimentation)
    oc_fraction_autoch = flux_autoch_bottom / sum(parms$net_sedimentation)
    
    oc_fraction = c(alloch = oc_fraction_alloch, autoch = oc_fraction_autoch,
                    all = oc_fraction_alloch + oc_fraction_autoch)
  }else{
    stop("Unknown method!")
  }
  
  if(length(oc_fraction) == 1L){
    if(oc_fraction < 0){
      warning("OC fraction was calculated to be less than 0; set to 0 instead")
      oc_fraction = 0.0
    }else if(oc_fraction > parms$c_dw_mass_ratio){
      warning("OC fraction was calculated to be higher than possible; set to ",
              parms$c_dw_mass_ratio, " instead")
      oc_fraction = parms$c_dw_mass_ratio
    }
  }
  
  return(oc_fraction)
}

#' @export

# DBD = Dry Bulk Density, g/m3
calc_dbd = function(parms, method_list){
  if(method_list$dbd == "method0"){
    dbd = parms$dbd
  }else if(method_list$dbd == "kastowski"){
    # Kastowski et al. (2011), doi:10.1029/2010GB003874
    # Note: the second equation uses a different unit (mg/g instead of %)
    
    if(parms$oc_fraction < 0.06){
      dbd = 1.665*(parms$oc_fraction * 100)^-0.887
    }else{
      dbd = 1.776 - 0.363 * log(parms$oc_fraction * 1000)
    }
  }else if(method_list$dbd == "dean_gorham"){
    # The first equation from Kastowski, but more stable at high conc. than 2nd
    dbd = 1.665*(parms$oc_fraction * 100)^-0.887
  }else if(method_list$dbd == "keogh"){
    # Equation 7 from Keogh et al. (2021), doi:10.1029/2021JF006231
    loi = parms$oc_fraction / parms$c_dw_mass_ratio * 100 # in %
    a_keogh = 2.296
    b_keogh = 0.139
    dbd = a_keogh / (1 + a_keogh * b_keogh * loi)
  }else{
    stop("Unknown method!")
  }
  
  # Conversion from g/cm3 to g/m3
  return(dbd * 1000000)
}
