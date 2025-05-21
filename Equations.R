deriv(dS_soil) <- -S_soil*beta_soil*G_soil + gamma_soil*R_soil - mu*S_soil + gsS*S_soil + alpha_water_soil*S_water + alpha_air_soil*S_air 
deriv(dR_soil) <- S_soil*beta_soil*G_soil - gamma_soil*R_soil - mus*R_soil + gsR*R_soil + alpha_water_soil*R_water + alpha_air_soil*R_air
deriv(dG_soil) <- - beta_soil*G_soil*S_soil -musG*G_soil + delta*R_soil + alpha_water_soil*G_water + alpha_air_soil*G_air

deriv(dS_water) <- -S_water*beta_soil*G_water + gamma_soil*R_water - muw*S_water + gwS*S_water + alpha_soil_water*S_soil + alpha_air_water*S_air 
deriv(dR_water) <- S_water*beta_soil*G_water - gamma_soil*R_water - muw*R_water + gwR*R_water + alpha_soil_water*R_soil + alpha_air_water*R_air
deriv(dG_water) <- - beta_soil*G_water*S_water -muwG*G_water + delta*R_water + alpha_soil_water*G_water + alpha_air_water*G_air

deriv(dS_air) <- -S_air*beta_soil*G_air + gamma_soil*R_air - mua*S_air + alpha_soil_air*S_soil + alpha_water_air*S_water 
deriv(dR_air) <- S_air*beta_soil*G_air - gamma_soil*R_air - mua*R_air  + alpha_soil_air*R_soil + alpha_water_air*R_water
deriv(dG_air) <- - beta_soil*G_air*S_air -muaG*G_air + delta*R_air + alpha_soil_air*G_soil + alpha_water_air*G_water

deriv(dS_plant) <- -S_plant*beta_soil*G_plant + gamma_soil*R_plant - mup*S_plant + alpha_soil_plant*S_soil + alpha_water_air*S_water 
deriv(dR_plant) <- S_plant*beta_soil*G_plant - gamma_soil*R_plant - mup*R_plant + alpha_soil_plant*R_soil + alpha_water_air*R_water
deriv(dG_plant) <- - beta_soil*G_plant*S_plant -mupG*G_plant + delta*R_plant + alpha_soil_plant*G_soil + alpha_water_air*G_water



initial(dS_soil)<-dS_soil_0
initial(dR_soil)<-dR_soil_0
initial(dG_soil)<-dG_soil_0
initial(dS_soil)<-dS_soil_0
initial(dR_soil)<-dR_soil_0
initial(dG_soil)<-dG_soil_0

N_age <- user()
N_age <- user()
N_age <- user()

  
  
  
  