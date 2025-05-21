
model <- function(t, pop, param){
  
  with(as.list(c(pop, param)), {
    
    # depot
    alpha_w_s_S = dS
    alpha_w_s_R = dR
    alpha_w_s_G = dG
    
    # lessivage
    alpha_s_w_S = roS
    alpha_s_w_R = roR
    alpha_s_w_G = roG
    
    # ev = evaporation ; Temp = temperature ; Tref = temperature de référence
    alpha_w_a = ev*Temp/Tref
    
    # r = rain drop from air ; R = rainfall ; Ref = rainfall de reference 
    alpha_a_w = r * R/Ref
    alpha_a_s = r * R/Ref
    
    # wind ; H = air humidity ; Href = humidité de référence
    alpha_s_a = wind * H/Href
    
    
    dS_s <- -S_s*betas*hs*G_s + gammas*R_s - mus*S_s + gsS*S_s + alpha_w_s_S*S_w + alpha_a_s*S_a 
    dR_s <- S_s*betas*hs*G_s - gammas*R_s - mus*R_s + gsR*R_s + alpha_w_s_R*R_w + alpha_a_s*R_a
    dG_s <- - betas*hs*G_s*S_s -musG*G_s + deltas*R_s + alpha_w_s_G*G_w + alpha_a_s*G_a
    
    dS_w <- -S_w*betaw*G_w + gammaw*R_w - muw*S_w + gwS*S_w + alpha_s_w_S*S_s + alpha_a_w*S_a 
    dR_w <- S_w*betaw*G_w - gammaw*R_w - muw*R_w + gwR*R_w + alpha_s_w_R*R_s + alpha_a_w*R_a
    dG_w <- - betaw*G_w*S_w -muwG*G_w + deltaw*R_w + alpha_s_w_G*G_w + alpha_a_w*G_a
    
    dS_a <- -S_a*betaa*G_a + gammaa*R_a - mua*S_a + alpha_s_a*S_s + alpha_w_a*S_w 
    dR_a <- S_a*betaa*G_a - gammaa*R_a - mua*R_a+ alpha_s_a*R_s + alpha_w_a*R_w
    dG_a <- - betaa*G_a*S_a -muaG*G_a + deltaa*R_a + alpha_s_a*G_s + alpha_w_a*G_w
    
    dS_p <- -S_p*betas*G_p + gammas*R_p - mup*S_p  + alpha_s_p*S_s + alpha_w_p*S_w 
    dR_p <- S_p*betas*G_p - gammas*R_p - mup*R_p + alpha_s_p*R_s + alpha_w_p*R_w
    dG_p <- - betas*G_p*S_p -mupG*G_p + deltap*R_p + alpha_s_p*G_s + alpha_w_p*G_w
    
    
    res<-c(dS_s, dR_s, dG_s,dS_w,dR_w,dG_w,dS_a,dR_a,dG_a,dS_p,dR_p,dG_p)
    
    list(res)
    
  })
  
}