library(deSolve)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

#Import model
source("Equations.R")

#Define list of parameter names
eq_param_names = c("gsS","gsR","gwS","gwR","mus","muw","mua","mup","musG",
                  "muwG","muaG","mupG","betas","betaw","betaa","betap",
                  "gammas","gammaw","gammap","gammaa",
                  "deltas","deltaw","deltap","deltaa","hs",
                  "dS","dG","dR","roS","roR","roG","wind","H","ev","r","R","Temp","alpha_s_p","alpha_w_p")
time_param_names = c("Tmax","dt")
init_param_names = c("S_s", "R_s", "G_s","S_w","R_w","G_w","S_a","R_a","G_a","S_p","R_p","G_p")

#Define parameter values for each scenario considered
param_values = data.frame(param_names = c(eq_param_names,time_param_names,init_param_names),
                          scenario1 = c(rep(1,length(eq_param_names)),100,1,rep(10^5,length(init_param_names))),
                          scenario2 = c(rep(10,length(eq_param_names)),100,1,rep(10^5,length(init_param_names)))) %>%
  pivot_longer(-param_names, names_to = "scenarios", values_to = "value") %>%
  pivot_wider(names_from = param_names, values_from = value)

#### Simulation functions ####

#Simulation function for one given scenario
simulate_one_scenario <- function(param_values,scenario){
  #Filter only the parameters for a given scenario
  param_values_scenario <- param_values %>% filter(scenarios == scenario)
  
  Time=seq(from=0,to=param_values_scenario[["Tmax"]],by=param_values_scenario[["dt"]])
  Init.cond <-  unlist(param_values_scenario %>% select(all_of(init_param_names)))
  param <-  unlist(param_values_scenario %>% select(all_of(eq_param_names)))
  
  # numerical resolution with lsoda
  sim <- as.data.frame(lsoda(y = Init.cond, times = Time, func = model, parms= param))
  
  # add parameter values and name of the scenario in the same dataframe to keep the conditions of simulation
  out <- sim %>%mutate(!!!setNames(param_values_scenario[c(eq_param_names, init_param_names)], c(eq_param_names, init_param_names)))%>%
    mutate(scenario = scenario, .after = G_p)
  
  return (out)
}

simulate_all_scenario <- function(param_values){
  # apply the simulation for each scenario considered
  out_all <- param_values %>%
    pull(scenarios) %>%
    map_dfr(~ simulate_one_scenario(param_values, .x)) 
  
  return(out_all)
}

#### Plot functions ####
plot_one_scenario<- function(out){
 out %>%
    select(time:G_p)%>%
    reshape2::melt(id="time")%>%
    mutate(reservoir = case_when(
      str_detect(variable,"_s") ~ "soil",
      str_detect(variable,"_w") ~ "water",
      str_detect(variable,"_a") ~ "air",
      str_detect(variable,"_p") ~ "plant"
    ),type = case_when(
      str_detect(variable,"S_") ~ "Sensible",
      str_detect(variable,"G_") ~ "Resistant",
      str_detect(variable,"R_") ~ "Gene"
    ))%>%
    mutate(reservoir=factor(reservoir,levels=c("soil","water","air","plant")),
           type = factor(type, levels = c("Resistant","Sensible","Gene")))%>%
    ggplot()+
    geom_line(aes(time, value, colour = type), linewidth = 0.8) +
    facet_wrap(~reservoir)+
    theme_bw() +
    labs(title =paste("Simulation"),
         x = "Time", y = "Number")
}


plot_all_scenario<- function(out_all){
  out_all %>%
    select(time:G_p,scenario)%>%
    reshape2::melt(id=c("time","scenario"))%>%
    mutate(reservoir = case_when(
      str_detect(variable,"_s") ~ "soil",
      str_detect(variable,"_w") ~ "water",
      str_detect(variable,"_a") ~ "air",
      str_detect(variable,"_p") ~ "plant"
    ),type = case_when(
      str_detect(variable,"S_") ~ "Sensible",
      str_detect(variable,"G_") ~ "Resistant",
      str_detect(variable,"R_") ~ "Gene"
    ))%>%
    mutate(reservoir=factor(reservoir,levels=c("soil","water","air","plant")),
           type = factor(type, levels = c("Resistant","Sensible","Gene")))%>%
    ggplot()+
    geom_line(aes(time, value, colour = type), linewidth = 0.8) +
    facet_grid(rows = vars(reservoir),cols = vars(scenario))+
    theme_bw() +
    labs(title =paste("Simulation"),
         x = "Time", y = "Number")
}

#### Runs ####

out = simulate_one_scenario(param_values,"scenario1")
plot_one_scenario(out)

out_all = simulate_all_scenario(param_values)
plot_all_scenario(out_all)



