library(ggplot2)
library(tidyr)
library(patchwork)
library(here)
source(here("R/tmodel.R"))

##///////////////////////////////////////////////////////////////////
## Execute T-model
##-------------------------------------------------------------------


simyears <- 1:500
P0 <- 3.0
P0 <- P0 * (1.0 - 0.1)

out_tmodel <- tmodel( simyears, P0, a, cr, Hm, rho, rr, rs, L, z, y, ss, tf, tr, d, K )


##///////////////////////////////////////////////////////////////////
## Plot some stuff
##-------------------------------------------------------------------
# par( las=1 )
# plot( simyears, 1e3 * 0.5 * out_tmodel$dD, type="l", xlab="Simulated year", ylab="Ring width (mm)" )  #, ylim=c(0,5) )
# plot( simyears, out_tmodel$Ac, type="l", xlab="Simulated year", ylab="Crown area (m2)" )
# plot( simyears, out_tmodel$NPP, type="l", xlab="Simulated year", ylab="NPP" )
# 
# plot( simyears, out_tmodel$GPP, type="l", xlab="Simulated year", ylab="GPP" )
# lines( simyears, out_tmodel$Rm_fineroot + out_tmodel$Rm_sapwood, col="red" )
# 
# plot( simyears, (1 - ((out_tmodel$Rm_fineroot + out_tmodel$Rm_sapwood) / out_tmodel$GPP)), type="l", xlab="Simulated year", ylab="GPP" )
# plot( out_tmodel$D, out_tmodel$H, type="l", xlab="DBH (m)", ylab="Height (m)", xlim=c(0.0,0.8), ylim=c(0,30) )


gg1 <- out_tmodel %>% 
  mutate(Rm = Rm_fineroot + Rm_sapwood) %>% 
  pivot_longer(c(GPP, Rm), names_to = "Variable", values_to = "Value") %>% 
  dplyr::select(year, Variable, Value) %>% 
  ggplot(aes(x = year, y = Value, color = Variable)) +
  geom_line() +
  labs(title = "T-model", x = "Simulation year", y = "Flux (kg C m-2 yr-1)")

gg2 <- out_tmodel %>% 
  mutate(CUE = 1 - ((Rm_fineroot + Rm_sapwood) / GPP)) %>% 
  ggplot(aes(x = year, y = CUE)) +
  geom_line() +
  labs(title = "T-model", x = "Simulation year", y = "CUE (fraction)")

gg1 + gg2

ggsave("cue_tmodel.pdf", width = 8, height = 4)

# Height
out_tmodel |> 
	ggplot(aes(D, H)) +
	geom_line()

# Crown area
out_tmodel |> 
	ggplot(aes(D, Ac)) +
	geom_line()

# Diameter growth
out_tmodel |>
	ggplot(aes(D, dD)) +
	geom_line()

out_tmodel |>
	tidyr::pivot_longer(
		cols = all_of(c("W_wood", "W_sapwood")),
		values_to = "var",
		names_to = "var_name"
	) |>
	ggplot(aes(D, var, color = var_name)) +
	geom_line()

# CUE
out_tmodel |>
	ggplot(aes(year, NPP/GPP)) +
	geom_line()

# sapwood, heartwood
out_tmodel |>
	ggplot() +
	geom_line(aes(year, W_wood), color = "brown") +
	geom_line(aes(year, W_sapwood), color = "green")