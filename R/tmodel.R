##/////////////////////////////////////////////////////////////////////////
## T-model
## Function 'tmodel' returns state variables of the T-model for all years
## given as 'simyears'
##------------------------------------------------------------------------
tmodel <- function( simyears, P0, a, cr, Hm, rho, rr, rs, L, z, y, ss, tf, tr, d, K ) {

  library(dplyr)
  
	##///////////////////////////////////////////////////////////////////
	## Parameters
	##-------------------------------------------------------------------
	## a : initial slope of the relationship between height and diameter
	a <- 116.0

	## cr : initial ratio of crown area to stem cross- sectional area ('c')
	cr <- 390.43

	## Hm : asymptotic maximum height
	Hm <- 25.33

	## rho : the density of wood
	rho <- 200.0

	## rr : Fine-root specific respiration rate (0.913 year-1; Yan and Zhao (2007))
	rr <- 0.913

	## rs : Sapwood-specific respiration rate (0.044 year-1; Landsberg and Sands (2010))
	rs <- 0.044

	## L : Leaf area index within the crown (1.8; Chen et al., 2004)
	L <- 1.8

	## z : Ratio of fine-root mass to foliage area (0.17 kg C m-2; White et al. (2000))
	z <- 0.17

	## y : Yield factor (0.6; Zhang et al. (2009))
	y <- 0.6

	## ss : Specific leaf area (14; Huo and Wang (2007))
	ss <- 14.0

	## tf : Foliage turnover time (4 years; Luo (1996))
	tf <- 4.0

	## tr : Fine-root turnover time (1.04 years; Shan et al. (1993))
	tr <- 1.04

	## K : decay constant in Beer-Lambert Law
	K <- 0.5

	##/////////////////////////////////////////////////////////////////////////
	## Return variable as data frame
	##------------------------------------------------------------------------
	output <- tibble( 
	                      year            =c(),
												dD              =c(),
												dW_wood         =c(),
												D               =c(),
												H               =c(),
												Ac              =c(),
												W_wood          =c(),
												W_sapwood       =c(),
												GPP             =c(),
												Rm_sapwood      =c(),
												Rm_fineroot     =c(),
												NPP             =c(),
												turnover        =c(),
												dW_foliage_root =c()                   
												)


	##/////////////////////////////////////////////////////////////////////////
	## Initial change in diameter acts as the "seed"
	##------------------------------------------------------------------------
	dD <- 0.001
	d  <- 0.0

	for (i in 1:length(simyears)) {

		##/////////////////////////////////////////////////////////////////////////
		## Update state variables
		##------------------------------------------------------------------------
		## Change in diameter, determined from previous year; is zero in first year
		d <- d + dD

		## update height (Eq. 4)
		H <- Hm * (1.0 - exp(-a * d/Hm))

		## update crown fraction ("thickness of crown as a fraction of total height", Eq. 11)
		fc <- H/(a * d)

		## update projected crown area (Eq. 8)
		Ac <- ((pi * cr)/(4.0 * a)) * d * H

		## update total stem mass (Eq. 6)
		Ws <- (pi/8.0) * (d^2.0) * H * rho

		## update foliage mass (ss is specific leaf area)
		Wf <- Ac * L * (ss^(-1.0))

		## update sapwood mass (Ac/cr = As; As:= sapwood cross-sectional area; Eq. 14; L*cr*v=1; Hf=H(1-fc/2))
		Wss <- Ac * rho * H * (1.0 - fc/2.0) / cr

		## Root mass
		Wr <- z * ss * Wf

		##/////////////////////////////////////////////////////////////////////////
		## Get fluxes of this year
		##------------------------------------------------------------------------
		## Photosynthesis, (kg C / m2 / yr) (use 4-6 for temperate forests)
		GPP <- (Ac * P0 * (1.0 - exp(-(K * L))))

		## sapwood respiration
		Rm1 <- Wss * rs

		## root respiration
		Rm2 <- Wr * rr

		## NPP
		NPP <- y * (GPP - Rm1 - Rm2)

		## biomass production = NPP available for growth; carry over effect
		BP <- NPP######### car is the ratio of last years NPP carried to the present year, alternatively use x% of this year's NPP and (100-x)% of last year's NPP

		## compensating for turnover; corresponds to last term in Eq. 16
		TO <- Ac * L * ((1.0/(ss * tf)) + (z/tr))

		
		##/////////////////////////////////////////////////////////////////////////
		## Get geometric relationships given current state variables
		##------------------------------------------------------------------------
		## stem mass increment proportionality to diameter increment (Eq. 7, using Eq. 5)
		dWs_per_dD <- pi/8.0 * rho * d * (a * d * (1.0 - (H/Hm)) + 2.0 * H)

		## "root and foliage allocation" per unit diameter increment; corresponds to second term in Eq. 16 without dD/dt
		dHP_dD <- L * ((pi * cr)/(4.0 * a)) * (a * d * (1.0 - (H/Hm) + H)) * (1.0/ss + z)


		##/////////////////////////////////////////////////////////////////////////
		## Calculate change in diameter, stem mass and height
		##------------------------------------------------------------------------
		## change in diameter
		dD <- (BP - TO) / (dHP_dD + dWs_per_dD)

		## actual stem mass increment (proportional to diameter increment)
		dWs <- dWs_per_dD * dD

		## actual root and foliage growth
		dWfr <- dHP_dD * dD

		## key output is dD (diameter increment = 2 * ring width, in units of m)
		tmp <- tibble( 
		  year            = i,
			dD              =as.numeric(dD),
			dW_wood         =as.numeric(dWs),
			D               =as.numeric(d),
			H               =as.numeric(H),
			Ac              =as.numeric(Ac),
			W_wood          =as.numeric(Ws),
			W_sapwood       =as.numeric(Wss),
			GPP             =as.numeric(GPP),
			Rm_sapwood      =as.numeric(Rm1),
			Rm_fineroot     =as.numeric(Rm2),
			NPP             =as.numeric(BP),
			turnover        =as.numeric(TO),
			dW_foliage_root =as.numeric(dWfr)                   
			)

		output <- bind_rows( output, tmp )

	}

	return( output )

}
