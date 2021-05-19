'___________________________________________________________________________________________________________________________________________________________________________

'  ESTIMATE NAIRU FOR AUSTRALIA
' email: david.stephan@gmail.com ;
' reference: https://www.rba.gov.au/publications/bulletin/2017/jun/pdf/bu-0617-2-estimating-the-nairu-and-the-unemployment-gap.pdf

'___________________________________________________________________________________________________________________________________________________________________________

close @wf
%path = @runpath
cd %path

!import_data = 0 'Set to 1 if you want to load the rawdata from ABS/RBA/FRED. Set to zero if you want it to load the previously imported database

'-----------------------------------------------------------------------------
if !import_data = 1 then

	'Import Rawdata from ABS/RBA/FRED

	'Create Workfile to populate with ABS/RBA Data
	wfcreate(wf=NAIRU, page=NAIRU) q 1959q3 2021q4

	'Set Strings to Load Data using R packages readabs and readrba
	%rpath = @replace(@runpath,"\","/")
	%setwd = "setwd("+"""" + @left(%rpath,@len(%rpath)-1) + """)"

	'Enter API Keys for Downloading Data from FRED
	'Register at FRED websites for API keys
	%FRED = "819674707cc9b3a42385749283aacc6e"

	'Open R connection
	xopen(r)

	'Set wd so can access and store R dataframes
	xrun {%setwd}

	'Check for R packages
	xpackage tidyverse
	xpackage fredr

	'Turn on active R 
	xon

	'Set FRED Key for Downloading Oil Price
	fredr_set_key(%FRED)

	MCOILWTICO <- fredr(series_id = "MCOILWTICO", observation_start = as.Date("1959-07-01"), frequency = "q")
	MCOILWTICO <- MCOILWTICO %>% rename(MCOILWTICO = value) %>% dplyr::select(date, MCOILWTICO)

	xoff

	xget(type=series) MCOILWTICO
	rename MCOILWTICO WTI

	'Download RBA and ABS Data
	xpackage readabs
	xpackage readrba
	xpackage reshape2
	xpackage lubridate
	xpackage tidyverse
	xpackage zoo
	xpackage Quandl
	xpackage data.table

	xon

		'---------------------------------------------------------------------------------------------------------
		'Download Most Recent ABS Data
		'---------------------------------------------------------------------------------------------------------
		'Import Data from ABS Website
		abs_5206 <- read_abs(series_id = c("A2304402X", "A2302915V"))
		abs_6202 <- read_abs(series_id = c("A84423043C", "A84423047L", "A84423091W"))
		abs_6457 <- read_abs(series_id = c("A2298279F"))
		abs_1364 <- read_abs(series_id = c("A2454521V", "A2454517C"))

		'---------------------------------------------------------------------------------------------------------		
		'CLEANUP ABS SPREADSHEETS
		'---------------------------------------------------------------------------------------------------------
		'5206.0 Australian National Accounts: National Income, Expenditure and Product
		R_5206 <- abs_5206 %>%  filter(series_id %in% c("A2304402X", "A2302915V")) %>% mutate(date = zoo::as.yearqtr(date)) %>% dplyr::select(date, series_id, value) 
		R_5206 <- distinct(R_5206,date,series_id, .keep_all= TRUE)
		R_5206 <- dcast(R_5206, date ~ series_id)
		
		'Historic Labor Market Data
		R_1364 <- abs_1364 %>% filter(series_id %in% c("A2454521V", "A2454517C", "A2454568C", "A2454516A", "A2454517C", "A2454518F")) %>% mutate(date = zoo::as.yearqtr(date)) %>% dplyr::select(date, series_id, value) 
		R_1364 <- distinct(R_1364,date,series_id, .keep_all= TRUE)
		R_1364 <- dcast(R_1364, date ~ series_id)
		
		'6457.0 International Trade Price Indexes, Australia
		R_6457 <- abs_6457 %>% filter(series_id %in% c("A2298279F")) %>% mutate(date = zoo::as.yearqtr(date)) %>% dplyr::select(date, value)
		R_6457 <- R_6457 %>% rename(A2298279F = value)
			
		'6202.0 Labour Force, Australia - Monthly
		R_6202 <- abs_6202 %>% filter(series_id %in% c("A84423043C", "A84423047L", "A84423091W")) %>% dplyr::select(date, series_id, value)
		R_6202 <- distinct(R_6202,date,series_id, .keep_all= TRUE)
		R_6202 <- dcast(R_6202, date ~ series_id)
		R_6202 <- R_6202 %>% group_by(date=floor_date(date, "quarter")) %>% summarize(A84423043C=mean(A84423043C), A84423047L=mean(A84423047L), A84423091W=mean(A84423091W)) %>% mutate(date = zoo::as.yearqtr(date))
		
		'Import RBA Data'
			'Trimmed-Mean Inflation
			rba_g1 <- read_rba(series_id = c("GCPIOCPMTMQP")) 
			R_g1 <- rba_g1 %>% filter(series_id %in% c("GCPIOCPMTMQP")) %>% mutate(date = zoo::as.yearqtr(date)) %>% dplyr::select(date, series_id, value)
			R_g1 <- distinct(R_g1,date,series_id, .keep_all= TRUE)
			R_g1 <- dcast(R_g1, date ~ series_id)
			
			'Bond-market inflation expectations
			rba_g3 <- read_rba(series_id = c("GBONYLD")) 
			R_g3 <- rba_g3 %>% filter(series_id %in% c("GBONYLD")) %>% mutate(date = zoo::as.yearqtr(date)) %>% dplyr::select(date, series_id, value)
			R_g3 <- distinct(R_g3,date,series_id, .keep_all= TRUE)
			R_g3 <- dcast(R_g3, date ~ series_id)
		
	xrun NAIRU_data <- list(R_5206, R_1364, R_6457, R_6202, R_g1, R_g3) %>% Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="date"), .)

	xoff
	pageselect NAIRU
	xget(type=series) NAIRU_data
	xclose

	'-----------------------------------------------------------------------------
	'Data Manipulations

	smpl @all

	'Rename Variables
		'Real GDP
		rename A2304402X Y
		'Nominal Household Compensation of Employees
		rename A2302915V NHCOE
		'Employment
		rename a84423043c LE
		'Labor Force
		rename A84423047L LF
		'Population
		rename A84423091W LPOP
		'Historical Population and Unemployment (Level)
		rename A2454517C pop_hist
		rename A2454521V ur_hist
		'Consumer Import Prices
		rename A2298279F pmcg
		'Trimmed Mean
		rename gcpiocpmtmqp PTM
		'Bond Yield Price Expectations
		rename gbonyld pie_bond

	'Unemployment Rate
	if @isobject("LUR") =0 then
		series LUR = 100*(1-LE/LF)
		'Unemployment Rate
		series LUR_hist = UR_hist/POP_hist*100
		LUR=@recode(LUR=NA,LUR_hist,LUR)
	endif

	'Nominal Unit Labor Costs
	if @isobject("NULC") =0 then
		series nulc = nhcoe/y
	endif

	'Nominal Unit Labor Costs (Balassa Sam. Adj RBA method from MARTIN database)
	series nulcbs = nulc*(0.8+ 0.2*exp(0.002746*@trend))

	'RBA Inflation Expectations Series (taken from MARTIN public workfile until 2019q1 - extended from then on as a constant)
		series pie_rba = na
		pie_rba.fill(o=1985Q1) 8.5729104505509, 	8.62951430296377, 	8.58460917092402, 	8.82939572237737, 	7.55453154305291, 	7.48099228620253, 	8.3021829592707, 	7.85349593013385, 	7.84505869781592, 	7.57716010248355, 	7.58499905840695, 	7.87859196155721, 	7.53122710897927, 	7.44237263083917, 	7.56481530545209, 	7.86493631937396, 	8.45996141964736, 	8.46486395143936, 	8.09211596538892, 	7.74137281700819, 	7.2994728035539, 	7.29689089187884, 	7.01575105087526, 	6.38677761366657, 	5.15696178608959, 	4.89554683446502, 	5.07335098125335, 	4.79756696398512, 	5.27134094500089, 	4.40516837508636, 	4.08070349044666, 	3.63267466493067, 	3.58205947086067, 	3.48729277111572, 	3.50132365201209, 	3.52570024639065, 	3.47393058681921, 	3.46173322803126, 	3.58899425321041, 	3.67984251215508, 	3.73797683950885, 	3.74308180742129, 	3.52261288527179, 	3.32992155415899, 	3.16088742876717, 	3.05284659332883, 	2.99608502968431, 	2.93036533038371, 	3.01337365063288, 	2.97487236715162, 	2.82296686373404, 	2.72255958625175, 	2.77263192348226, 	2.7358513606477, 	2.65290709004575, 	2.54710464536716, 	2.54664907842952, 	2.51224730152763, 	2.53093552306571, 	2.62430474757952, 	2.5907163622568, 	2.51057184772075, 	2.48838019158015, 	2.48059583598477, 	2.3731868991153, 	2.38847135319472, 	2.4158204626897, 	2.39686383489116, 	2.44518933357952, 	2.47665583820822, 	2.48405565457904, 	2.5161708981641, 	2.53832625133724, 	2.4893657875921, 	2.4785521620139, 	2.4962916199112, 	2.50783530361791, 	2.54745294077593, 	2.54187045323263, 	2.5457304647181, 	2.61706484578048, 	2.63115282699124, 	2.61346073404365, 	2.59437113993076, 	2.64726724693431, 	2.65406347515607, 	2.66144726004589, 	2.64825225412956, 	2.64413906434308, 	2.66565165674881, 	2.71069050883535, 	2.71662916262144, 	2.72867618001188, 	2.75690214419736, 	2.7398480545502, 	2.57475182229607, 	2.5209533406765, 	2.53588160840859, 	2.61389813618642, 	2.66378906221983, 	2.72141407954205, 	2.75650054275859, 	2.71567133979247, 	2.74861557264, 	2.80800108287261, 	2.7646602822957, 	2.75614057399601, 	2.80089848701518, 	2.7004255437551, 	2.6966321633078, 	2.71548205502453, 	2.6474048860748, 	2.63603591910426, 	2.62000286113064, 	2.57901971277365, 	2.57618835329246, 	2.57671786551171, 	2.5772426324059, 	2.57125402176353, 	2.57115930442339, 	2.56316875667163, 	2.58232247049901, 	2.56548380166735, 	2.548083350073, 	2.49234054730198, 	2.49416818305888, 	2.37454348849844, 	2.37668273193634, 	2.36642464792722, 	2.35445666043058, 	2.38155512732425, 	2.3539994381746, 	2.34055113666414, 	2.31103012064833, 	2.31895145600698, 	2.33653606055428, 	2.32067729165903  
	
		'Extend to end of file
		smpl @all
		!obs = @obs(PIE_RBA)
		smpl if PIE_RBA<>na
		%lastdate=@otods(!obs)
		smpl %lastdate+1 @last
		PIE_RBA=PIE_RBA(-1)

	'RBA Historic Inflation Series (public dataset in G1 table only has core inflation back to 1982q2 but MARTIN public workfile had it back to 1966q4 in growth rates)
	smpl @all
	series ptm_rba = na
	ptm_rba.fill(o=1966q4) 0.401606425702795, 	0.402010050250908, 	0.49950049950094, 	0.694444444444798, 	0.297324083250558, 	0.194552529182516, 	0.860420650095335, 	0.0993048659390467, 	0.652376514445166, 	0.644567219152933, 	1.15942028985548, 	0.442869796279297, 	0.553505535055891, 	0.794351279787975, 	1.01289134438298, 	0.374531835206199, 	1.52243589743605, 	1.34158926728563, 	1.72727272727289, 	1.09335576114367, 	1.83028286189717, 	0.677200902934132, 	0.801924619086265, 	1.20870265914539, 	0.977777777777632, 	1.62932790224039, 	0.801924619086037, 	2.57542310522405, 	3.6953242835597, 	2.61818181818198, 	3.04449648711925, 	5.74929311969841, 	4.6615581098345, 	3.14300680984769, 	3.61726954492414, 	2.70087778528054, 	3.82608695652134, 	2.70270270270299, 	2.13723284589432, 	2.58410531448081, 	1.92307689999976, 	2.45078970000012, 	2.28471000000006, 	1.16959060000002, 	1.96560199999982, 	1.37362640000023, 	1.23138270000007, 	1.39534879999961, 	1.92307690000015, 	1.31926120000028, 	1.97842949999993, 	1.98511169999979, 	1.8001839999999, 	1.92307689999997, 	2.27258450000014, 	1.94384449999998, 	2.00285279999991, 	2.50521920000031, 	1.83299389999982, 	2.38611710000001, 	2.80112040000012, 	2.03735139999992

		'Backcast Trimmed Mean Using Historical Data from MARTIN database
		!obs = @obs(PTM)
		smpl if PTM<>na
		%firstdate=@otods(1)
		smpl @first %firstdate-1
		PTM = PTM_RBA
		smpl @all

		'Qtly Trimmed Mean Inflation
		series DLPTM = PTM/100

		'Qtly Inflation Expectations (RBA Constructed series from MARTIN database)
		series pie_rbaq = ((1+pie_rba/100)^(1/4)-1)*100

		'Qtly Inflation Expectations (Bond Market)
		series pie_bondq = ((1+pie_bond/100)^(1/4)-1)*100

		'Qtly Unit Labor Costs
		series dlulc = dlog(nulc)*100
		
		'Year-ended Growth in Consumer Import Prices
		series dl4pmcg = (log(pmcg)-log(pmcg(-4)))*100

		'Qtly Growth in WTI Oil Price (use WTI because longer publicly available series)
		series dlwti = dlog(wti)*100
		
		'Dummy Variable for 1997
		series dum_mod = 0
		smpl @first 1976q4 
		dum_mod =1 

		smpl @all
		wfsave importeddata

else

	wfopen importeddata
	
endif

'-----------------------------------------------------------------------------
'Creat initial series for initialzing state space

'Set Estimation Period
sample ssest 1986q3 2019q4

smpl ssest
LUR.hpf(lambda=1600) LURsmooth
series LURgap_ini = LUR-LURsmooth

vector(1) mprior = 0
mprior(1) = 6 ' Using Eye-balled value from RBA Paper
sym(1) vprior = 0.4 'Using RBA Paper Value

'-----------------------------------------------------------------------------
'Create Parameters/Starting Values for State Space Model

	' Setup coefficient vectors
	coef(2) delta
	coef(3) beta
	coef(1) phi
	coef(2) gamma
	coef(2) lambda
	coef(1) alpha
	coef(2) omega
	coef(3) sigma
	
	'-----------------------------------------------------------------------------
	' Estimate initial coefficients
	
		'***********************************************	
		' Trimmed Mean Inflation
		smpl ssest
		equation eq_ptm.ls dlptm = delta(1)*pie_rbaq + _
											beta(1)*dlptm(-1) + beta(2)*dlptm(-2) + beta(3)*dlptm(-3) + _
											phi(1)*dlulc(-1) + _
											gamma(1)*LURgap_ini/LUR + _
											lambda(1)*(d(LUR(-1))/LUR) + _
											alpha(1)*dl4pmcg(-1)

		' Store estimates for later use
		!delta1=delta(1)
		!beta1=beta(1)
		!beta2=beta(2)
		!beta3=beta(3)
		!phi1=phi(1)
		!gamma1=gamma(1)
		!lambda1=lambda(1)
		!alpha1=alpha(1)
		!sigma1 = eq_ptm.@se 
		'***********************************************

		'***********************************************	
		' ULC Growth
		smpl ssest
		equation eq_ulc.ls dlulc = delta(2)*pie_rbaq + _
											omega(1)*dlulc(-1) + omega(2)*dlulc(-2) + _
											gamma(2)*LURgap_ini/LUR + _
											lambda(2)*(d(LUR(-1))/LUR)

		' Store estimates for later use
		!delta2=delta(2)
		!omega1=omega(1)
		!omega2=omega(2)
		!gamma2=gamma(2)
		!lambda2=lambda(2)
		!sigma2 = eq_ptm.@se 
		'***********************************************
		
		'***********************************************
		' LUR Trend Rate
		smpl ssest
		equation eq_LURsmooth.ls d(LURsmooth) = c(1)
		!sigma3=eq_LURsmooth.@se 
		'***********************************************
	
'-----------------------------------------------------------------------------------------------------------------------------------------------
'State Space System of NAIRU Using Inflation and ULC Equations

	'********Estimate System Using RBA Estimated Inflation Expectations*********
	sspace ss_nairu
	ss_nairu.append @param delta(1) !delta1 delta(2) !delta2 
	ss_nairu.append @param gamma(1) !gamma1 gamma(2) !gamma2 
	ss_nairu.append @param lambda(1) !lambda1 lambda(2) !lambda2
	ss_nairu.append @param beta(1) !beta1 beta(2) !beta2 beta(3) !beta3
	ss_nairu.append @param omega(1) !omega1 omega(2) !omega2
	ss_nairu.append @param phi(1) !phi1
	ss_nairu.append @param alpha(1) !alpha1 
	ss_nairu.append @param sigma(1) !sigma1 sigma(2) !sigma2 sigma(3) !sigma3
	
	ss_nairu.append @signal dlptm = delta(1)*pie_rbaq + _
												beta(1)*dlptm(-1) + beta(2)*dlptm(-2) + beta(3)*dlptm(-3) + _
												phi(1)*dlulc(-1) + _
												gamma(1)*(LUR-NAIRU)/LUR + _
												lambda(1)*(d(LUR(-1))/LUR) + _
												alpha(1)*dl4pmcg(-1) + _
												[ename = e1, var = (sigma(1)^2)]
	
	ss_nairu.append @signal dlulc = delta(2)*pie_rbaq + _
												omega(1)*dlulc(-1) + omega(2)*dlulc(-2) + _
												gamma(2)*(LUR-NAIRU)/LUR + _
												lambda(2)*(d(LUR(-1))/LUR) + _
												[ename = e2, var = (sigma(2)^2)]
	
	ss_nairu.append @state NAIRU = NAIRU(-1) + [ename = e3, var = (sigma(3)^2)]
	
	ss_nairu.append @mprior mprior
	ss_nairu.append @vprior vprior
	
	smpl ssest
	ss_nairu.ml

	smpl ssest
	ss_nairu.makestates(t=smooth) *_rbaq
	ss_nairu.makestates(t=smoothse) *_rbaq_se
	

'-----------------------------------------------------------------------------
'Plotting Results

!bound1=@qnorm(0.85)
!bound2=@qnorm(0.95)

smpl ssest
series low70=(nairu_rbaq-!bound1*nairu_rbaq_se) 
series high70=(nairu_rbaq+!bound1*nairu_rbaq_se)
series low90=(nairu_rbaq-!bound2*nairu_rbaq_se) 
series high90=(nairu_rbaq+!bound2*nairu_rbaq_se)

group g_NAIRU low90 high90 low70 high70 nairu_rbaq LUR
freeze(p_NAIRU) g_NAIRU.mixed band(1,2,3,4) line(5,6)
p_NAIRU.setelem(1) fillcolor(@rgb(16, 189, 239))
p_NAIRU.setelem(2) fillcolor(@rgb(14, 139, 241))
p_NAIRU.setelem(1) lcolor(black)
p_NAIRU.setelem(2) lcolor(red)
p_NAIRU.setelem(3) lcolor(orange)
p_NAIRU.name(1) 90 per cent confidence interval
p_NAIRU.name(2) 
p_NAIRU.name(3) 70 per cent confidence interval
p_NAIRU.name(4) 
p_NAIRU.name(5) NAIRU RBA Inflation Expectations
p_NAIRU.name(6) Unemployment Rate
p_NAIRU.legend display position(2.5,0)
p_nairu.options gridnone
show p_NAIRU

p_NAIRU.save(t=pdf) NAIRU_chart.pdf


