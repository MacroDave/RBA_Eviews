'___________________________________________________________________________________________________________________________________________________________________________

'  ESTIMATE NAIRU FOR AUSTRALIA
' email: david.stephan@gmail.com ;
' reference: https://www.rba.gov.au/publications/bulletin/2017/jun/pdf/bu-0617-2-estimating-the-nairu-and-the-unemployment-gap.pdf

'___________________________________________________________________________________________________________________________________________________________________________
close @all
%path = @runpath
cd %path

'Read Data File
import nairu_data.xlsx range="EVIEWS" @freq q 1959q3
delete(noerr) series01

'Set Estimation Period
sample ssest 1986q3 @last

'-----------------------------------------------------------------------------
'Data Manipulations
	'Qtly Trimmed Mean Inflation
	series dlptm= dlog(ptm)*100

	'Qtly Inflation Expectations (RBA Constructed series from MARTIN database)
	series pie_rbaq = ((1+pie_rba/100)^(1/4)-1)*100

	'Qtly Inflation Expectations (Bond Market)
	series pie_bondq = ((1+pie_bond/100)^(1/4)-1)*100

	'Qtly Non-Farm Unit Labor Costs
	series dlulc = dlog(ulc_nonfarm)*100
	
	'Year-ended Growth in Consumer Import Prices
	series dl4pimp = (log(pimp)-log(pimp(-4)))*100

	'Qtly Growth in WTI Oil Price (use WTI because longer publicly available series)
	series dlwti = dlog(wti)*100
	
	'Dummy Variable for 1997
	series dum_mod = 0
	smpl @first 1976q4 
	dum_mod =1 

	smpl @all

'-----------------------------------------------------------------------------
'Creat initial series for initialzing state space
smpl ssest
unr.hpf(lambda=1600) unrsmooth
series unrgap_ini = unr-unrsmooth

vector(1) mprior = 0
'mprior(1) = unrsmooth(@ifirst(unrsmooth)+1)
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
											gamma(1)*unrgap_ini/unr + _
											lambda(1)*(d(UNR(-1))/UNR) + _
											alpha(1)*dl4pimp(-1)

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
											gamma(2)*unrgap_ini/unr + _
											lambda(2)*(d(UNR(-1))/UNR)

		' Store estimates for later use
		!delta2=delta(2)
		!omega1=omega(1)
		!omega2=omega(2)
		!gamma2=gamma(2)
		!lambda2=lambda(2)
		!sigma2 = eq_ptm.@se 
		'***********************************************
		
		'***********************************************
		' UNR Trend Rate
		smpl ssest
		equation eq_unrsmooth.ls d(unrsmooth) = c(1)
		!sigma3=eq_unrsmooth.@se 
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
												gamma(1)*(UNR-NAIRU)/unr + _
												lambda(1)*(d(UNR(-1))/UNR) + _
												alpha(1)*dl4pimp(-1) + _
												[ename = e1, var = (sigma(1)^2)]
	
	ss_nairu.append @signal dlulc = delta(2)*pie_rbaq + _
												omega(1)*dlulc(-1) + omega(2)*dlulc(-2) + _
												gamma(2)*(UNR-NAIRU)/unr + _
												lambda(2)*(d(UNR(-1))/UNR) + _
												[ename = e2, var = (sigma(2)^2)]
	
	ss_nairu.append @state NAIRU = NAIRU(-1) + [ename = e3, var = (sigma(3)^2)]
	
	ss_nairu.append @mprior mprior
	ss_nairu.append @vprior vprior
	
	smpl ssest
	ss_nairu.ml
	ss_nairu.makestates(t=smooth) *_rbaq
	ss_nairu.makestates(t=smoothse) *_rbaq_se
	
	'Update Coef Vectors with State Space Estimated Parameters
	!delta1 = delta(1) 
	!delta2 = delta(2)
	!gamma1 = gamma(1) 
	!gamma2 = gamma(2) 
	!lambda1 = lambda(1)
	!lambda2 = lambda(2) 
	!beta1 = beta(1)
	!beta2 = beta(2)
	!beta3 = beta(3)
	!omega1 = omega(1)  
	!omega2 = omega(2) 
	!phi1 = phi(1)
	!alpha1 = alpha(1)  
	!sigma1 = sigma(1)  
	!sigma2 = sigma(2)
	!sigma3 = sigma(3)

	'Estimate State-Space System with calibrated parameters but change infaltion expectations to the bond-market implied values
	sspace ss_bond

	ss_bond.append @signal dlptm = !delta1*pie_bondq + _
												!beta1*dlptm(-1) + !beta2*dlptm(-2) + !beta3*dlptm(-3) + _
												!phi1*dlulc(-1) + _
												!gamma1*(UNR-NAIRU)/unr + _
												!lambda1*(d(UNR(-1))/UNR) + _
												!alpha1*dl4pimp(-1) + _
												[ename = e1, var = (!sigma1^2)]
	
	ss_bond.append @signal dlulc = !delta2*pie_bondq + _
												!omega1*dlulc(-1) + !omega2*dlulc(-2) + _
												!gamma2*(UNR-NAIRU)/unr + _
												!lambda2*(d(UNR(-1))/UNR) + _
												[ename = e2, var = (!sigma2^2)]
	
	ss_bond.append @state NAIRU = NAIRU(-1) + [ename = e3, var = (!sigma3^2)]
	
	ss_bond.append @mprior mprior
	ss_bond.append @vprior vprior
	
	smpl ssest
	ss_bond.ml(optmethod=legacy)
	ss_bond.makestates(t=smooth) *_bondq
	
'-----------------------------------------------------------------------------
'Plotting Results

!bound1=@qnorm(0.85)
!bound2=@qnorm(0.95)

smpl ssest
series low70=(nairu_rbaq-!bound1*nairu_rbaq_se) 
series high70=(nairu_rbaq+!bound1*nairu_rbaq_se)
series low90=(nairu_rbaq-!bound2*nairu_rbaq_se) 
series high90=(nairu_rbaq+!bound2*nairu_rbaq_se)

group g_NAIRU low90 high90 low70 high70 nairu_rbaq nairu_bondq unr
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
p_NAIRU.name(6) NAIRU Bond Market Expectations
p_NAIRU.name(7) Unemployment Rate
p_NAIRU.legend display position(2.5,0)
p_nairu.options gridnone
show p_NAIRU

p_NAIRU.save(t=pdf) NAIRU_chart.pdf

