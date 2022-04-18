close @all

'Open the file
wfcreate q 1990 2015

import(resize, mode=o) .\pottest.xlsx range=Sheet1 colhead=1 na="#N/A" @freq Q @id @date(series01) @destid @date @smpl @all

'Make the necessary transformations
	series lgdp = log(gdp)
	series dlgdp = 100*d(lgdp)
	
	smpl @all
	
	series lrer=log(rer)
	series dlrer=d(lrer)*100

'Define sample periods
	sample ssest 1980q1 2014q4
	sample ssall 1980q1 2014q4

' Hodrick Prescott filter
	lgdp.hpf(lambda=1600) lgdpt_hp
	series lgdpc_hp = lgdp - lgdpt_hp

'############################
' ESTIMATE UNOBSERVED COMPONENT MODELS
' Simple trend/cycle model of GDP augmented by philips curve equation which includes gdp cycle
'############################
' Step 1: setup coefficient vectors
	' XXX

' Step 2: Establish starting values using frequency domain filter estimates of trend and cycle
	smpl @all

	' Trend and cycle estimates
		' XXX

' Step 3: Estimate initial coefficients
	' Trend
		smpl ssest
		' XXX

	' Cycle
		smpl ssest
		' XXX

	' Inflation markup model
		smpl ssest
		' XXX

' Step 4: Store estimates for later use
	' XXX
	
' Step 5 : Estimate simple trend/cycle + markup state space model using HP estimates as starting values (uses MLE)
	' XXX
	
	'Step 6 : Initial values
		' XXX

	'Step 7 : Signal equations
		' XXX
	
	'Step 8 : State equations
		' XXX

	'Step 9 : MLE
		' XXX
	
	'Step 10 : Compute unobserved variables
		' XXX

	'Step 11 : Plot
		' XXX

