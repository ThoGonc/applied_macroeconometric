close @all

'CPI

'Create quarterly workfile and load data
	wfcreate(page=q) q 1980 2021
	
	dbopen(type=eurostat, server=http://ec.europa.eu/eurostat/sdmx/diss-web/rest) eurostat
	
	%querylist = "namq_10_gdp__freq=q.geo=ea.na_item=b1gq.s_adj=sca.unit=clv10_meur namq_10_gdp__freq=q.geo=ea.na_item=b1gq.s_adj=sca.unit=clv10_meur ei_bsin_q_r2__freq=q.geo=de.indic=bs-icu-pc.s_adj=sa"
	%queryname = "gdp gfcf tuc"
	
	!i = 0
	for %var {%querylist}
		!i = !i + 1
	
		fetch(d=eurostatsdmx) {%var}
		%query = %var
		%query = @replace(%query,".","_")
		%query = @replace(%query,"=","_")
		%query = @replace(%query,"-","_")
		%name = @word(%queryname,!i)
		rename {%query} {%name}
	next

'Load annual data
	pagecreate(page=a) a 1980 2021
	pageselect a
	
	%querylist = "lfsi_emp_a_h__age=y15-64.freq=a.geo=de.indic_em=act.sex=t.unit=ths_per"
	%queryname = "pop"

	!i = 0
	for %var {%querylist}
		!i = !i + 1
	
		fetch(d=eurostatsdmx) {%var}
		%query = %var
		%query = @replace(%query,".","_")
		%query = @replace(%query,"=","_")
		%query = @replace(%query,"-","_")
		%name = @word(%queryname,!i)
		rename {%query} {%name}
		
		copy(c=dentona) {%name} q\{%name}
	next

'Load monthly data
	pagecreate(page=m) m 1980 2021
	pageselect m
	
	%querylist = "ei_bsci_m_r2__freq=m.geo=ea19.indic=bs-bci.s_adj=sa"
	%queryname = "clim"

	!i = 0
	for %var {%querylist}
		!i = !i + 1
	
		fetch(d=eurostatsdmx) {%var}
		%query = %var
		%query = @replace(%query,".","_")
		%query = @replace(%query,"=","_")
		%query = @replace(%query,"-","_")
		%name = @word(%queryname,!i)
		rename {%query} {%name}
		
		copy(c=a) {%name} q\{%name}
	next	

	pageselect q

'Extend population data
	smpl 2021q1 2021q3
		series pop = pop(-1) * (@elem(pop,2020q4)/@elem(pop,2018q4))^(1/8)
	smpl @all

'Define log data
	series lgdp = log(gdp)

'#######################
'HP filtering
'#######################
	hpf(lambda=1600) lgdp lgdppotl_hp
	series og_hp = lgdp - lgdppotl_hp

	show gdp lgdppotl_hp
	plot og_hp

'#######################
'Baxter King band-pass filter
'#######################
	lgdp.bpf(type=bk, low=6, high=36, lag=12) og_bk
	series lgdppotl_bk = NA
	series lgdppotl_bk = lgdp - og_bk

	show lgdp lgdppotl_bk
	plot og_bk

'#######################
' Christiano Fitzgerald filter band-pass filter
'#######################
	lgdp.bpf(type=cffix, low=6, high=36, lag=12, iorder=1) og_cffix
	lgdp.bpf(type=cfasym, low=6, high=32, iorder=1) og_cfasym
	series lgdppotl_cffix = NA
	series lgdppotl_cfasym = NA
	series lgdppotl_cffix = lgdp - og_cffix
	series lgdppotl_cfasym = lgdp - og_cfasym

	show lgdp lgdppotl_cffix lgdppotl_cfasym
	plot og_cffix og_cfasym

'#######################
' Compare band-pass filter
'#######################
	smpl 1995 2021
	plot og_hp og_bk og_cffix og_cfasym
	show lgdp lgdppotl_hp lgdppotl_bk lgdppotl_cffix lgdppotl_cfasym
 	smpl @all

'#######################
'Structural approach
'#######################
	'Parameters
		smpl @all
			series depr = 5
			series alpha = 0.625

	'Capital stock		
		smpl 1995q1 2021q3
			series k = NA
		smpl 1995q1 1995q1
			series  k = gfcf
		smpl 1995q2 2021q3
			series  k = k(-1)*(1-depr/100) + gfcf
		smpl @all

	'TFP
		pagestruct(end=2050q4)
		
		series tfp_raw = gdp / (pop^alpha*k(-1)^(1-alpha))
	
		series temp = @recode(tfp_raw<>na,@trend,na)+1 
			scalar first = @min(temp)
			scalar last = @max(temp)
			%firstdate=@otod(first)
			%lastdate=@otod(last) 
	
		smpl 2021q4 @last
			series tfp_raw =tfp_raw(-1)*(1+(((@elem(tfp_raw,"2018q4")/@elem(tfp_raw,"2008q1"))^(1/43)-1)))
	
		smpl @all
			hpf(1600) tfp_raw tfp
			pagestruct(end=2021q3)
	
	'Potential
		series gdppotl_struct = (tfp*pop^alpha*k(-1)^(1-alpha))
	
		series og_struct  = (gdp/gdppotl_struct-1)*100
	
		delete tfp_raw k temp
		smpl @all
	
		show gdp gdppotl_struct
		show og_struct exp(og_hp)*100-100
