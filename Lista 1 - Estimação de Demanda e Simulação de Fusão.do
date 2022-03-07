********** Lista 1: Estimação de Demanda e Simulação de Fusão ***********

* Aqui está o código da Lista 1 de Organização Industrial e Regulação Econômica I

* Estimação de Demanda I

clear all

use "G:\Meu Drive\Teoria Econômica - Doutorado\5º Bimestre\Organização Industrial e Regulação Econômica I\Trabalho\cars.dta", replace

*Overview of the data 


summarize qu princ hp li wi he cy pop ma ye frm brd co cla org

*Painel

egen  yema=group(ye ma), label 
xtset co yema


*Tamanho do mercado

// Duas maneiras de considera: 1º Considerar toda população como mercado consumidor (pop) ou 2º Considerar mercado consumidor como msize = número de familias = pop/4

gen msize =pop/4

// O market share é dado por q_j = s_j. pop => s_j = q_j/M

gen sj = (qu/msize)

// Outside Option: s_0 = 1 - \sum s_j
tabstat qu, by (yema) stat (mean sum min max)

egen sum_sj = sum(qu), by(yema) // Soma das vendas do ano por País

gen s0 = 1-(sum_sj/msize) // Outside Option

// Variavel independente do Modelo: ln(sj) - ln(s0)

gen m_ls = ln(sj/s0)


// 1. Logit Agregado por MQO

xtreg m_ls princ hp li wi we he cy home i.ye i.ma, fe
estimates store Logit_Agregado

// 2. IV

// Intrumento de Hausman

sort yema brd
foreach var of varlist type yema{	
	bysort yema brd: gen teste1=price if ma=2 
	}


// Intrumento BLP

sort yema brd
foreach var of varlist hp li wi we he cy home {
	bysort yema brd: egen caract_j=total(`var')
	qui gen instr1_`var'=caract_j-`var' // Intrumento BLP para outros para bens produzidos pela mesma firma
	bysort yema: egen  total_caract=total(`var')
	qui gen instr2_`var'=total_caract-caract_j // Intrumento para outros produtos produzidos por outras firmas do mesmo mercado
	drop caract_j
	drop total_caract
	}



xtivreg m_ls hp li wi we he cy home i.ye i.ma (princ = instr1* instr2*), fe
estimates store Logit_Agregado_IV_BLP

esttab Logit_Agregado_IV_BLP using Logit_Agregado.tex, replace


// Instrumento Verboven (1996) (Média das do vetor de características dos outros produtos)

//3. Elasticidades

clear all

use "G:\Meu Drive\Teoria Econômica - Doutorado\5º Bimestre\Organização Industrial e Regulação Econômica I\Trabalho\cars.dta", replace

*Painel

egen  yema=group(ye ma), label 
xtset co yema

gen msize =pop/4


mergersim init, price(princ) quantity(qu) marketsize(msize) firm(frm)

xtreg M_ls princ cy li wi he home  i.ye i.ma, fe

mergersim init, price(princ) quantity(qu) marketsize(msize) firm(frm)

mergersim market if ye == 98 & ma==3

matrix list r(elasticities)

//

xtivreg m_ls hp li wi we he cy home i.ye i.ma (princ = instr1* instr2*), fe

sort yema brd
foreach var of varlist hp li wi we he cy home {
	bysort yema brd: egen v_x = total(`var')
	}


gen elas_preco_pr = _b[princ]*matrix_x*(1-princ)







* Estimação de Demanda II

// 1. Logit Aninhado


clear all

use "G:\Meu Drive\Teoria Econômica - Doutorado\5º Bimestre\Organização Industrial e Regulação Econômica I\Trabalho\cars.dta", replace

*Painel

egen  yema=group(ye ma), label 
xtset co yema


*Tamanho do mercado

// Duas maneiras de considera: 1º Considerar toda população como mercado consumidor (pop) ou 2º Considerar mercado consumidor como msize = número de familias = pop/4

gen msize =pop/4

//Definindo os Ninhos

mergersim init, nests(cla home) price(princ) quantity(qu) marketsize(msize) firm(frm)



xtreg M_ls princ M_lsjh M_lshg  cy li wi he home  i.ye i.ma, fe //Sem IV
estimates store Logit_Aninhado

//Com IV

sort yema brd
foreach var of varlist cy li wi he home {
	bysort yema brd: egen caract_j=total(`var')
	qui gen instr1_`var'=caract_j-`var' // Intrumento BLP para outros para bens produzidos pela mesma firma
	bysort yema: egen  total_caract=total(`var')
	qui gen instr2_`var'=total_caract-caract_j // Intrumento para outros produtos produzidos por outras firmas do mesmo mercado
	drop caract_j
	drop total_caract
	}


xtivreg M_ls cy li wi he home i.ye i.ma (princ M_lsjh M_lshg = instr1* instr2*), fe
estimates store Logit_Aninhado_IV

esttab Logit_Aninhado Logit_Aninhado_IV using Logit_Aninhado.tex, replace




//2. Elasticidades

clear all

use "G:\Meu Drive\Teoria Econômica - Doutorado\5º Bimestre\Organização Industrial e Regulação Econômica I\Trabalho\cars.dta", replace

egen  yema=group(ye ma), label 
xtset co yema


gen msize =pop/4

mergersim init, nests(cla home) price(princ) quantity(qu) marketsize(msize) firm(frm)

xtreg M_ls princ M_lsjh M_lshg cy li wi he home i.ye i.ma, fe

mergersim market if ye == 98 & ma==3

matrix list r(elasticities)


// 3. Fusão: GM (seller=15) and VW (buyer=26) in Germany 1998

clear all

use "G:\Meu Drive\Teoria Econômica - Doutorado\5º Bimestre\Organização Industrial e Regulação Econômica I\Trabalho\cars.dta", replace

egen  yema=group(ye ma), label 
xtset co yema


gen msize =pop/4

mergersim init, nests(cla home) price(princ) quantity(qu) marketsize(msize) firm(frm)

xtreg M_ls princ M_lsjh M_lshg cy li wi he home i.ye i.ma, fe

mergersim market if ye == 98

mergersim simulate if ye == 98 & ma == 3, seller(15) buyer(26) detail
	
	
//4. Fusão: GM (seller=15) and VW (buyer=26) in Germany 1998 com queda de 2% de CMg

mergersim simulate if ye == 98 & ma == 3, seller(15) buyer(26) ///
	sellereff(0.02) buyereff(0.02) method(fixedpoint) maxit(40) dampen(0.5)

mergersim mre if ye == 98 & ma == 3, seller(15) buyer(26)