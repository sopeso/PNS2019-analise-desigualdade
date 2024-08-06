 // Renomear variáveis para nomes mais significativos
rename (c006 c008 c009 v0001 vdd004a) (sexo idade raca unidade instrucao)

// Definir rótulos de valores para a variável 'sexo'
label define gen 1 "Homem" 2 "Mulher"
label values sexo gen

// Definir rótulos de valores para a variável 'raca'
label define color 1 "Branca" 2 "Preta" 4 "Parda"
label values raca color

// Definir rótulos de valores para a variável 'unidade', que representa estados brasileiros
label define federacao 11 "Rondônia" 12 "Acre" 13 "Amazonas" 14 "Roraima" 15 "Pará" 16 "Amapá" 17 "Tocantins" 21 "Maranhão" 22 "Piauí" 23 "Ceará" 24 "Rio Grande do Norte" 25 "Paraíba" 26 "Pernambuco" 27 "Alagoas" 28 "Sergipe" 29 "Bahia" 31 "Minas Gerais" 32 "Espírito Santo" 33 "Rio de Janeiro" 35 "São Paulo" 41 "Paraná" 42 "Santa Catarina" 43 "Rio Grande do Sul" 50 "Mato Grosso do Sul" 51 "Mato Grosso" 52 "Goiás" 53 "Distrito Federal"
label values unidade federacao

// Agrupar níveis de instrução em três categorias
replace instrucao = 1 if (instrucao == 1 | instrucao == 2)
replace instrucao = 2 if (instrucao == 3 | instrucao == 4)
replace instrucao = 3 if (instrucao == 5 | instrucao == 6 | instrucao == 7)

// Definir rótulos de valores para a variável 'instrucao'
label define escolaridade 1 "Fundamental incompleto" 2 "Médio incompleto" 3 "Médio completo ou +"
label values instrucao escolaridade

// Criar variável para identificar a subamostra analisada (60+ anos, brancos, pretos ou pardos)
gen used = 1 if idade >= 60 & (raca == 1 | raca == 2 | raca == 4)
replace used = 0 if used == .

// Mostrar distribuição da variável 'used'
tab used

// Definir o desenho amostral usando pesos e estratificação
svyset upa_pns [iweight=v00291], strata(v0024) vce(linearized) singleunit(missing)

// Criar rótulos para variáveis demográficas
label variable unidade "UF"
label variable sexo "Sexo"
label variable raca "Cor ou raça"
label variable instrucao "Nível de instrução"
label variable idade "Idade do morador (anos)"

// Criar variável 'saude' para autoavaliação de saúde (bom/regular/ruim)
gen saude = 1 if n001 == 1
replace saude = 1 if n001 == 2
replace saude = 0 if n001 == 3
replace saude = 0 if n001 == 4
replace saude = 0 if n001 == 5

// Definir rótulos para a variável 'saude'
label var saude "Auto-avaliação de saúde"
label define saude 1 "Muito bom e bom " 0 "Regular, ruim e muito ruim"
label val saude saude

// Criar variáveis indicadoras (0 ou 1) para doenças crônicas
// Hipertensão
gen hipertensao = 1 if q00201 == 1
replace hipertensao = 0 if q00201 == 2
replace hipertensao = 0 if q00202 == 1
replace hipertensao = 1 if q00202 == 2
label var hipertensao "Hipertensão"
label define hipertensao 0 "Não" 1 "Sim"
label val hipertensao hipertensao

// Diabetes
gen diabetes = 1 if q03001 == 1
replace diabetes = 0 if q03001 == 2
replace diabetes = 0 if q03002 == 1
replace diabetes = 1 if q03002 == 2
label var diabetes "Diabetes"
label define diabetes 0 "Não" 1 "Sim"
label val diabetes diabetes

// Doenças do coração
gen coracao = 1 if q06306 == 1
replace coracao = 0 if q06306 == 2
label var coracao "Doença do coração"
label define coracao 0 "Não" 1 "Sim"
label val coracao coracao

// AVC
gen avc = 1 if q068 == 1
replace avc = 0 if q068 == 2
label var avc "AVC ou derrame"
label define avc 0 "Não" 1 "Sim"
label val avc avc

// Asma
gen asma = 1 if q074 == 1
replace asma = 0 if q074 == 2
label var asma "Asma"
label define asma 0 "Não" 1 "Sim"
label val asma asma

// Artrite
gen artrite = 1 if q079 == 1
replace artrite = 0 if q079 == 2
label var artrite "Artrite ou Reumatismo"
label define artrite 0 "Não" 1 "Sim"
label val artrite artrite

// Doenças da coluna
gen coluna = 1 if q084 == 1
replace coluna = 0 if q084 == 2
label var coluna "Doenças na coluna"
label define coluna 0 "Não" 1 "Sim"
label val coluna coluna

// Depressão
gen depressao = 1 if q092 == 1
replace depressao = 0 if q092 == 2
label var depressao "Depressão"
label define depressao 0 "Não" 1 "Sim"
label val depressao depressao

// Doença crônica no pulmão
gen pulmao = 1 if q11604 == 1
replace pulmao = 0 if q11604 == 2
label var pulmao "Doença crônica no pulmão"
label define pulmao 0 "Não" 1 "Sim"
label val pulmao pulmao

// Câncer
gen cancer = 1 if q120 == 1
replace cancer = 0 if q120 == 2
label var cancer "Câncer"
label define cancer 0 "Não" 1 "Sim"
label val cancer cancer

// Calcular número total de doenças crônicas
gen aux = hipertensao + diabetes + coracao + avc + asma + artrite + coluna + depressao + pulmao + cancer

// Calcular total de doenças crônicas usando outra abordagem para casos ausentes
egen float aux2 = rowtotal(hipertensao diabetes coracao avc asma artrite coluna depressao pulmao cancer) if aux == .

// Criar variável de multimorbidade
gen mult = aux
replace mult = aux2 if mult == . & aux2 >= 2

// Recode multimorbidade: 0 = Não, 1 = Sim
recode mult (0 = 1) (1=1) (2/10 = 0)
label var mult "Multimorbidade"
label define mult 1 "Não" 0 "Sim"
label val mult mult

// Tabelas de frequências e médias ponderadas
svy: tab unidade saude, row
svy: tab unidade mult, row

// Instalar pacotes para exportação de resultados
ssc install outreg2
ssc install estout
ssc install logout

// Exportar tabelas de frequências para Excel
logout, save(tab_sexo_unidade) noauto excel: svy: tab unidade sexo if used == 1,row 
logout, save(tab_raca_unidade) noauto excel: svy: tab unidade raca if used == 1,row
logout, save(tab_media_idade_unidade) noauto excel: svy linearized, subpop(if used == 1) : mean idade, over(unidade)
logout, save(tab_instrucao_unidade) noauto excel: svy: tab unidade instrucao if used == 1,row
logout, save(tab_mult_unidade) noauto excel: svy: tab unidade mult, row
logout, save(tab_saude_unidade) noauto excel: svy: tab unidade saude, row

// Instalar pacote para cálculo de índice de concentração
net install cixr, from("https://www.equidade.org/files")

// Calcular o índice de concentração por UF para escolaridade e autoavaliação de saúde
// O índice de concentração é calculado usando o pacote cixr

// Primeiro, criamos a variável de peso arredondado para o cálculo
gen peso = round(v00291)

// Calcular o índice de concentração para cada UF, usando 'cixr'
// A variável de desfecho é 'saude' (autoavaliação de saúde com 5 categorias)
// A variável de estratificação é 'instrucao' (nível de escolaridade)
// Os pesos são aplicados com a opção [w = peso]
// A opção 'cluster(upa_pns)' é usada para levar em conta o desenho amostral

// Rondônia
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 11

// Acre
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 12

// Amazonas
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 13

// Roraima
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 14

// Pará
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 15

// Amapá
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 16

// Tocantins
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 17

// Maranhão
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 21

// Piauí
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 22

// Ceará
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 23

// Rio Grande do Norte
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 24

// Paraíba
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 25

// Pernambuco
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 26

// Alagoas
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 27

// Sergipe
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 28

// Bahia
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 29

// Minas Gerais
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 31

// Espírito Santo
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 32

// Rio de Janeiro
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 33

// São Paulo
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 35

// Paraná
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 41

// Santa Catarina
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 42

// Rio Grande do Sul
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 43

// Mato Grosso do Sul
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 50

// Mato Grosso
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 51

// Goiás
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 52

// Distrito Federal
cixr instrucao saude [w = peso], cluster(upa_pns), if unidade == 53

// Cálculo do índice de concentração para o total do Brasil
cixr instrucao saude [w = peso], cluster(upa_pns)

// Calcular o índice de concentração por UF para escolaridade e multimorbidade
// A variável de desfecho agora é 'mult' (número de doenças crônicas)

// Rondônia
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 11

// Acre
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 12

// Amazonas
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 13

// Roraima
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 14

// Pará
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 15

// Amapá
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 16

// Tocantins
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 17

// Maranhão
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 21

// Piauí
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 22

// Ceará
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 23

// Rio Grande do Norte
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 24

// Paraíba
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 25

// Pernambuco
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 26

// Alagoas
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 27

// Sergipe
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 28

// Bahia
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 29

// Minas Gerais
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 31

// Espírito Santo
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 32

// Rio de Janeiro
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 33

// São Paulo
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 35

// Paraná
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 41

// Santa Catarina
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 42

// Rio Grande do Sul
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 43

// Mato Grosso do Sul
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 50

// Mato Grosso
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 51

// Goiás
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 52

// Distrito Federal
cixr instrucao mult [w = peso], cluster(upa_pns), if unidade == 53

// Cálculo do índice de concentração para o total do Brasil
cixr instrucao mult [w = peso], cluster(upa_pns)

