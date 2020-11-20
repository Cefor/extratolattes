#############################################
# PONTUACAO CAPES - DOCENTE
#############################################

# criterios de pontuacao para artigos
pontos_artigo <- function(eq){ # eq: estrato qualis
  
  # valor <- c(100, 85, 70, 55, 25, 20, 10, 5, 0, 0, 0, 0) # novo qualis
  valor <- c(100, 85,  0,  0, 70,  0,  0, 0, 0, 0, 0, 0) # qualis 2013-2016
  names(valor) <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", "B5", "C", "--", "NP")
  
  valor[eq]
  
#  if(eq != "--"){
#    if(eq == "A1") { 
#      100
#    } else if(eq == "A2") {
#      85
#    } else if(eq == "B1") { 
#      70
#    } else { 0 }
#  } else { 0 }
  
  
}

# criterios de pontuacao de livros
pontos_livro <- function(eqcp = NULL, tipo){ # eq: estrato qualis

  # pontuacao CAPES
  if(tipo == "LIVRO_PUBLICADO"){
    
    valor <- c(2, 1.5, 0, 0, 0)
    names(valor) <- c("L4", "L3", "L2", "L1", "--")
    as.numeric(valor[eqcp])

#    if(eqcp == "L4"){
#      2
#    } else if(eqcp == "L3"){
#      1.5
#    } else 0
    
    
  } else if(tipo == "LIVRO_ORGANIZADO_OU_EDICAO"){

    valor <- c(1, 0.75, 0, 0, 0)
    names(valor) <- c("L4", "L3", "L2", "L1", "--")
    as.numeric(valor[eqcp])

#    if(eqcp == "L4"){
#      1
#    } else if(eqcp == "L3"){
#      0.75
#    } else 0
    
  } else if(tipo == "Capitulo de livro publicado") {

    valor <- c(0.5, 0.35, 0, 0, 0)
    names(valor) <- c("L4", "L3", "L2", "L1", "--")
    as.numeric(valor[eqcp])
     
#    if(eqcp == "L4"){
#      0.5
#    } else if(eqcp == "L3"){
#      0.35
#    } else 0
  }  
}

# criterios de pontuacao orientacoes
pontos_orientacao <- function(item, orientacao, nivel){

  # orientacoes concluidas
  if(item == 1){
    
    if(nivel == "dissertacao de mestrado"){
      if(orientacao == "orientador principal") {
        1
      } else if(orientacao == "co orientador"){
        1
      } else 0
    } else 0
    
    
    # orientacoes em andamento
  } else if(item == 2){
    
    if(nivel == "dissertacao de mestrado"){
      if(orientacao == "orientador principal") {
        1
      } else if(orientacao == "co orientador"){
        1
      } else 0
    } else 0
    
  }    

}


# criterios de pontuacao de bancas
pontos_banca <- function(item, nivel = NULL){
  
  # bancas no Programa de Mestrado
  if(item == 1){
    
    if(nivel == "mestrado"){
      1
    } else if(nivel == "exame de qualificacao de mestrado"){
      1
    } else if(nivel == "doutorado"){
      1
    } else if(nivel == "exame de qualificacao de doutorado"){
      1
    } else 0
    
    
    # bancas em outros programas
  } else if(item == 2){
    
    if(nivel == "mestrado"){
      1
    } else if(nivel == "exame de qualificacao de mestrado"){
      1
    } else if(nivel == "doutorado"){
      1
    } else if(nivel == "exame de qualificacao de doutorado"){
      1
    } else 0
    
    # quando for processo seletivo do MPPL
  } else if(item == 3){
    0
    
    # quando for outro tipo de banca julgadora
  } else if(item == 4){
    
    if(nivel == "professor titular"){
      1
    } else if(nivel == "concurso publico"){
      1
    } else if(nivel == "livre-docencia"){
      1
    } else if(nivel == "avalizacao de cursos"){
      1
    } else if(nivel == "outra"){
      1
    } else 0
  }
  
}


# criterios de pontuacao de projetos de pesquisa
pontos_projeto <- function(fl_responsavel, fl_financiado, tipo){
  
  # coordenador
  if(fl_responsavel == "SIM"){
    
    # financiado
    if(fl_financiado == "SIM"){
      1

      # nao financiado
    } else {
      if(tipo == "<GPDP>"){
        0
      } else if(tipo == "<GPII>"){
        0
      } else if(tipo == "<GPEC>"){
        0
      } else 0
    }

    # participante
  } else {
    
    # financiado
    if(fl_financiado == "SIM"){
      1
      
      # nao financiado
    } else {
      if(tipo == "<GPDP>"){
        0
      } else if(tipo == "<GPII>"){
        0
      } else if(tipo == "<GPEC>"){
        0
      } else 0
    }
  }
  
}


# criterios de pontuacao de disciplinas
pontos_disciplina <- function(fl_programa){
  if(fl_programa){
    1
  } else 0
  
}

# criterios de pontuacao colaboracao tecnica
pontos_coltec <- function(item, fl_qualificado, fl_editor){
 
  # Membro de comite assessor
  if(item == 1){
    1
    
    # Membro de corpo editorial qualificado
  } else if(item == 2){
    
    if(fl_qualificado){
      # editor
      if(fl_editor){
        1
      } else{
        1
      }
    } else 0
  }
  
}


# criterios de pontuacao tecnica
pontos_tec <- function(item, param = NULL){

  valor <- c(2, 1.5, 1, 1, 0.5, 0)
  names(valor) <- c("T4", "T3", "T2", "T2o", "T1", "--")
  
  # se item 0, retorna a pontuacao dos estratos em param
  if(item == 0){
    return(as.numeric(valor[param]))
  }
  
  # criterios de pontuacao
  if(item == 1){
    # 1 - material didatico
    if(is.null(param)) {
      list("--", as.numeric(valor["--"]))
    } else if(param == "<BASICA>"){ # educacao basica
      list("T1", as.numeric(valor["T1"]))
    } else if(param == "<SUPERIOR>") { # educacao superior
      list("T2", as.numeric(valor["T2"]))
    } else if(param == "<APLICADA>") { # educacao profissional aplicada
      list("T3", as.numeric(valor["T3"]))
    } else if(param == "<APLICADAE>") { # educacao profissional aplicada estrangeiro
      list("T4", as.numeric(valor["T4"]))
    } else list("--", as.numeric(valor["--"]))
  } else if(item == 2){
    # 2 - relatorio de pesquisa
    list("T4", as.numeric(valor["T4"]))
  } else if(item == 3){
    # 3 - programa de radio ou TV
    l <- list("T1", as.numeric(valor["T1"]))
    if(!is.null(param)) {
      if(param == "<PRODUCAO>") {
        l <- list("T3", as.numeric(valor["T3"]))
      }
    }
    l
  } else if(item == 4){
    # 4 - apresentacao de trabalho
    list("T1", as.numeric(valor["T1"]))
  } else if(item == 5){
    # 5 - curso de curta duracao ministrado
    list("T3", as.numeric(valor["T3"]))
  } else if(item == 6){
    # 6 - organizacao de evento
    list("T1", as.numeric(valor["T1"]))
  } else if(item == 7){
    # 7 - outra producao tecnica
    if(is.null(param)) {
      list("--", as.numeric(valor["--"]))
    } else if(param == "manual de operacao tecnica"){
      list("T4", as.numeric(valor["T4"]))
    } else if(param == "revisao de traducao"){
      list("T2", as.numeric(valor["T2"]))
    } else if(param == "base de dados publica"){
      list("T4", as.numeric(valor["T4"]))
    } else if(param == "base de dados restrita"){
      list("T2", as.numeric(valor["T2"]))
    } else if(param == "servico tecnico especializado"){
      list("T2", as.numeric(valor["T2"]))
    } else if(param == "elaboracao de norma ou marco regulatorio"){
      list("T4", as.numeric(valor["T4"]))
    } else {
      list("T2o", as.numeric(valor["T2o"])) # Outro tipo de produto
    }
  } else if(item == 8){
    # 8 - trabalho tecnico
    if(param[1] == "PARECER"){
      list("T2o", as.numeric(valor["T2o"])) # outro tipo de produto
    } else if(param[1] == "Parecer de artigo de revista"){
      # se for "parecer de artigo de revista", existe param[2]
      if(param[2] %in% c("A1","A2","B1")) { # "A1","A2","B1" em CP
          list("T2", as.numeric(valor["T2"]))
      } else if(param[2] != "--"){ # outro estrato em CP
          list("T1", as.numeric(valor["T1"]))
      } else list("--", as.numeric(valor["--"])) # nao tem estrato em CP
    } else if(param[1] == "ELABORACAO_DE_PROJETO"){ 
      list("T4", as.numeric(valor["T4"]))
    } else if(param[1] == "RELATORIO_TECNICO"){ 
      list("T4", as.numeric(valor["T4"]))
    } else if(param[1] == "ASSESSORIA"){ 
      list("T3", as.numeric(valor["T3"]))
    } else if(param[1] == "CONSULTORIA"){ 
      list("T3", as.numeric(valor["T3"]))
    } else if(param[1] == "Laudo tecnico"){ 
      list("T3", as.numeric(valor["T3"]))
    } else {
      list("T2o", as.numeric(valor["T2o"])) # outro tipo de produto
    }
  } else if(item == 9){ 
    # 9 - software
    if(param == "registro de software"){ 
      list("T4", as.numeric(valor["T4"]))
    } else list("T2", as.numeric(valor["T2"])) # outro tipo de produto
  } else if(item == 10){
    # 10 - artigo publicado em revista de divulgacao; artigo em jornal
    if(param[1] == "artigo em jornal"){
      list("T1", as.numeric(valor["T1"]))
    } else{ # senao eh revista
      if(param[2] == "Brasil"){ 
        list("T1", as.numeric(valor["T1"]))
      } else { 
        list("T2", as.numeric(valor["T2"]))
      } 
    }
  } else if(item == 11){
    # 11 - prefacio/posfacio de obra tecnica
    if(is.null(param)) {
      list("--", as.numeric(valor["--"]))
    } else if(param == "<OBRA TECNICA>"){
      list("T2", as.numeric(valor["T2"]))
    } else list("--", as.numeric(valor["--"]))
  } else if(item == 12){
    # 12 - traducao
    list("T3", as.numeric(valor["T3"]))
  } else if(item == 13){
    # 13 - participacao em eventos e congressos
    if(param != "Ouvinte"){
      list("T1", as.numeric(valor["T1"]))
    } else list("--", as.numeric(valor["--"]))
  } else if(item == 14){
    # 14 - processos ou tecnicas
    list("T4", as.numeric(valor["T4"]))
  } else if(item == 15){
    # 15 - redes sociais, websites e blogs
    list("T2o", as.numeric(valor["T2o"]))
  }
}
