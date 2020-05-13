#############################################
# PONTUACAO COLEGIADO - DOCENTE
#############################################

# criterios de pontuacao para artigos
pontos_artigo <- function(eq){ # eq: estrato qualis
  if(eq != "--"){
    if(eq == "A1") { 
      10
    } else if(eq == "A2") {
      8.5
    } else if(eq == "B1") { 
      7
    } else { 0 }
  } else { 0 }
}

# criterios de pontuacao de livros
pontos_livro <- function(eqcp = NULL, tipo){ # eq: estrato qualis

  if(tipo == "LIVRO_PUBLICADO"){
    5
  } else if(tipo == "LIVRO_ORGANIZADO_OU_EDICAO"){
    5
  } else if(tipo == "Capitulo de livro publicado"){
    2
  } else{
    0
  }
  
}

# criterios de pontuacao orientacoes
pontos_orientacao <- function(item, orientacao, nivel){

  # orientacoes concluidas
  if(item == 1){
    
    if(nivel == "dissertacao de mestrado"){
      if(orientacao == "orientador principal") {
        5
      } else if(orientacao == "co orientador"){
        2
      } else 0
    } else 0
    
    
    # orientacoes em andamento
  } else if(item == 2){
    
    if(nivel == "dissertacao de mestrado"){
      if(orientacao == "orientador principal") {
        2
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
      2
    } else if(nivel == "exame de qualificacao de mestrado"){
      2
    } else 0
    
    
    # bancas em outros programas
  } else if(item == 2){
    
    if(nivel == "mestrado"){
      2
    } else if(nivel == "exame de qualificacao de mestrado"){
      2
    } else if(nivel == "doutorado"){
      2
    } else if(nivel == "exame de qualificacao de doutorado"){
      2
    } else 0
    
    # quando for processo seletivo do MPPL
  } else if(item == 3){
    2
    
    # quando for outro tipo de banca julgadora
  } else if(item == 4){
    
    if(nivel == "professor titular"){
      0
    } else if(nivel == "concurso publico"){
      0
    } else if(nivel == "livre-docencia"){
      0
    } else if(nivel == "avalizacao de cursos"){
      0
    } else if(nivel == "outra"){
      0
    } else 0
  }
  
}


# criterios de pontuacao de projetos de pesquisa
pontos_projeto <- function(fl_responsavel, fl_financiado, tipo){
  
  # coordenador
  if(fl_responsavel == "SIM"){
    
    # financiado
    if(fl_financiado == "SIM"){
      10

      # nao financiado
    } else {
      if(tipo == "<GPDP>"){
        9
      } else if(tipo == "<GPII>"){
        8
      } else if(tipo == "<GPEC>"){
        7
      } else 0
    }

    # participante
  } else {
    
    # financiado
    if(fl_financiado == "SIM"){
      7
      
      # nao financiado
    } else {
      if(tipo == "<GPDP>"){
        6
      } else if(tipo == "<GPII>"){
        5
      } else if(tipo == "<GPEC>"){
        4
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

# criterios de pontuacao tecnica
pontos_coltec <- function(item, fl_qualificado, fl_editor){
 
  # Membro de comite assessor
  if(item == 1){
    5
    
    # Membro de corpo editorial qualificado
  } else if(item == 2){
    
    if(fl_qualificado){
      # editor
      if(fl_editor){
        5
      } else{
        1
      }
    } else 0
  }
  
}


# criterios de pontuacao tecnica
pontos_tec <- function(item, param = NULL){

  # criterios de pontuacao
  if(item == 1){
    # 1 - material didatico
    if(is.null(param)) {
      list("--", 0)
    } else if(param == "<BASICA>"){ # educacao basica
      list("T1", 8)
    } else if(param == "<SUPERIOR>") { # educacao superior
      list("T2", 8)
    } else if(param == "<APLICADA>") { # educacao profissional aplicada
      list("T3", 15)
    } else if(param == "<APLICADAE>") { # educacao profissional aplicada estrangeiro
      list("T4", 15)
    } else list("--",0)
  } else if(item == 2){
    # 2 - relatorio de pesquisa
    list("T4", 15)
  } else if(item == 3){
    # 3 - programa de radio ou TV
    l <- list("T1", 8)
    if(!is.null(param)) {
      if(param == "<PRODUCAO>") {
        l <- list("T3", 15)
      }
    }
    l
  } else if(item == 4){
    # 4 - apresentacao de trabalho
    list("T1", 8)
  } else if(item == 5){
    # 5 - curso de curta duracao ministrado
    list("T3", 15)
  } else if(item == 6){
    # 6 - organizacao de evento
    list("T1", 8)
  } else if(item == 7){
    # 7 - outra producao tecnica
    if(is.null(param)) {
      list("--", 0)
    } else if(param == "manual de operacao tecnica"){
      list("T4", 15)
    } else if(param == "revisao de traducao"){
      list("T2", 8)
    } else if(param == "base de dados publica"){
      list("T4", 15)
    } else if(param == "base de dados restrita"){
      list("T2", 8)
    } else if(param == "servico tecnico especializado"){
      list("T2", 8)
    } else if(param == "elaboracao de norma ou marco regulatorio"){
      list("T4", 15)
    } else {
      list("T2o", 8) # Outro tipo de produto/servico tecnico especializado
    }
  } else if(item == 8){
    # 8 - trabalho tecnico
    if(param[1] == "PARECER"){
      list("T2o", 8) # outro tipo de produto/servico tecnico especializado
    } else if(param[1] == "Parecer de artigo de revista"){
      # se for "parecer de artigo de revista", existe param[2]
      if(param[2] %in% c("A1","A2","B1")) { # "A1","A2","B1" em CP
          list("T2", 8)
      } else if(param[2] != "--"){ # outro estrato em CP
          list("T1", 8)
      } else list("--", 0) # n?o tem estrato em CP
    } else if(param[1] == "ELABORACAO_DE_PROJETO"){ 
      list("T4", 15)
    } else if(param[1] == "RELATORIO_TECNICO"){ 
      list("T4", 15)
    } else if(param[1] == "ASSESSORIA"){ 
      list("T3", 15)
    } else if(param[1] == "CONSULTORIA"){ 
      list("T3", 15)
    } else if(param[1] == "Laudo tecnico"){ 
      list("T3", 15)
    } else {
      list("T2o", 8) # outro tipo de produto/servico tecnico especializado
    }
  } else if(item == 9){ 
    # 9 - software
    if(param == "registro de software"){ 
      list("T4", 15)
    } else list("T2", 8) # outro tipo de produto/servico tecnico especializado
  } else if(item == 10){
    # 10 - artigo publicado em revista de divulgacao; artigo em jornal
    if(param[1] == "artigo em jornal"){
      list("T1", 8)
    } else{ # senao eh revista
      if(param[2] == "Brasil"){ 
        list("T1", 8)
      } else { 
        list("T2", 8)
      } 
    }
  } else if(item == 11){
    # 11 - prefacio/posfacio de obra tecnica
    if(is.null(param)) {
      list("--", 0)
    } else if(param == "<OBRA TECNICA>"){
      list("T2", 8)
    } else list("--", 0)
  } else if(item == 12){
    # 12 - traducao
    list("T3", 15)
  } else if(item == 13){
    # 13 - participacao em eventos e congressos
    if(param != "Ouvinte"){
      list("T1", 8)
    } else list("--", 0)
  }
}
