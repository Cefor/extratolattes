
tecnica <- function(id, xml_data,
                              pontos_tec = function(FUN, item, md_nivel) {FUN(item, md_nivel)},
                              qualis, nome_area, ano_ini, ano_fim){

  producao_tec <- NULL
 
  # DEMAIS-TIPOS-DE-PRODUCAO-TECNICA
  ptec <- xml_data$"PRODUCAO-TECNICA"$"DEMAIS-TIPOS-DE-PRODUCAO-TECNICA"

  # para todos os itens em
  #`DESENVOLVIMENTO-DE-MATERIAL-DIDATICO-OU-INSTRUCIONAL`
  for(i in (1:length(ptec))[names(ptec) == "DESENVOLVIMENTO-DE-MATERIAL-DIDATICO-OU-INSTRUCIONAL"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-MATERIAL-DIDATICO-OU-INSTRUCIONAL"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-MATERIAL-DIDATICO-OU-INSTRUCIONAL"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
    
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-MATERIAL-DIDATICO-OU-INSTRUCIONAL"["TITULO"])
      md_nivel <- ptec[[i]]$"INFORMACOES-ADICIONAIS"
      
      if(!is.null(md_nivel)){ # se diferente de NULL, recupera a primeira linha
        # md_nivel <- str_split(md_nivel[1],"\n")[[1]][1]
        nivel <- NA
        for(niv in c("BASICA", "SUPERIOR", "APLICADA", "APLICADAE")){
          nivel <- str_extract(md_nivel, niv)[1]
          if(!is.na(nivel)) break
        }
        if(!is.na(nivel)){
          md_nivel <- paste0("<",nivel,">")
        }
      }
      
      pontos <- pontos_tec(1, md_nivel)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
      }

      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = "desenvolvimento de material didatico",
        estrato = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }

  # para todos os itens em
  #`RELATORIO-DE-PESQUISA`
  for(i in (1:length(ptec))[names(ptec) == "RELATORIO-DE-PESQUISA"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-RELATORIO-DE-PESQUISA"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-RELATORIO-DE-PESQUISA"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-RELATORIO-DE-PESQUISA"["TITULO"])

      pontos <- pontos_tec(2)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
      }

      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = "relatorio conclusivo de pesquisa aplicada",
        estrato = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  
  # para todos os itens em
  #`PROGRAMA-DE-RADIO-OU-TV`
  for(i in (1:length(ptec))[names(ptec) == "PROGRAMA-DE-RADIO-OU-TV"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-PROGRAMA-DE-RADIO-OU-TV"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-PROGRAMA-DE-RADIO-OU-TV"['FLAG-RELEVANCIA']

    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PROGRAMA-DE-RADIO-OU-TV"["TITULO"])
      atuacao <- ptec[[i]]$"INFORMACOES-ADICIONAIS"["DESCRICAO-INFORMACOES-ADICIONAIS"] # <PRODUCAO> ou <PARTICIPACAO>
      tipo <- "participacao em programa de radio ou TV"
      
      if(!is.null(atuacao)){ # se diferente de NULL, recupera a primeira linha
        atuacao <- str_split(atuacao[1],"\n")[[1]][1]
        if(atuacao == "<PRODUCAO>") tipo <- "producao de programas de midia"
      }
      
      pontos <- pontos_tec(3, atuacao)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
      }

      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = tipo,
        estrato = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  # para todos os itens em
  # MIDIA-SOCIAL-WEBSITE-BLOG
  for(i in (1:length(ptec))[names(ptec) == "MIDIA-SOCIAL-WEBSITE-BLOG"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-MIDIA-SOCIAL-WEBSITE-BLOG"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-MIDIA-SOCIAL-WEBSITE-BLOG"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-MIDIA-SOCIAL-WEBSITE-BLOG"["TITULO"])
      tipo <- str_to_lower(as.vector(ptec[[i]]$"DADOS-BASICOS-DA-MIDIA-SOCIAL-WEBSITE-BLOG"["NATUREZA"]))

      pontos <- pontos_tec(15)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = tipo,
        estrato = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  
  # para todos os itens em
  #`APRESENTACAO-DE-TRABALHO`
  for(i in (1:length(ptec))[names(ptec) == "APRESENTACAO-DE-TRABALHO"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO"["TITULO"])
      
      pontos <- pontos_tec(4)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = "apresentacao de trabalho",
        estrato = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  # para todos os itens em
  #`CURSO-DE-CURTA-DURACAO-MINISTRADO`
  for(i in (1:length(ptec))[names(ptec) == "CURSO-DE-CURTA-DURACAO-MINISTRADO"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DE-CURSOS-CURTA-DURACAO-MINISTRADO"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DE-CURSOS-CURTA-DURACAO-MINISTRADO"['FLAG-RELEVANCIA']

    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DE-CURSOS-CURTA-DURACAO-MINISTRADO"["TITULO"])
      nivel <- as.vector(ptec[[i]]$"DADOS-BASICOS-DE-CURSOS-CURTA-DURACAO-MINISTRADO"["NIVEL-DO-CURSO"])
      atuacao <- as.vector(ptec[[i]]$"DETALHAMENTO-DE-CURSOS-CURTA-DURACAO-MINISTRADO"["PARTICIPACAO-DOS-AUTORES"])
      
      pontos <- pontos_tec(5)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = str_to_lower(paste("curso de curta duracao ministrado -", nivel, "-", atuacao)),
        estrato = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }  
  
  # para todos os itens em
  #`ORGANIZACAO-DE-EVENTO`
  for(i in (1:length(ptec))[names(ptec) == "ORGANIZACAO-DE-EVENTO"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-ORGANIZACAO-DE-EVENTO"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-ORGANIZACAO-DE-EVENTO"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-ORGANIZACAO-DE-EVENTO"["TITULO"])
      
      pontos <- pontos_tec(6)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = "organizacao de evento",
        estrato = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }  
  

  # para todos os itens em
  #`OUTRA-PRODUCAO-TECNICA`
  for(i in (1:length(ptec))[names(ptec) == "OUTRA-PRODUCAO-TECNICA"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA"['FLAG-RELEVANCIA']

    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA"["TITULO"])
      natureza <- str_trim(str_to_lower(as.vector(ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA"["NATUREZA"])))
      
      pontos <- pontos_tec(7, natureza)
      if(ano >= ano_ini & ano <= ano_fim){
        pontos[[2]] <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = natureza,
        estrato = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  
  ptec <- xml_data$"PRODUCAO-TECNICA"
  
  # para todos os itens em
  # TRABALHO-TECNICO
  for(i in (1:length(ptec))[names(ptec) == "TRABALHO-TECNICO"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-TRABALHO-TECNICO"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-TRABALHO-TECNICO"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-TRABALHO-TECNICO"["TITULO-DO-TRABALHO-TECNICO"])
      tipo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-TRABALHO-TECNICO"["NATUREZA"])

      # quando o PARECER for "parecer de artigo de revista"
      if(tipo == "PARECER" & as.vector(ptec[[i]]$"DETALHAMENTO-DO-TRABALHO-TECNICO"["FINALIDADE"]) == "Parecer de artigo de revista"){
          # recupera o ISSN no inicio do titulo
          issn <- gsub("-", "", str_sub(titulo, 1, 9))
          # verifica o estrato na area especificada no parametro nome_area
          eqcp <- as.character(qualis[(qualis$Area %in% nome_area ) & (qualis$ISSN == issn), "Estrato"][1])
          if(is.na(eqcp)) eqcp <- "--"
          tipo <- "Parecer de artigo de revista"
          pontos <- pontos_tec(8, c(tipo,eqcp))
          
      } else if(tipo == "OUTRA"){
        
        # verificar se eh LAUDO TECNICO
        
        info_adicional <- ptec[[i]]$"INFORMACOES-ADICIONAIS"
        
        if(!is.null(info_adicional)){ # se diferente de NULL, recupera a primeira linha
          info_adicional <- str_split(info_adicional[1],"\n")[[1]][1]
          
          if(info_adicional == "<LAUDO TECNICO>"){
            tipo <- "Laudo tecnico"
          }
        }
        
        pontos <- pontos_tec(8, tipo)
        
      } else pontos <- pontos_tec(8, tipo)

      if(!(ano >= ano_ini & ano <= ano_fim)) {
        pontos[[2]] <- 0
      }

      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = str_to_lower(tipo),
        estrato = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  

  # para todos os itens em
  # SOFTWARE
  for(i in (1:length(ptec))[names(ptec) == "SOFTWARE"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-SOFTWARE"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-SOFTWARE"['FLAG-RELEVANCIA']

    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-SOFTWARE"["TITULO-DO-SOFTWARE"])
      
      if("REGISTRO-OU-PATENTE" %in% names(ptec[[i]]$"DETALHAMENTO-DO-SOFTWARE")){
        tipo <- "registro de software"
        titulo <- paste0(titulo, " - Registro (", 
                         ptec[[i]]$"DETALHAMENTO-DO-SOFTWARE"$"REGISTRO-OU-PATENTE"["CODIGO-DO-REGISTRO-OU-PATENTE"],
                         ")")
      } else tipo <- "software"

      pontos <- pontos_tec(9, tipo)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = tipo,
        estrato = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  # para todos os itens em
  # PROCESSOS-OU-TECNICAS
  for(i in (1:length(ptec))[names(ptec) == "PROCESSOS-OU-TECNICAS"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-PROCESSOS-OU-TECNICAS"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-PROCESSOS-OU-TECNICAS"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PROCESSOS-OU-TECNICAS"["TITULO-DO-PROCESSO"])
      
      tipo <- str_to_lower(paste0("Processo ou tecnica - ",
                     ptec[[i]]$"DADOS-BASICOS-DO-PROCESSOS-OU-TECNICAS"["NATUREZA"]))
      
      pontos <- pontos_tec(14)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = tipo,
        estrato = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  
  ptec <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TEXTOS-EM-JORNAIS-OU-REVISTAS"
  # para todos os itens em
  # TEXTOS-EM-JORNAIS-OU-REVISTAS
  if(length(ptec) > 0){
    for(i in (1:length(ptec))){
      
      ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-TEXTO"["ANO-DO-TEXTO"])
      
      # flag relevancia
      fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-TEXTO"['FLAG-RELEVANCIA']
      
      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
        
        titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-TEXTO"["TITULO-DO-TEXTO"])
        pais <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-TEXTO"["PAIS-DE-PUBLICACAO"])
        natureza <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-TEXTO"["NATUREZA"])
          
        if(natureza == "REVISTA_MAGAZINE"){
          tipo <- "artigo publicado em revista de divulgacao" 
        } else if(natureza == "JORNAL_DE_NOTICIAS"){
          tipo <- "artigo em jornal"
        } else tipo <- natureza

        pontos <- pontos_tec(10, c(tipo, pais))
        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos[[2]] <- 0
        }
        
        ap <- data.frame(
          id = id,
          ano = ano,
          titulo = titulo,
          tipo = tipo,
          estrato = pontos[[1]],
          pontos = pontos[[2]],
          flag_relevancia = fl_rlvnc
        )
        
        producao_tec <- rbind(producao_tec, ap, row.names = NULL)
      }
    }
  }
  

  ptec <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"DEMAIS-TIPOS-DE-PRODUCAO-BIBLIOGRAFICA"
  # para todos os itens em
  # PREFACIO-POSFACIO
  if(length(ptec) > 0){
    for(i in (1:length(ptec))[names(ptec) == "PREFACIO-POSFACIO"]){
      
      ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"["ANO"])
      
      # flag relevancia
      fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"['FLAG-RELEVANCIA']
      
      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
        
        titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"["TITULO"])
        tipo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"["TIPO"])
        natureza <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"["NATUREZA"])
        
        fl_tecnica <- as.vector(ptec[[i]]$"INFORMACOES-ADICIONAIS"["DESCRICAO-INFORMACOES-ADICIONAIS"])
        if(!is.null(fl_tecnica)){ # se diferente de NULL, recupera a primeira linha
          fl_tecnica <- str_split(fl_tecnica,"\n")[[1]][1]
          if(fl_tecnica == "<OBRA TECNICA>"){
            tipo <- str_to_lower(paste(tipo, "de", natureza, "-", gsub(">", "", gsub("<", "", fl_tecnica))))  
          }
        } else tipo <- str_to_lower(paste(tipo, "de", natureza))
          
        pontos <- pontos_tec(11, fl_tecnica)
        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos[[2]] <- 0
        }
          
        ap <- data.frame(
          id = id,
          ano = ano,
          titulo = titulo,
          tipo = tipo,
          estrato = pontos[[1]],
          pontos = pontos[[2]],
          flag_relevancia = fl_rlvnc
        )
        
        producao_tec <- rbind(producao_tec, ap, row.names = NULL)
      }
    }
  }
  
  # para todos os itens em
  # TRADUCAO
  if(length(ptec) > 0){
    for(i in (1:length(ptec))[names(ptec) == "TRADUCAO"]){
      
      ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-TRADUCAO"["ANO"])
      
      # flag relevancia
      fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-TRADUCAO"['FLAG-RELEVANCIA']

      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
        
        titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-TRADUCAO"["TITULO"])
        tipo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-TRADUCAO"["NATUREZA"])
        
        pontos <- pontos_tec(12)
        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos[[2]] <- 0
        }
        
        ap <- data.frame(
          id = id,
          ano = ano,
          titulo = titulo,
          tipo = paste("traducao de ", str_to_lower(tipo)),
          estrato = pontos[[1]],
          pontos = pontos[[2]],
          flag_relevancia = fl_rlvnc
        )
        
        producao_tec <- rbind(producao_tec, ap, row.names = NULL)
      }
    }
  }
  
  
  ptec <- xml_data$"DADOS-COMPLEMENTARES"$"PARTICIPACAO-EM-EVENTOS-CONGRESSOS"
  # para todos os itens em
  # PARTICIPACAO-EM-EVENTOS-CONGRESSOS
  # Participacao em mesa redonda; Palestrante, conferencista
  if(length(ptec) > 0){
    for(i in (1:length(ptec))){
      
      ano <- as.numeric(ptec[[i]][[1]]["ANO"])
      
      # flag relevancia
      fl_rlvnc <- ptec[[i]][[1]]['FLAG-RELEVANCIA']

      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
        
        titulo <- as.vector(ptec[[i]][[1]]["TITULO"])
        natureza <- as.vector(ptec[[i]][[1]]["NATUREZA"])
        
        tipo_participacao <- ptec[[i]][[1]]["TIPO-PARTICIPACAO"]
        forma_participacao <- ptec[[i]][[1]]["FORMA-PARTICIPACAO"]
        
        pontos <- pontos_tec(13, forma_participacao)
        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos[[2]] <- 0
        }

        ap <- data.frame(
          id = id,
          ano = ano,
          titulo = titulo,
          tipo = str_to_lower(paste(natureza, "-", forma_participacao, "-", tipo_participacao)),
          estrato = pontos[[1]],
          pontos = pontos[[2]],
          flag_relevancia = fl_rlvnc
        )
        
        producao_tec <- rbind(producao_tec, ap, row.names = NULL)
      }
    }
  }
  
  

  
  
  
  #####################################
  # insere linha de totais
  ap <- data.frame(
    id = id,
    ano = "",
    titulo = "",
    tipo = "",
    estrato = "TOTAL",
    pontos = sum(producao_tec$pontos, na.rm = TRUE),
    flag_relevancia = ""
  )
  producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  
  # retorna o dataframe
  producao_tec
}