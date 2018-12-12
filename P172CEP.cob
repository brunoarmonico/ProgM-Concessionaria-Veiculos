      *AUTOR: BRUNO CHAGAS DA SILVA ARMONICO
      *TURMA: ADS 4° SEMESTRE - TARDE - 2017
      *
      *RODAR NO OPEN COBOL IDE, COMPILADOR DEFAULT OU MF
      *ATIVAR "RUN IN EXTERNAL TERMINAL" NA GUIA "RUN" NAS
      *PREFERENCIAS (F2) DO OPEN COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P172CEP.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCEP ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CEP
               FILE STATUS  IS ERRO
               ALTERNATE RECORD KEY IS LOGRADOURO WITH DUPLICATES.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.

           FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
           01 REGCEP.
               02 CEP PIC 9(8).
               02 LOGRADOURO PIC X(35) VALUE SPACES.
               02 BAIRRO PIC X(20) VALUE SPACES.
               02 CIDADE PIC X(20) VALUE SPACES.
               02 UF PIC X(2) VALUE SPACES.
               02 REFERENCIA PIC X(35) VALUE SPACES.
               02 LATITUDE PIC X(15) VALUE SPACES.
               02 LONGITUDE PIC X(15) VALUE SPACES.

       WORKING-STORAGE SECTION.

           01 TABUF PIC X(54)
          VALUE"ACALAPAMBACEDFESGOMAMTMSMGPAPBPRPEPIRJRNRSRORRSCSPSETO".
           01 TAUF REDEFINES TABUF.
              03 TUFP PIC X(2) OCCURS 27 TIMES.
           01 TABUFM PIC X(54)
          VALUE"acalapambacedfesgomamtmsmgpapbprpepirjrnrsrorrscspseto".
           01 TAUFM REDEFINES TABUFM.
              02 TUFPM PIC X(2) OCCURS 27 TIMES.
           01 ESTADOS.
               02 ES01 PIC X(20) VALUE "ACRE".
               02 ES02 PIC X(20) VALUE "ALAGOAS".
               02 ES03 PIC X(20) VALUE "AMAPA".
               02 ES04 PIC X(20) VALUE "AMAZONAS".
               02 ES05 PIC X(20) VALUE "BAHIA".
               02 ES06 PIC X(20) VALUE "CEARA".
               02 ES07 PIC X(20) VALUE "DISTRITO FEDERAL".
               02 ES08 PIC X(20) VALUE "ESPIRITO SANTO".
               02 ES09 PIC X(20) VALUE "GOIAS".
               02 ES10 PIC X(20) VALUE "MARANHAO".
               02 ES11 PIC X(20) VALUE "MATO GROSSO".
               02 ES12 PIC X(20) VALUE "MATO GROSSO DO SUL".
               02 ES13 PIC X(20) VALUE "MINAS GERAIS".
               02 ES14 PIC X(20) VALUE "PARA".
               02 ES15 PIC X(20) VALUE "PARAIBA".
               02 ES16 PIC X(20) VALUE "PARANA".
               02 ES17 PIC X(20) VALUE "PERNAMBUCO".
               02 ES18 PIC X(20) VALUE "PIAUI".
               02 ES19 PIC X(20) VALUE "RIO DE JANEIRO".
               02 ES20 PIC X(20) VALUE "RIO GRANDE DO NORTE".
               02 ES21 PIC X(20) VALUE "RIO GRANDE DO SUL".
               02 ES22 PIC X(20) VALUE "RONDONIA".
               02 ES23 PIC X(20) VALUE "RORAIMA".
               02 ES24 PIC X(20) VALUE "SANTA CATARINA".
               02 ES25 PIC X(20) VALUE "SAO PAULO".
               02 ES26 PIC X(20) VALUE "SERGIPE".
               02 ES27 PIC X(20) VALUE "TOCANTINS".
           01 TABESTADOS REDEFINES ESTADOS.
              03 BUSCAESTADOS PIC X(20) OCCURS 27 TIMES.
           01 IND PIC 9(2) VALUE 1.
           01 ERRO PIC X(2) VALUE "00".
           01 ACTKEY PIC 9(02) VALUE ZEROES.
           01 OPC PIC 9(1).
           01 AUX PIC X(01) VALUE SPACES.
           01 EXISTENCIA PIC 9(1) VALUE 0.


       SCREEN SECTION.

       01  TELACEP.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 01  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»".
           05  LINE 02  COLUMN 01
               VALUE  "º                               CADASTRO".
           05  LINE 02  COLUMN 41
               VALUE  " DE CEP                                º".
           05  LINE 03  COLUMN 01
               VALUE  "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 03  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹".
           05  LINE 04  COLUMN 01
               VALUE  "º  CEP:".
           05  LINE 04  COLUMN 41
               VALUE  "                                       º".
           05  LINE 05  COLUMN 01
               VALUE  "ºÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 05  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄº".
           05  LINE 06  COLUMN 01
               VALUE  "º  LOGRADOURO:".
           05  LINE 06  COLUMN 41
               VALUE  "           UF:                         º".
           05  LINE 07  COLUMN 01
               VALUE  "ºÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 07  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄº".
           05  LINE 08  COLUMN 01
               VALUE  "º  CIDADE:                      BAIRRO:".
           05  LINE 08  COLUMN 41
               VALUE  "                                       º".
           05  LINE 09  COLUMN 01
               VALUE  "ºÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 09  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄº".
           05  LINE 10  COLUMN 01
               VALUE  "º  REFERENCIA:".
           05  LINE 10  COLUMN 41
               VALUE  "                                       º".
           05  LINE 11  COLUMN 01
               VALUE  "ºÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 11  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄº".
           05  LINE 12  COLUMN 01
               VALUE  "º  LATITUDE:".
           05  LINE 12  COLUMN 41
               VALUE  " LONGITUDE:                            º".
           05  LINE 13  COLUMN 01
               VALUE  "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 13  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹".
           05  LINE 14  COLUMN 01
               VALUE  "º".
           05  LINE 14  COLUMN 41
               VALUE  "                                       º".
           05  LINE 15  COLUMN 01
               VALUE  "º".
           05  LINE 15  COLUMN 41
               VALUE  "                                       º".
           05  LINE 16  COLUMN 01
               VALUE  "º".
           05  LINE 16  COLUMN 41
               VALUE  "                                       º".
           05  LINE 17  COLUMN 01
               VALUE  "º".
           05  LINE 17  COLUMN 41
               VALUE  "                                       º".
           05  LINE 18  COLUMN 01
               VALUE  "º".
           05  LINE 18  COLUMN 41
               VALUE  "                                       º".
           05  LINE 19  COLUMN 01
               VALUE  "º".
           05  LINE 19  COLUMN 41
               VALUE  "                                       º".
           05  LINE 20  COLUMN 01
               VALUE  "º".
           05  LINE 20  COLUMN 41
               VALUE  "                                       º".
           05  LINE 21  COLUMN 01
               VALUE  "º".
           05  LINE 21  COLUMN 41
               VALUE  "                                       º".
           05  LINE 22  COLUMN 01
               VALUE  "º".
           05  LINE 22  COLUMN 41
               VALUE  "                                       º".
           05  LINE 23  COLUMN 01
               VALUE  "º".
           05  LINE 23  COLUMN 41
               VALUE  "                                       º".
           05  LINE 24  COLUMN 01
               VALUE  "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 24  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼".
           05  TCEP
               LINE 04  COLUMN 08  PIC 99999.999
               USING  CEP
               HIGHLIGHT BLANK ZERO.
           05  TLOGRADOURO
               LINE 06  COLUMN 15  PIC X(35)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TUF
               LINE 06  COLUMN 55  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TCIDADE
               LINE 08  COLUMN 11  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TBAIRRO
               LINE 08  COLUMN 40  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TREFERENCIA
               LINE 10  COLUMN 15  PIC X(35)
               USING  REFERENCIA
               HIGHLIGHT.
           05  TLATI
               LINE 12  COLUMN 13  PIC X(15)
               USING  LATITUDE
               HIGHLIGHT.
           05  TLONG
               LINE 12  COLUMN 52  PIC X(15)
               USING  LONGITUDE
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

       ARQUIVO.
      *ABRE O ARQUIVO DE DADOS DE CEP
           OPEN I-O CADCEP
           IF ERRO NOT = "00"
               IF ERRO = "30" OR ERRO = "35"
                   OPEN OUTPUT CADCEP
                   CLOSE CADCEP
                  DISPLAY "ARQUIVO CADCEP FOI CRIADO" AT 0622
                   GO TO MENU
               ELSE
                  DISPLAY "ERRO NA ABERTURA DO ARQUIVO CADCEP" AT 0622
           ELSE
               DISPLAY TELACEP
               CONTINUE.

      *INICIALIZA TELA DE MENU INICIAL
       INICIALIZA.
           DISPLAY TELACEP
           CONTINUE.

      *OPCOES DO MENU INICIAL
       MENU.
           PERFORM LIMPAVAR
           DISPLAY "F1 CADASTRAR, F2 PROCURAR, F3 SAIR" AT 1505
           ACCEPT AUX AT 2360
           ACCEPT ACTKEY FROM ESCAPE KEY
               IF ACTKEY = 01
                   PERFORM LIMPATELA
                   DISPLAY TELACEP
                   GO TO R-CEP
               ELSE IF ACTKEY = 02
                   PERFORM LIMPATELA
                   DISPLAY "CEP: " AT 1605
                   ACCEPT CEP AT 1610
                   ACCEPT ACTKEY FROM ESCAPE KEY
                   IF ACTKEY = 01
                       PERFORM LIMPATELA
                       PERFORM LIMPAVAR
                       DISPLAY TELACEP
                       GO TO MENU
                   END-IF
                   PERFORM PROCURA
               ELSE IF ACTKEY = 03
                   GO TO SAIR
               ELSE
                   DISPLAY "OPCAO INVALIDA" AT 1705
                   GO TO MENU.

      *RECEBE NUMERO DE CEP
       R-CEP.
           ACCEPT TCEP
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               PERFORM LIMPAVAR
               DISPLAY TELACEP
               GO TO MENU
           ELSE IF CEP = ZEROES
               PERFORM LIMPATELA
               DISPLAY "CEP INVALIDO" AT 1505
               GO TO R-CEP
           ELSE
               PERFORM LIMPATELA
               PERFORM LERCEP
               GO TO R-LOGR
           END-IF.

      *RECEBE NOME DO LOGRADOURO
       R-LOGR.
           ACCEPT TLOGRADOURO
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               GO TO R-CEP
           ELSE IF LOGRADOURO = SPACES
               PERFORM LIMPATELA
               DISPLAY "LOGRADOURO INVALIDO" AT 1505
               GO TO R-LOGR
           ELSE
               PERFORM LIMPATELA
               GO TO R-UF
           END-IF.

      *RECEBE UF DO ESTADO
       R-UF.
           ACCEPT TUF
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               DISPLAY "                    " AT 0658
               GO TO R-LOGR
           ELSE IF UF = SPACES
               PERFORM LIMPATELA
               DISPLAY "UF INVALIDO" AT 1505
               GO TO R-UF
           ELSE
               PERFORM LIMPATELA
               MOVE 1 TO IND
               GO TO VALIDA-UF
           END-IF.

      *VALIDA UF COM SIGLAS DA VARIAVEL TUFPM
       VALIDA-UF.
           IF IND > 27
               DISPLAY "UF NAO ENCONTRADO" AT 1505
               MOVE 1 TO IND
               GO TO R-UF
           ELSE
               IF UF = TUFP(IND) OR UF = TUFPM(IND)
                   DISPLAY BUSCAESTADOS(IND) AT 0658
                   GO TO R-CIDADE
               ELSE
                   ADD 1 TO IND
                   GO TO VALIDA-UF
               END-IF
           END-IF
               CONTINUE.

      *RECEBE NOME DA CIDADE
       R-CIDADE.
           ACCEPT TCIDADE
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               GO TO R-UF
           ELSE IF CIDADE = SPACES
               PERFORM LIMPATELA
               DISPLAY "CIDADE INVALIDO" AT 1505
               GO TO R-CIDADE
           ELSE
               PERFORM LIMPATELA
               GO TO R-BAIRRO
           END-IF.

      *RECEBE NOME DO BAIRRO
       R-BAIRRO.
           ACCEPT TBAIRRO
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               GO TO R-CIDADE
           ELSE IF BAIRRO = SPACES
               PERFORM LIMPATELA
               DISPLAY "BAIRRO INVALIDO" AT 1505
               GO TO R-BAIRRO
           ELSE
               PERFORM LIMPATELA
               GO TO R-REFR
           END-IF.

      *RECEBE REFERENCIA DO ENDERECO
       R-REFR.
           ACCEPT TREFERENCIA
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               GO TO R-BAIRRO
           ELSE IF REFERENCIA = SPACES
               PERFORM LIMPATELA
               DISPLAY "REFERENCIA INVALIDO" AT 1505
               GO TO R-REFR
           ELSE
               PERFORM LIMPATELA
               GO TO R-LATI
           END-IF.

      *RECEBE LATITUDE
       R-LATI.
           ACCEPT TLATI
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               GO TO R-REFR
           ELSE IF LATITUDE = SPACES
               PERFORM LIMPATELA
               DISPLAY "LATITUDE INVALIDO" AT 1505
               GO TO R-LATI
           ELSE
               PERFORM LIMPATELA
               GO TO R-LONG
           END-IF.

      *RECEBE LONGITUDE
       R-LONG.
           ACCEPT TLONG
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               GO TO R-LATI
           ELSE IF LONGITUDE = SPACES
               PERFORM LIMPATELA
               DISPLAY "LONGITUDE INVALIDA" AT 1505
               GO TO R-LONG
           ELSE
               PERFORM LIMPATELA
               CONTINUE
           END-IF.

      *VERIFICA SE CEP JA ESTA REGISTRADO
       VERIFICAOPC.
           IF EXISTENCIA = 0
               GO TO ESCRITA
           ELSE
               GO TO ALTERA
           END-IF
               CONTINUE.

      *FAZ LEITURA DO CEP NO ARQUIVO
       LERCEP.
           READ CADCEP
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   PERFORM LIMPATELA
                   DISPLAY TELACEP
                   DISPLAY "CEP JA CADASTRADO" AT 2010
                   DISPLAY "F1 ALTERAR, F2 CANCELAR" AT 2110
                   ACCEPT AUX AT 2360
                   ACCEPT ACTKEY FROM ESCAPE KEY
                   IF ACTKEY = 01
                       MOVE 1 TO EXISTENCIA
                       PERFORM LIMPATELA
                   ELSE
                       PERFORM LIMPAVAR
                       PERFORM LIMPATELA
                       DISPLAY TELACEP
                       GO TO MENU
                   END-IF
               ELSE
                   DISPLAY "ERRO NA LEITURA ARQUIVO CADCEP" AT 2010
                   GO TO SAIR
                 END-IF
           END-IF.

      *GRAVA DADOS DO CEP NO ARQUIVO
       ESCRITA.
           PERFORM LIMPATELA
           DISPLAY "GRAVAR DADOS?" AT 1605
           DISPLAY "F1 - SIM, F2 - NAO" AT 1705
           ACCEPT AUX AT 2360
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               WRITE REGCEP
               IF ERRO = "00" OR "02"
                   PERFORM LIMPATELA
                   CLOSE CADCEP
                   OPEN I-O CADCEP
                   DISPLAY "DADOS GRAVADOS" AT 2010
                   GO TO MENU
                ELSE IF ERRO = "22"
                   DISPLAY "CADCEP JA EXISTE " AT 2010
                   GO TO MENU
                ELSE
                  DISPLAY "ERRO NA GRAVACAO DO ARQUIVO CADCEP" AT 2010
                      GO TO MENU
           ELSE
               PERFORM LIMPAVAR
               PERFORM LIMPATELA
           GO TO MENU.

      *BUSCA CEP NO ARQUIVO
       PROCURA.
           READ CADCEP
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   DISPLAY TELACEP
                   DISPLAY "F1 DELETAR CEP, F2 VOLTAR" AT 1605
                   ACCEPT AUX AT 2360
                   ACCEPT ACTKEY FROM ESCAPE KEY
                   IF ACTKEY = 01
                       DELETE CADCEP
                       IF ERRO = "00"
                           PERFORM LIMPAVAR
                           DISPLAY TELACEP
                           CLOSE CADCEP
                           OPEN I-O CADCEP
                           DISPLAY "CEP EXCLUIDO" AT 2110
                           GO TO MENU
                       ELSE
                           PERFORM LIMPATELA
                           DISPLAY "ERRO AO REMOVER O CEP" AT 2110
                           GO TO MENU
                       END-IF
                   ELSE
                       PERFORM LIMPAVAR
                       DISPLAY TELACEP
                       GO TO MENU
                   END-IF
               END-IF
           ELSE
               PERFORM LIMPATELA
               DISPLAY "CEP NAO ENCONTRADO" AT 2010
               GO TO MENU
           END-IF.

      *ALTERA DADOS DO CEP NO ARQUIVO
       ALTERA.
           REWRITE REGCEP.
           IF ERRO = "00" OR "02"
               CLOSE CADCEP
               OPEN I-O CADCEP
               DISPLAY "INFORMACOES DE CEP ALTERADAS" AT 2110
               GO TO MENU
           ELSE
               DISPLAY "ERRO AO ALTERAR O CEP" AT 2110
           GO TO MENU.

      *LIMPA A TELA - PREENCHE COM ESPACOS EM BRANCO
       LIMPATELA.
           DISPLAY "                                         " AT 1405
           DISPLAY "                                         " AT 1505
           DISPLAY "                                         " AT 1605
           DISPLAY "                                         " AT 1705
           DISPLAY "                                         " AT 1805
           DISPLAY "                                         " AT 1905
           DISPLAY "                                         " AT 2005
           DISPLAY "                                         " AT 2105
           DISPLAY "                                         " AT 2205
           DISPLAY "                                         " AT 2305.

      *LIMPA DADOS NAS VARIAVEIS
       LIMPAVAR.
           MOVE ZEROES TO CEP ACTKEY EXISTENCIA
           MOVE SPACES TO LOGRADOURO BAIRRO AUX
           MOVE SPACES TO CIDADE REFERENCIA UF LATITUDE LONGITUDE.

      *FIM DO PROGRAMA
       SAIR.
           CLOSE CADCEP.
           END PROGRAM P172CEP.
