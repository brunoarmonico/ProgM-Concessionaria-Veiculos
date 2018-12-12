      *AUTOR: BRUNO CHAGAS DA SILVA ARMONICO
      *TURMA: ADS 4ฐ SEMESTRE - TARDE - 2017
      *
      *RODAR NO OPEN COBOL IDE (ADMINISTRADOR), COMPILADOR DEFAULT OU MF
      *ATIVAR "RUN IN EXTERNAL TERMINAL" NA GUIA "RUN" DAS
      *PREFERENCIAS (F2) DO OPEN COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P172903.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADPROPR ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CPF
               FILE STATUS  IS ERRO
               ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.

           SELECT CADCEP ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CEP
               FILE STATUS  IS ERRO
               ALTERNATE RECORD KEY IS LOGRADOURO WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
           FD CADPROPR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPROPR.DAT".
           01 REGPROPR.
               02 CPF PIC X(11) VALUE ZEROES.
               02 NOME PIC X(30) VALUE SPACES.
               02 DATANASC.
                   03 DIA PIC 9(2) VALUE ZEROES.
                   03 MES PIC 9(2) VALUE ZEROES.
                   03 ANO PIC 9(4) VALUE ZEROES.
               02 BCEP PIC 9(8) VALUE ZEROES.
               02 NUM PIC 9(6) VALUE ZEROES.
               02 COMPLEMENTO PIC X(25) VALUE SPACES.
               02 TELEFONE.
                   03 DDD PIC 9(2) VALUE ZEROES.
                   03 NUMERO PIC 9(8) VALUE ZEROES.
               02 EMAIL PIC X(30) VALUE SPACES.

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
           01 BSCEP PIC 9(1) VALUE 0.
           01 ERRO PIC X(2) VALUE "00".
           01 AUX PIC X(01) VALUE SPACES.
           01 ACTKEY PIC 9(02) VALUE ZEROES.
           01 EXISTENCIA PIC 9(1) VALUE 0.
           01 AUXANO       PIC 9(2) VALUE ZEROS.
           01 AUXANO2      PIC 9(2) VALUE ZEROS.

           SCREEN SECTION.
       01  TELAPROPR.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "ษอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 02  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออป".
           05  LINE 03  COLUMN 01
               VALUE  "บ                           CADASTRO DE".
           05  LINE 03  COLUMN 41
               VALUE  "PROPRIETARIO                           บ".
           05  LINE 04  COLUMN 01
               VALUE  "ฬอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 04  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออน".
           05  LINE 05  COLUMN 01
               VALUE  "บ CPF:             NOME:".
           05  LINE 05  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 06  COLUMN 01
               VALUE  "บ".
           05  LINE 06  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 07  COLUMN 01
               VALUE  "บ DATA DE NASCIMENTO:".
           05  LINE 07  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 08  COLUMN 01
               VALUE  "บฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ".
           05  LINE 08  COLUMN 41
               VALUE  "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤบ".
           05  LINE 09  COLUMN 01
               VALUE  "บ CEP:          LOGRADOURO:".
           05  LINE 09  COLUMN 41
               VALUE  "                        NUMERO:        บ".
           05  LINE 10  COLUMN 01
               VALUE  "บ".
           05  LINE 10  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 11  COLUMN 01
               VALUE  "บ COMPLEMENTO:".
           05  LINE 11  COLUMN 41
               VALUE  " BAIRRO:                               บ".
           05  LINE 12  COLUMN 01
               VALUE  "บ".
           05  LINE 12  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 13  COLUMN 01
               VALUE  "บ CIDADE:                      UF:".
           05  LINE 13  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 14  COLUMN 01
               VALUE  "บฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ".
           05  LINE 14  COLUMN 41
               VALUE  "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤบ".
           05  LINE 15  COLUMN 01
               VALUE  "บ TELEFONE:            E-MAIL".
           05  LINE 15  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 16  COLUMN 01
               VALUE  "ฬอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 16  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออน".
           05  LINE 17  COLUMN 01
               VALUE  "บ".
           05  LINE 17  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 18  COLUMN 01
               VALUE  "บ".
           05  LINE 18  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 19  COLUMN 01
               VALUE  "บ".
           05  LINE 19  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 20  COLUMN 01
               VALUE  "บ".
           05  LINE 20  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 21  COLUMN 01
               VALUE  "บ".
           05  LINE 21  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 22  COLUMN 01
               VALUE  "บ".
           05  LINE 22  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 23  COLUMN 01
               VALUE  "บ".
           05  LINE 23  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 24  COLUMN 01
               VALUE  "ศอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 24  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออผ".
           05  TCPF
               LINE 05  COLUMN 07  PIC 9(11)
               USING  CPF
               HIGHLIGHT     BLANK ZERO.
           05  TNOME
               LINE 05  COLUMN 25  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TNASC
               LINE 07  COLUMN 22  PIC 9(08)
               USING  DATANASC
               HIGHLIGHT     BLANK ZERO.
           05  TCEP
               LINE 09  COLUMN 07  PIC 99999.999
               USING  CEP
               HIGHLIGHT     BLANK ZERO.
           05  TLOGRADOURO
               LINE 09  COLUMN 28  PIC X(35)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TNUM
               LINE 09  COLUMN 72  PIC 9(06)
               USING  NUM
               HIGHLIGHT     BLANK ZERO.
           05  TCOMPLEMENTO
               LINE 11  COLUMN 15  PIC X(25)
               USING  COMPLEMENTO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 11  COLUMN 49  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 13  COLUMN 10  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 13  COLUMN 35  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TFONE
               LINE 15  COLUMN 12  PIC 9(10)
               USING  TELEFONE
               HIGHLIGHT     BLANK ZERO.
           05  TMAIL
               LINE 15  COLUMN 31  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.


           PROCEDURE DIVISION.
      *ABRE ARQUIVO COM DADOS DO PROPRIETARIO
       ABREARQUIVO.
           OPEN I-O CADPROPR
           IF ERRO NOT = "00"
               IF ERRO = "30" OR ERRO = "35"
                   OPEN OUTPUT CADPROPR
                   CLOSE CADPROPR
                  DISPLAY "ARQUIVO CADPROPR FOI CRIADO" AT 0622
                   GO TO ABREARQUIVO
               ELSE
                  DISPLAY "ERRO NA ABERTURA DO ARQUIVO CADPROPR" AT 0622
           ELSE
               DISPLAY TELAPROPR
               CONTINUE.

      *MENU PRINCIPAL
       MENU.
           PERFORM LIMPAVAR
           DISPLAY "F1 CADASTRAR, F2 PROCURAR, F3 SAIR" AT 1705
           ACCEPT AUX AT 0101
           ACCEPT ACTKEY FROM ESCAPE KEY
           EVALUATE ACTKEY
           WHEN = 01
               PERFORM LIMPATELA
               DISPLAY TELAPROPR
               GO TO R-CPF
           WHEN = 02
      *REALIZA BUSCA POR CPF
               PERFORM LIMPATELA
               DISPLAY "CPF: " AT 1805
               ACCEPT CPF AT 1814
               ACCEPT ACTKEY FROM ESCAPE KEY
               IF ACTKEY = 01
                   PERFORM LIMPATELA
                   PERFORM LIMPAVAR
                   DISPLAY TELAPROPR
                   GO TO MENU
               END-IF
               PERFORM BUSCAPROPR
           WHEN = 03
               GO TO SAIR
           WHEN OTHER
           DISPLAY "OPCAO INVALIDA" AT 1805
               GO TO MENU
           END-EVALUATE.

      *RECEBE CPF DO PROPRIETARIO
       R-CPF.
           ACCEPT TCPF
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               PERFORM LIMPAVAR
               DISPLAY TELAPROPR
               GO TO MENU
           ELSE IF CPF = ZEROES OR CPF = SPACES
               DISPLAY "CPF INVALIDO" AT 1805
               PERFORM LIMPAVAR
               GO TO R-CPF
           ELSE
               PERFORM LERPROPR
               CONTINUE.

      *RECEBE NOME DO PROPRIETARIO
       R-NOME.
           ACCEPT TNOME
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE SPACES TO NOME
               DISPLAY TNOME
               GO TO R-CPF
           ELSE IF NOME = SPACES
               DISPLAY "NOME INVALIDO" AT 1805
               GO TO R-NOME
           ELSE
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE DATA DE NASCIMENTO DO PROPRIETARIO
       R-NASC.
           ACCEPT TNASC WITH UPDATE
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE ZEROES TO DATANASC
               DISPLAY TNASC
               GO TO R-NOME
           END-IF
           IF ANO < 1900 OR ANO > 2017
               DISPLAY "ANO INVALIDO" AT 1705
               MOVE ZEROES TO DATANASC
               GO TO R-NASC
           ELSE
      *VERIFICA DATA VALIDA
               DIVIDE ANO BY 4 GIVING AUXANO REMAINDER AUXANO
               DIVIDE ANO BY 400 GIVING AUXANO2 REMAINDER AUXANO2
           END-IF
           IF MES = 4 OR MES = 6 OR MES = 9 OR MES = 11
               IF DIA > 30 OR DIA < 1
                   DISPLAY "DIA INVALIDO" AT 1705
                   GO TO R-NASC
               END-IF
           END-IF
           IF MES = 02
               IF AUXANO = 0 OR AUXANO2 = 0
                  IF DIA > 29 OR DIA < 1
                       DISPLAY "DIA INVALIDO PARA FEVEREIRO" AT 1705
                       GO TO R-NASC
                  ELSE
                       PERFORM LIMPATELA
                       GO TO R-CEP
                  END-IF

               ELSE
                  IF DIA > 28 OR DIA < 1
                       DISPLAY "DIA INVALIDO PARA FEVEREIRO" AT 1705
                       GO TO R-NASC
                  ELSE
                       PERFORM LIMPATELA
                       GO TO R-CEP
                  END-IF
              END-IF

           ELSE IF MES < 1 OR MES > 12
               DISPLAY "MES INVALIDO" AT 1705
               GO TO R-NASC
           ELSE
               IF DIA < 1 OR DIA > 31
                   DISPLAY "DIA INVALIDO" AT 1705
                   GO TO R-NASC
               END-IF
           PERFORM LIMPATELA
           CONTINUE.

      *RECEBE CEP DO PROPRIETARIO
       R-CEP.
           MOVE 0 TO BSCEP
           ACCEPT TCEP
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE ZEROES TO CEP
               DISPLAY TCEP
               GO TO R-NASC
           ELSE IF CEP = ZEROES
               DISPLAY "CEP INVALIDO" AT 1805
               GO TO R-CEP
           ELSE
               PERFORM LIMPATELA
               PERFORM LERCEP
               IF BSCEP = 1
                   GO TO R-CEP
               ELSE
                   MOVE CEP TO BCEP
               CONTINUE.

      *RECEBE NUMERO DA CASA DO PROPRIETARIO
       R-NUM.
           ACCEPT TNUM
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE ZEROES TO NUM
               DISPLAY TNUM
               GO TO R-CEP
           ELSE IF NUM = ZEROES
               DISPLAY "NUMERO INVALIDO" AT 1805
               GO TO R-NUM
           ELSE
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE COMPLEMENTO DE ENDERECO DO PROPRIETARIO
       R-COMP.
           ACCEPT TCOMPLEMENTO
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE SPACES TO COMPLEMENTO
               DISPLAY TCOMPLEMENTO
               GO TO R-NUM
           END-IF
           CONTINUE.

      *RECEBE TELEFONE DO PROPRIETARIO
       R-TELEFONE.
           ACCEPT TFONE
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE ZEROES TO TELEFONE
               DISPLAY TFONE
               GO TO R-COMP
           ELSE IF DDD = ZEROES OR NUMERO = ZEROES
               DISPLAY "TELEFONE INVALIDO" AT 1805
               GO TO R-TELEFONE
           ELSE
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE E-MAIL DO PROPRIETARIO
       R-MAIL.
           ACCEPT TMAIL
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE SPACES TO EMAIL
               DISPLAY TMAIL
               GO TO R-TELEFONE
           ELSE IF EMAIL = SPACES
               DISPLAY "E-MAIL INVALIDO" AT 1805
               GO TO R-MAIL
           ELSE
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE SE PROPRIETARIO JA ESTA REGISTRADO
       VERIFICAOPC.
           IF EXISTENCIA = 0
               GO TO GRAVAPROPR
           ELSE
               GO TO ALTERACHAVE
           END-IF.

      *FAZ LEITURA DE DADOS DO PROPRIETARIO NO ARQUIVO
       LERPROPR.
           READ CADPROPR
              IF ERRO NOT = "23"
                 IF ERRO = "00"
                   PERFORM LIMPATELA
                   MOVE BCEP TO CEP
                   DISPLAY TELAPROPR
                   PERFORM LERCEP
                   DISPLAY "PROPRIETARIO JA CADASTRADO" AT 1810
                   DISPLAY "F1 ALTERAR, F2 CANCELAR" AT 1910
                   ACCEPT AUX AT 0101
                   ACCEPT ACTKEY FROM ESCAPE KEY
                   IF ACTKEY = 01
                       MOVE 1 TO EXISTENCIA
                       PERFORM LIMPATELA
                   ELSE
                       MOVE 0 TO EXISTENCIA
                       PERFORM LIMPATELA
                       PERFORM LIMPAVAR
                       GO TO MENU
                   END-IF
                 ELSE
                   DISPLAY "ERRO NA LEITURA ARQUIVO CADPROPR" AT 2010
                   GO TO SAIR
                 END-IF
              END-IF
           CONTINUE.

      *GRAVA DADOS DO PROPRIETARIO NO ARQUIVO
       GRAVAPROPR.
           PERFORM LIMPATELA
           DISPLAY "GRAVAR DADOS?" AT 1705
           DISPLAY "F1 - SIM, F2 - NAO" AT 1805
           ACCEPT AUX AT 0101
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               WRITE REGPROPR
               IF ERRO = "00" OR "02"
                   PERFORM LIMPATELA
                   CLOSE CADPROPR
                   OPEN I-O CADPROPR
                   DISPLAY "DADOS GRAVADOS" AT 2010
                   GO TO MENU
               ELSE IF ERRO = "22"
                   DISPLAY "CADPROPR JA EXISTE " AT 2010
                   GO TO MENU
               ELSE
                  DISPLAY "ERRO NA GRAVACAO DO ARQUIVO CADPROPR" AT 2010
                   GO TO MENU
               END-IF
           ELSE
               PERFORM LIMPAVAR
               PERFORM LIMPATELA
           GO TO MENU.

      *ALTERA DADOS DO PROPRIETARIO NO ARQUIVO
       ALTERACHAVE.
           REWRITE REGPROPR
           IF ERRO = "00" OR "02"
               CLOSE CADPROPR
               OPEN I-O CADPROPR
               DISPLAY "INFORMACOES DE PROPRIETARIO ALTERADAS" AT 2010
               GO TO MENU
           ELSE
               DISPLAY "ERRO AO ALTERAR PROPRIETARIO" AT 2010
           GO TO MENU.

      *BUSCA DADOS DO PROPRIETARIO
       BUSCAPROPR.
           READ CADPROPR
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   MOVE BCEP TO CEP
                   DISPLAY TELAPROPR
                   PERFORM LERCEP
      *POSSIBILITA REMOวรO DOS DADOS ENCONTRADOS
                   DISPLAY "F1 DELETAR PROPRIETARIO, F2 VOLTAR" AT 1705
                   ACCEPT AUX AT 0101
                   ACCEPT ACTKEY FROM ESCAPE KEY
                   IF ACTKEY = 01
                       DELETE CADPROPR
                       IF ERRO = "00"
                           PERFORM LIMPAVAR
                           DISPLAY TELAPROPR
                           CLOSE CADPROPR
                           OPEN I-O CADPROPR
                           DISPLAY "PROPRIETARIO EXCLUIDO" AT 2110
                           GO TO MENU
                       ELSE
                           PERFORM LIMPATELA
                          DISPLAY "ERRO AO REMOVER PROPRIETARIO" AT 2110
                           GO TO MENU
                       END-IF
                   ELSE
                       PERFORM LIMPATELA
                       GO TO MENU
                   END-IF
               END-IF
           ELSE
               PERFORM LIMPATELA
               DISPLAY "PROPRIETARIO NAO ENCONTRADO" AT 2010
               GO TO MENU
           END-IF.

      *FAZ LEITURA DE DADOS DO CEP NO ARQUIVO DE CEP
       LERCEP.
           OPEN I-O CADCEP
           READ CADCEP
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   DISPLAY TLOGRADOURO
                   DISPLAY TBAIRRO
                   DISPLAY TCIDADE
                   DISPLAY TUF
                   CLOSE CADCEP
               ELSE
                   DISPLAY "ERRO AO LER O CEP" AT 2005
                   PERFORM LIMPAVAR
                   GO TO MENU
               END-IF
           ELSE
               MOVE 1 TO BSCEP
               DISPLAY "CEP NAO ENCONTRADO" AT 1805
               CLOSE CADCEP
           END-IF.

      *LIMPA DADOS NAS VARIAVEIS
       LIMPAVAR.
           MOVE ZEROES TO CPF DATANASC BCEP NUM DDD NUMERO EXISTENCIA
           MOVE ZEROES TO BSCEP AUX ACTKEY CEP
           MOVE SPACES TO NOME COMPLEMENTO EMAIL LOGRADOURO BAIRRO
           MOVE SPACES TO CIDADE UF.

      *LIMPA A TELA - PREENCHE COM ESPACOS EM BRANCO
       LIMPATELA.
           DISPLAY "                                       " AT 1702
           DISPLAY "                                       " AT 1741
           DISPLAY "                                       " AT 1802
           DISPLAY "                                       " AT 1841
           DISPLAY "                                       " AT 1902
           DISPLAY "                                       " AT 1941
           DISPLAY "                                       " AT 2002
           DISPLAY "                                       " AT 2041
           DISPLAY "                                       " AT 2102
           DISPLAY "                                       " AT 2141
           DISPLAY "                                       " AT 2202
           DISPLAY "                                       " AT 2241
           DISPLAY "                                       " AT 2302
           DISPLAY "                                       " AT 2341.

      *FIM DO PROGRAMA
       SAIR.
           CLOSE CADPROPR.
           END PROGRAM P172903.
