      *AUTOR: BRUNO CHAGAS DA SILVA ARMONICO
      *TURMA: ADS 4ฐ SEMESTRE - TARDE - 2017
      *
      *RODAR NO OPEN COBOL IDE (ADMINISTRADOR), COMPILADOR DEFAULT OU MF
      *ATIVAR "RUN IN EXTERNAL TERMINAL" NA GUIA "RUN" DAS
      *PREFERENCIAS (F2) DO OPEN COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P172901.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADMARCA ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CODIGO
               FILE STATUS  IS ERRO
               ALTERNATE RECORD KEY IS MARCA WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
           FD CADMARCA
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMARCA.DAT".
           01 REGMARCA.
               02 CODIGO PIC 9(3) VALUE ZEROES.
               02 MARCA  PIC X(20) VALUE SPACES.
               02 ORIGEM PIC X(1) VALUE SPACES.

           WORKING-STORAGE SECTION.
           01 TIPOORIGEM.
               03 PIC X(9) VALUE "NACIONAL".
               03 PIC X(9) VALUE "IMPORTADO".
           01 BUSCAOR REDEFINES TIPOORIGEM.
               02 BUSCA PIC X(9) OCCURS 2 TIMES.
           01 ERRO PIC X(2) VALUE "00".
           01 AUX PIC X(01) VALUE SPACES.
           01 ACTKEY PIC 9(02) VALUE ZEROES.
           01 EXISTENCIA PIC 9(1) VALUE 0.

           SCREEN SECTION.
       01  TELAMARCA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "ษอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 02  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออป".
           05  LINE 03  COLUMN 01
               VALUE  "บ                             CADASTRO D".
           05  LINE 03  COLUMN 41
               VALUE  "E MARCA                                บ".
           05  LINE 04  COLUMN 01
               VALUE  "ฬอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 04  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออน".
           05  LINE 05  COLUMN 01
               VALUE  "บ".
           05  LINE 05  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 06  COLUMN 01
               VALUE  "บ  CODIGO:".
           05  LINE 06  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 07  COLUMN 01
               VALUE  "บ".
           05  LINE 07  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 08  COLUMN 01
               VALUE  "บ  MARCA:".
           05  LINE 08  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 09  COLUMN 01
               VALUE  "บ".
           05  LINE 09  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 10  COLUMN 01
               VALUE  "บ  ORIGEM:".
           05  LINE 10  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 11  COLUMN 01
               VALUE  "บ".
           05  LINE 11  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 12  COLUMN 01
               VALUE  "ฬอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 12  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออน".
           05  LINE 13  COLUMN 01
               VALUE  "บ".
           05  LINE 13  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 14  COLUMN 01
               VALUE  "บ".
           05  LINE 14  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 15  COLUMN 01
               VALUE  "บ".
           05  LINE 15  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 16  COLUMN 01
               VALUE  "บ".
           05  LINE 16  COLUMN 41
               VALUE  "                                       บ".
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
               VALUE  "ศอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 21  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออผ".
           05  TCODIGO
               LINE 06  COLUMN 12  PIC 9(03)
               USING  CODIGO
               BLANK ZERO.
           05  TMARCA
               LINE 08  COLUMN 11  PIC X(20)
               USING  MARCA.
           05  TORIGEM
               LINE 10  COLUMN 12  PIC X(01)
               USING  ORIGEM.

           PROCEDURE DIVISION.
      *ABRE ARQUIVO COM DADOS DAS MARCAS
       ABREARQUIVO.
           OPEN I-O CADMARCA
           IF ERRO NOT = "00"
               IF ERRO = "30" OR ERRO = "35"
                   OPEN OUTPUT CADMARCA
                   CLOSE CADMARCA
                  DISPLAY "ARQUIVO CADCEP FOI CRIADO" AT 0622
                   GO TO ABREARQUIVO
               ELSE
                  DISPLAY "ERRO NA ABERTURA DO ARQUIVO CADCEP" AT 0622
           ELSE
               DISPLAY TELAMARCA
               CONTINUE.

      *MENU PRINCIPAL
       MENU.
           PERFORM LIMPAVAR
           DISPLAY "F1 CADASTRAR, F2 PROCURAR, F3 SAIR" AT 1505
           ACCEPT AUX AT 2360
           ACCEPT ACTKEY FROM ESCAPE KEY
           EVALUATE ACTKEY
           WHEN = 01
               DISPLAY TELAMARCA
               GO TO R-CODIGO
           WHEN = 02
               PERFORM LIMPATELA
               DISPLAY "CODIGO: " AT 1705
               ACCEPT CODIGO AT 1714
               ACCEPT ACTKEY FROM ESCAPE KEY
               IF ACTKEY = 01
                   PERFORM LIMPAVAR
                   DISPLAY TELAMARCA
                   GO TO MENU
               END-IF
               PERFORM BUSCAMARCA
           WHEN = 03
               GO TO SAIR
           WHEN OTHER
           DISPLAY "OPCAO INVALIDA" AT 1705
               GO TO MENU
           END-EVALUATE.

      *RECEBE CODIGO DA MARCA
       R-CODIGO.
           ACCEPT TCODIGO
           ACCEPT ACTKEY FROM ESCAPE KEY
      *F1 PARA VOLTAR AO MENU ANTERIOR
           IF ACTKEY = 01
               PERFORM LIMPAVAR
               DISPLAY TELAMARCA
               GO TO MENU
           ELSE IF CODIGO = ZEROES
               DISPLAY "CODIGO INVALIDO" AT 1405
               PERFORM LIMPAVAR
               GO TO R-CODIGO
           ELSE
               PERFORM LERMARCA
               CONTINUE.

      *RECEBE NOME DA MARCA
       R-MARCA.
           ACCEPT TMARCA
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE SPACES TO MARCA
               DISPLAY TMARCA
               GO TO R-CODIGO
           ELSE IF MARCA = SPACES
               DISPLAY "MARCA INVALIDA" AT 1405
               GO TO R-MARCA
           ELSE
               CONTINUE.

      *RECEBE ORIGEM DA MARCA
       R-ORIGEM.
           DISPLAY "N - NACIONAL, I - IMPORTADO" AT 1705
           ACCEPT TORIGEM
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE SPACES TO ORIGEM
               DISPLAY TORIGEM
               GO TO R-MARCA
           END-IF
           EVALUATE ORIGEM
           WHEN = "N" OR "n"
               DISPLAY BUSCA(1) AT 1015
               PERFORM LIMPATELA
               GO TO VERIFICAOPC
           WHEN = "I" OR "i"
               DISPLAY BUSCA(2) AT 1015
               PERFORM LIMPATELA
               GO TO VERIFICAOPC
           WHEN OTHER
               DISPLAY "ORIGEM INVALIDA" AT 1405
               GO TO R-ORIGEM.

      *VERIFICA SE MARCA JA E REGISTRADA
       VERIFICAOPC.
           IF EXISTENCIA = 0
               GO TO GRAVAMARCA
           ELSE
               GO TO ALTERACHAVE
           END-IF.

      *FAZ LEITURA DA MARCA NO ARQUIVO DE DADOS
       LERMARCA.
           READ CADMARCA
              IF ERRO NOT = "23"
                 IF ERRO = "00"
                   DISPLAY TELAMARCA
                   DISPLAY "MARCA JA CADASTRADA" AT 1610
                   DISPLAY "F1 ALTERAR, F2 CANCELAR" AT 1710
                   ACCEPT AUX AT 2360
                   ACCEPT ACTKEY FROM ESCAPE KEY
                   IF ACTKEY = 01
                       MOVE 1 TO EXISTENCIA
                       PERFORM LIMPATELA
                   ELSE
                       MOVE 0 TO EXISTENCIA
                       PERFORM LIMPAVAR
                       DISPLAY TELAMARCA
                       GO TO MENU
                   END-IF
                 ELSE
                   DISPLAY "ERRO NA LEITURA ARQUIVO CADMARCA" AT 2010
                   GO TO SAIR
                 END-IF
              END-IF
           CONTINUE.

      *GRAVA MARCA NO ARQUIVO DE DADOS
       GRAVAMARCA.
           PERFORM LIMPATELA
           DISPLAY "GRAVAR DADOS?" AT 1605
           DISPLAY "F1 - SIM, F2 - NAO" AT 1705
           ACCEPT AUX AT 2360
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               WRITE REGMARCA
               IF ERRO = "00" OR "02"
                   PERFORM LIMPATELA
                   CLOSE CADMARCA
                   OPEN I-O CADMARCA
                   DISPLAY "DADOS GRAVADOS" AT 2010
                   GO TO MENU
               ELSE IF ERRO = "22"
                   DISPLAY "CADMARCA JA EXISTE " AT 2010
                   GO TO MENU
               ELSE
                  DISPLAY "ERRO NA GRAVACAO DO ARQUIVO CADMARCA" AT 2010
                   GO TO MENU
               END-IF
           ELSE
               PERFORM LIMPAVAR
               PERFORM LIMPATELA
           GO TO MENU.

      *BUSCA MARCA NO ARQUIVO DE DADOS
       BUSCAMARCA.
           READ CADMARCA
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   DISPLAY TELAMARCA
                   DISPLAY "F1 DELETAR MARCA, F2 VOLTAR" AT 1605
                   ACCEPT AUX AT 2360
                   ACCEPT ACTKEY FROM ESCAPE KEY
                   IF ACTKEY = 01
                       DELETE CADMARCA
      *REMOVE MARCA DO ARQUIVO DE DADOS
                       IF ERRO = "00"
                           PERFORM LIMPAVAR
                           DISPLAY TELAMARCA
                           CLOSE CADMARCA
                           OPEN I-O CADMARCA
                           DISPLAY "MARCA EXCLUIDA" AT 2010
                           GO TO MENU
                       ELSE
                           PERFORM LIMPATELA
                           DISPLAY "ERRO AO REMOVER MARCA" AT 2110
                           GO TO MENU
                       END-IF
                   ELSE
                       PERFORM LIMPAVAR
                       DISPLAY TELAMARCA
                       GO TO MENU
                   END-IF
               END-IF
           ELSE
               PERFORM LIMPATELA
               DISPLAY "MARCA NAO ENCONTRADA" AT 2010
               GO TO MENU
           END-IF.

      *ALTERA DADOS DA MARCA NO ARQUIVO DE DADOS
       ALTERACHAVE.
           REWRITE REGMARCA
           IF ERRO = "00" OR "02"
               CLOSE CADMARCA
               OPEN I-O CADMARCA

               DISPLAY "INFORMACOES DE MARCA ALTERADAS" AT 2010
               GO TO MENU
           ELSE
               DISPLAY "ERRO AO ALTERAR MARCA" AT 2010
           GO TO MENU.

       LIMPAVAR.
           MOVE ZEROS TO CODIGO EXISTENCIA ACTKEY
           MOVE SPACES TO MARCA ORIGEM AUX.


      *LIMPA A TELA - PREENCHE COM ESPACOS EM BRANCO
       LIMPATELA.
           DISPLAY "                                       " AT 1302
           DISPLAY "                                       " AT 1341
           DISPLAY "                                       " AT 1402
           DISPLAY "                                       " AT 1441
           DISPLAY "                                       " AT 1502
           DISPLAY "                                       " AT 1541
           DISPLAY "                                       " AT 1602
           DISPLAY "                                       " AT 1641
           DISPLAY "                                       " AT 1702
           DISPLAY "                                       " AT 1741
           DISPLAY "                                       " AT 1802
           DISPLAY "                                       " AT 1841
           DISPLAY "                                       " AT 1902
           DISPLAY "                                       " AT 1941
           DISPLAY "                                       " AT 2002
           DISPLAY "                                       " AT 2041.

      *FIM DO PROGRAMA
       SAIR.
           CLOSE CADMARCA.
           END PROGRAM P172901.
