      *AUTOR: BRUNO CHAGAS DA SILVA ARMONICO
      *TURMA: ADS 4ฐ SEMESTRE - TARDE - 2017
      *
      *RODAR NO OPEN COBOL IDE (ADMINISTRADOR), COMPILADOR DEFAULT OU MF
      *ATIVAR "RUN IN EXTERNAL TERMINAL" NA GUIA "RUN" DAS
      *PREFERENCIAS (F2) DO OPEN COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P172902.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADMODEL ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CODIGOM
               FILE STATUS  IS ERRO
               ALTERNATE RECORD KEY IS MODELO WITH DUPLICATES.
           SELECT CADMARCA ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CODIGO
               FILE STATUS  IS ERRO
               ALTERNATE RECORD KEY IS MARCA WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
           FD CADMODEL
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMODEL.DAT".
           01 REGMODEL.
               02 CODIGOM.
                   03 CODIGO1 PIC 9(3) VALUE ZEROES.
                   03 CODIGO2 PIC 9(3) VALUE ZEROES.
               02 MODELO PIC X(20) VALUE SPACES.

           FD CADMARCA
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMARCA.DAT".
           01 REGMARCA.
               02 CODIGO PIC 9(3).
               02 MARCA  PIC X(20).

           WORKING-STORAGE SECTION.
           01 BCMARCA PIC 9(1) VALUE 0.
           01 ERRO PIC X(2) VALUE "00".
           01 AUX PIC X(01) VALUE SPACES.
           01 ACTKEY PIC 9(02) VALUE ZEROES.
           01 EXISTENCIA PIC 9(1) VALUE 0.
      ***************************************
           01 FLAG                   PIC 9(2) COMP-X VALUE 1.
           01 USER-KEY-CONTROL.
               03 ENABLE-FN-KEYS          PIC 9(2) COMP-X VALUE 1.
               03 FILLER                  PIC X           VALUE "1".
               03 FIRST-USER-KEY          PIC 9(2) COMP-X VALUE 1.
               03 NUMBER-OF-KEYS          PIC 9(2) COMP-X VALUE 10.
           01 KEY-STATUS.
               04 KEY-TYPE     PIC X.
               04 KEY-CODE-1   PIC 9(2) COMP-X.
               04 KEY-CODE-2   PIC 9(2) COMP-X.

           SCREEN SECTION.
       01  TELAMODELO.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "ษอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 02  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออป".
           05  LINE 03  COLUMN 01
               VALUE  "บ                            CADASTRO DE".
           05  LINE 03  COLUMN 41
               VALUE  " MODELO                                บ".
           05  LINE 04  COLUMN 01
               VALUE  "ฬอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 04  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออน".
           05  LINE 05  COLUMN 01
               VALUE  "บ  CODIGO:".
           05  LINE 05  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 06  COLUMN 01
               VALUE  "บ".
           05  LINE 06  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 07  COLUMN 01
               VALUE  "บ  MARCA:".
           05  LINE 07  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 08  COLUMN 01
               VALUE  "บ".
           05  LINE 08  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 09  COLUMN 01
               VALUE  "บ  MODELO:".
           05  LINE 09  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 10  COLUMN 01
               VALUE  "บ".
           05  LINE 10  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 11  COLUMN 01
               VALUE  "ฬอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 11  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออน".
           05  LINE 12  COLUMN 01
               VALUE  "บ".
           05  LINE 12  COLUMN 41
               VALUE  "                                       บ".
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
               VALUE  "ศอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 18  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออผ".
           05  TCODIGO
               LINE 05  COLUMN 12  PIC 999999
               USING  CODIGOM
               HIGHLIGHT     BLANK ZERO.
           05  TMODELO
               LINE 09  COLUMN 12  PIC X(20)
               USING  MODELO
               HIGHLIGHT.

           PROCEDURE DIVISION.
      *ABRE ARQUIVO COM DADOS DO MODELO
       ABREARQUIVO.
           OPEN I-O CADMODEL
           IF ERRO NOT = "00"
               IF ERRO = "30" OR ERRO = "35"
                   OPEN OUTPUT CADMODEL
                   CLOSE CADMODEL
                  DISPLAY "ARQUIVO CADCEP FOI CRIADO" AT 0622
                   GO TO ABREARQUIVO
               ELSE
                  DISPLAY "ERRO NA ABERTURA DO ARQUIVO CADCEP" AT 0622
           ELSE
               DISPLAY TELAMODELO
               CONTINUE.

      *MENU PRINCIPAL
       MENU.
           PERFORM LIMPAVAR
           DISPLAY "F1 CADASTRAR, F2 PROCURAR, F3 SAIR" AT 1305
           ACCEPT AUX AT 2360
           ACCEPT ACTKEY FROM ESCAPE KEY
           EVALUATE ACTKEY
           WHEN = 01
               PERFORM LIMPATELA
               DISPLAY TELAMODELO
               GO TO R-CODIGO
           WHEN = 02
               PERFORM LIMPATELA
               DISPLAY "CODIGO: " AT 1705
               ACCEPT CODIGOM AT 1714
               ACCEPT ACTKEY FROM ESCAPE KEY
               IF ACTKEY = 01
                   PERFORM LIMPAVAR
                   DISPLAY TELAMODELO
                   GO TO MENU
               END-IF
               PERFORM BUSCAMODELO
           WHEN = 03
               GO TO SAIR
           WHEN OTHER
           DISPLAY "OPCAO INVALIDA" AT 1705
               GO TO MENU
           END-EVALUATE.

      *RECEBE CODIGO DO MODELO
       R-CODIGO.
           MOVE 0 TO BCMARCA
           ACCEPT TCODIGO
           ACCEPT ACTKEY FROM ESCAPE KEY
      *F1 PARA VOLTAR AO MENU ANTERIOR
           IF ACTKEY = 01
               PERFORM LIMPAVAR
               DISPLAY TELAMODELO
               GO TO MENU
           ELSE IF CODIGO1 = ZEROES OR CODIGO2 = ZEROES
               DISPLAY "CODIGO INVALIDO" AT 1405
               PERFORM LIMPAVAR
               GO TO R-CODIGO
           ELSE
               PERFORM LERMARCA
               IF BCMARCA = 1
                   GO TO R-CODIGO
               ELSE
                   PERFORM LERMODELO
               CONTINUE.

      *RECEBE NOME DO MODELO
       R-MODELO.
           ACCEPT TMODELO
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE SPACES TO MODELO
               DISPLAY TMODELO
               GO TO R-CODIGO
           ELSE IF CODIGO = SPACES
               DISPLAY "MODELO INVALIDO" AT 1405
               PERFORM LIMPAVAR
               GO TO R-CODIGO
           ELSE
               CONTINUE.

      *VERIFICA SE MODELO JA E REGISTRADO
       VERIFICAOPC.
           IF EXISTENCIA = 0
               GO TO GRAVAMODELO
           ELSE
               GO TO ALTERACHAVE
           END-IF.

      *FAZ LEITURA DO MODELO NO ARQUIVO DE DADOS
       LERMODELO.
           PERFORM LIMPATELA
           READ CADMODEL
              IF ERRO NOT = "23"
                 IF ERRO = "00"
                   DISPLAY TELAMODELO
                   PERFORM LERMARCA
      *POSSIBILITA ALTERACAO DE DADOS DO MODELO SE JA CADASTRADO
                   DISPLAY "MODELO JA CADASTRADO" AT 1310
                   DISPLAY "F1 ALTERAR, F2 CANCELAR" AT 1410
                   ACCEPT AUX AT 2360
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
                   DISPLAY "ERRO NA LEITURA ARQUIVO CADMODEL" AT 1710
                   GO TO SAIR
                 END-IF
              END-IF
           CONTINUE.

      *GRAVA MODELO NO ARQUIVO DE DADOS
       GRAVAMODELO.
           PERFORM LIMPATELA
           DISPLAY "GRAVAR DADOS?" AT 1305
           DISPLAY "F1 - SIM, F2 - NAO" AT 1405
           ACCEPT AUX AT 2360
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               WRITE REGMODEL
               IF ERRO = "00" OR "02"
                   PERFORM LIMPATELA
                   CLOSE CADMODEL
                   OPEN I-O CADMODEL
                   DISPLAY "DADOS GRAVADOS" AT 1710
                   GO TO MENU
               ELSE IF ERRO = "22"
                   DISPLAY "CADMODEL JA EXISTE " AT 1710
                   GO TO MENU
               ELSE
                  DISPLAY "ERRO NA GRAVACAO DO ARQUIVO CADMODEL" AT 1710
                   GO TO MENU
               END-IF
           ELSE
               PERFORM LIMPAVAR
               DISPLAY TELAMODELO
           GO TO MENU.

      *ALTERA DADOS DO MODELO NO ARQUIVO
       ALTERACHAVE.
           PERFORM LIMPATELA
           REWRITE REGMODEL
           IF ERRO = "00" OR "02"
               CLOSE CADMODEL
                   OPEN I-O CADMODEL
               DISPLAY "INFORMACOES DE MODELO ALTERADAS" AT 1710
               GO TO MENU
           ELSE
               DISPLAY "ERRO AO ALTERAR MODELO" AT 1710
           GO TO MENU.

      *BUSCA O MODELO POR CODIGO NO ARQUIVO DE DADOS
       BUSCAMODELO.
           PERFORM LIMPATELA
           READ CADMODEL
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   DISPLAY TELAMODELO
                   MOVE CODIGO2 TO CODIGO
                   PERFORM LERMARCA
                   DISPLAY "F1 DELETAR MODELO, F2 VOLTAR" AT 1305
                   ACCEPT AUX AT 2360
                   ACCEPT ACTKEY FROM ESCAPE KEY
                   IF ACTKEY = 01
                       DELETE CADMODEL
                       IF ERRO = "00"
                           PERFORM LIMPAVAR
                           DISPLAY TELAMODELO
                           CLOSE CADMODEL
                           OPEN I-O CADMODEL
                           DISPLAY "MODELO EXCLUIDO" AT 1710
                           GO TO MENU
                       ELSE
                           PERFORM LIMPATELA
                           DISPLAY "ERRO AO REMOVER MODELO" AT 1710
                           GO TO MENU
                       END-IF
                   ELSE
                       PERFORM LIMPATELA
                       GO TO MENU
                   END-IF
               END-IF
           ELSE
               PERFORM LIMPATELA
               DISPLAY "MODELO NAO ENCONTRADO" AT 1710
               GO TO MENU
           END-IF.

      *FAZ BUSCA E LEITURA NO ARQUIVO DE MARCA
       LERMARCA.
           PERFORM LIMPATELA
           MOVE CODIGO2 TO CODIGO
           OPEN INPUT CADMARCA
           READ CADMARCA
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   DISPLAY MARCA AT 0711
                   CLOSE CADMARCA
               ELSE
                   DISPLAY "ERRO AO LER A MARCA" AT 1705
                   CLOSE CADMARCA
                   PERFORM LIMPAVAR
                   GO TO SAIR
               END-IF
           ELSE
               MOVE 1 TO BCMARCA
               DISPLAY "MARCA NAO ENCONTRADA" AT 1605
               CLOSE CADMARCA
           END-IF.

      *LIMPA DADOS DAS VARIAVEIS NA MEMORIA
       LIMPAVAR.
           MOVE ZEROES TO CODIGOM EXISTENCIA
           MOVE SPACES TO MODELO.

      *LIMPA A TELA - PREENCHE COM ESPACOS EM BRANCO
       LIMPATELA.
           DISPLAY "                                       " AT 1202
           DISPLAY "                                       " AT 1241
           DISPLAY "                                       " AT 1302
           DISPLAY "                                       " AT 1341
           DISPLAY "                                       " AT 1402
           DISPLAY "                                       " AT 1441
           DISPLAY "                                       " AT 1502
           DISPLAY "                                       " AT 1541
           DISPLAY "                                       " AT 1602
           DISPLAY "                                       " AT 1641
           DISPLAY "                                       " AT 1702
           DISPLAY "                                       " AT 1741.

      *FIM DO PROGRAMA
       SAIR.
           CLOSE CADMODEL.
           END PROGRAM P172902.
