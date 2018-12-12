      ******************************************************************
      *AUTOR: BRUNO CHAGAS DA SILVA ARMONICO
      *TURMA: ADS 4ฐ SEMESTRE - TARDE - 2017
      ******************************************************************
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
           01 POSICAO  PIC 99 VALUE 5.
           01 ERRO     PIC X(2) VALUE "00".
           01 AUX      PIC X(01) VALUE SPACES.
           01 ACTKEY   PIC 9(02) VALUE ZEROES.
           01 CONLIN   PIC 9(03) VALUE 001.
           01 EXISTENCIA PIC 9(1) VALUE 0.

       01 RELATORIO.
           03 FILLER       PIC X(12) VALUE SPACES.
           03 RECOD        PIC 9(03) VALUE ZEROS.
           03 FILLER       PIC X(07) VALUE SPACES.
           03 REMARCA      PIC X(15) VALUE SPACES.
           03 FILLER       PIC X(10) VALUE SPACES.
           03 REORIGEM     PIC X(15) VALUE SPACES.

       01 LIMPA.
           03 FILLER PIC X(33) VALUE"                                 ".
           03 FILLER PIC X(33) VALUE"                                 ".

           SCREEN SECTION.
       01  TELA.
           05  LINE 01  COLUMN 01
               VALUE  "ษอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 01  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออป".
           05  LINE 02  COLUMN 01
               VALUE  "บ                             RELATORIO".
           05  LINE 02  COLUMN 41
               VALUE  "DE MARCAS                              บ".
           05  LINE 03  COLUMN 01
               VALUE  "ฬอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 03  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออน".
           05  LINE 04  COLUMN 01
               VALUE  "บ".
           05  LINE 04  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 05  COLUMN 01
               VALUE  "บ".
           05  LINE 05  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 06  COLUMN 01
               VALUE  "บ".
           05  LINE 06  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 07  COLUMN 01
               VALUE  "บ".
           05  LINE 07  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 08  COLUMN 01
               VALUE  "บ".
           05  LINE 08  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 09  COLUMN 01
               VALUE  "บ".
           05  LINE 09  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 10  COLUMN 01
               VALUE  "บ".
           05  LINE 10  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 11  COLUMN 01
               VALUE  "บ".
           05  LINE 11  COLUMN 41
               VALUE  "                                       บ".
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
               VALUE  "ฬออออออออออหออออออออออออออออออออออออออออ".
           05  LINE 22  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออน".
           05  LINE 23  COLUMN 01
               VALUE  "บ MENSAGEM:บ".
           05  LINE 23  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 24  COLUMN 01
               VALUE  "ศออออออออออสออออออออออออออออออออออออออออ".
           05  LINE 24  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออผ".

           05 LINE 04 COLUMN 22
               VALUE "CODIGO    MARCA                    ORIGEM".

           PROCEDURE DIVISION.
      *ABRE ARQUIVO DE MARCAS
       ABREARQUIVO.
           OPEN I-O CADMARCA
           IF ERRO NOT = "00"
               IF ERRO = "30" OR ERRO = "35"
                  DISPLAY "ARQUIVO CADCEP NรO EXISTE" AT 2314
                   GO TO ABREARQUIVO
               ELSE
                  DISPLAY "ERRO NA ABERTURA DO ARQUIVO CADCEP" AT 2314
           ELSE
               DISPLAY TELA
               CONTINUE.

      *MENU PRINCIPAL
       INICIABUSCA.
           PERFORM LIMPAVAR.
           DISPLAY "F1 - CONSULTAR CODIGO," AT 2314
           DISPLAY " F2 - CONSULTAR MARCA, F3 - SAIR" AT 2336
           ACCEPT AUX AT 2375
           ACCEPT ACTKEY FROM ESCAPE KEY
           EVALUATE ACTKEY
           WHEN = 01
               GO TO BUSCACODIGO
           WHEN = 02
               GO TO BUSCAMARCA
           WHEN = 03
               GO TO SAIR
           WHEN OTHER
               GO TO INICIABUSCA
           END-EVALUATE.

      *BUSCA MARCA POR NOME
       BUSCAMARCA.
           PERFORM LIMPATELA
           DISPLAY "DIGITE A MARCA:" AT 2314
           ACCEPT MARCA AT 2330
           GO TO BUSCA.
               START CADMARCA KEY IS NOT LESS MARCA INVALID KEY
               DISPLAY "FIM DE PROCESSAMENTO" AT 2314
               GO TO INICIABUSCA.

      *BUSCA MARCA POR CODIGO
       BUSCACODIGO.
           PERFORM LIMPATELA
           DISPLAY "DIGITE O CODIGO:" AT 2314
           ACCEPT CODIGO AT 2331
           GO TO BUSCA.
               START CADMARCA KEY IS NOT LESS CODIGO INVALID KEY
               DISPLAY "FIM DE PROCESSAMENTO" AT 2314
               GO TO INICIABUSCA.

      *PROCURA MARCA NO ARQUIVO
       BUSCA.
           READ CADMARCA NEXT
           IF ERRO NOT = "00"
              IF ERRO = "10"
                 PERFORM LIMPATELA
                 DISPLAY "*** FIM DE PROCESSAMENTO ***" AT 2314
                 GO TO INICIABUSCA
              ELSE
                 PERFORM LIMPATELA
                 DISPLAY ERRO AT 2370
                 DISPLAY "ERRO NA LEITURA DO ARQUIVO" AT 2314
                 GO TO SAIR
               END-IF
           ELSE
               CONTINUE.

      *EXIBE DADOS ENCONTRADOS
       DADOS.
           MOVE REGMARCA TO RECOD
           MOVE MARCA TO REMARCA
           MOVE ORIGEM TO REORIGEM
           DISPLAY RELATORIO AT LINE CONLIN COL 10
           DISPLAY CONLIN AT LINE CONLIN COL 2
           ADD 1 TO CONLIN
           IF CONLIN < 16
               GO TO BUSCA
           ELSE
               CONTINUE.

      *PROXIMA PAGINA OU ENCERRA CONSULTA
       CONTINUA.
           ACCEPT AUX
           ACCEPT ACTKEY FROM ESCAPE KEY
           PERFORM LIMPATELA
           DISPLAY "F1 - CONTINUAR CONSULTA F2 - ENCERRAR" AT 2314
           IF ACTKEY = 01
               MOVE 01 TO CONLIN
               GO TO BUSCA
           IF ACTKEY = 02
               GO TO INICIABUSCA.

       LIMPATELA.
           DISPLAY LIMPA AT 2314.

       LIMPAVAR.
           MOVE ZEROES TO CODIGO ERRO ACTKEY EXISTENCIA
           MOVE SPACES TO MARCA ORIGEM AUX
           MOVE 5 TO POSICAO CONLIN.

       SAIR.
           CLOSE CADMARCA.
