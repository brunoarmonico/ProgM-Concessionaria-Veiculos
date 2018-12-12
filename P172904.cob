      *AUTOR: BRUNO CHAGAS DA SILVA ARMONICO
      *TURMA: ADS 4ฐ SEMESTRE - TARDE - 2017
      *
      *RODAR NO OPEN COBOL IDE (ADMINISTRADOR), COMPILADOR DEFAULT OU MF
      *ATIVAR "RUN IN EXTERNAL TERMINAL" NA GUIA "RUN" DAS
      *PREFERENCIAS (F2) DO OPEN COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P172904.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADVEIC ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS VEICULO
               FILE STATUS  IS ERRO
               ALTERNATE RECORD KEY IS V-ANO WITH DUPLICATES.
           SELECT CADPROPR ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CPF
               FILE STATUS  IS ERRO
               ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.
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
           FD CADVEIC
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADVEIC.DAT".
           01 REGVEIC.
               02 VEICULO.
                   03 V-MARCA          PIC 9(3) VALUE ZEROES.
                   03 V-MODELO         PIC 9(3) VALUE ZEROES.
                   03 P-CPF            PIC X(11) VALUE SPACES.
               02 V-ANO VALUE ZEROES.
                   03 ANOFABRIC        PIC 9(4).
                   03 ANOMODELO        PIC 9(4).
               02 COR                  PIC 99 VALUE ZEROES.
               02 VENDA VALUE ZEROES.
                   03 VALORVENDA       PIC 9(7)V99.
                   03 COMISSAO         PIC 9(2)V99.

           FD CADPROPR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPROPR.DAT".
           01 REGPROPR.
               02 CPF          PIC X(11) VALUE ZEROES.
               02 NOME         PIC X(30) VALUE SPACES.
               02 DATANASC.
                   03 DIA      PIC 9(2) VALUE ZEROES.
                   03 MES      PIC 9(2) VALUE ZEROES.
                   03 ANO      PIC 9(4) VALUE ZEROES.
               02 BCEP         PIC 9(8) VALUE ZEROES.
               02 NUM          PIC 9(6) VALUE ZEROES.
               02 COMPLEMENTO  PIC X(25) VALUE SPACES.
               02 TELEFONE.
                   03 DDD      PIC 9(2) VALUE ZEROES.
                   03 NUMERO   PIC 9(8) VALUE ZEROES.
               02 EMAIL        PIC X(30) VALUE SPACES.

           FD CADMODEL
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMODEL.DAT".
           01 REGMODEL.
               02 CODIGOM.
                   03 CODIGO1  PIC 9(3) VALUE ZEROES.
                   03 CODIGO2  PIC 9(3) VALUE ZEROES.
               02 MODELO       PIC X(20) VALUE SPACES.

           FD CADMARCA
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMARCA.DAT".
           01 REGMARCA.
               02 CODIGO PIC 9(3).
               02 MARCA  PIC X(20).

           WORKING-STORAGE SECTION.
           01 BSMARCA PIC 9(1) VALUE 0.
           01 BSMODELO PIC 9(1) VALUE 0.
           01 BSCPF PIC 9(1) VALUE 0.
           01 ERRO PIC X(2) VALUE "00".
           01 AUX PIC X(01) VALUE SPACES.
           01 ACTKEY PIC 9(02) VALUE ZEROES.
           01 EXISTENCIA PIC 9(1) VALUE 0.
           01 VALORPAGAMENTO   PIC 9(7)V99.
           01 VALORCOMISSAO    PIC 9(7)V99.
           01 PORCENTAGEM    PIC 9(7)V99.
           01 CORES.
               03 PIC X(10) VALUE "PRETO".
               03 PIC X(10) VALUE "PRATA".
               03 PIC X(10) VALUE "CINZA".
               03 PIC X(10) VALUE "BRANCO".
               03 PIC X(10) VALUE "AZUL".
               03 PIC X(10) VALUE "VERDE".
               03 PIC X(10) VALUE "VERMELHO".
               03 PIC X(10) VALUE "LARANJA".
               03 PIC X(10) VALUE "DOURADO".
               03 PIC X(10) VALUE "TURQUESA".
               03 PIC X(10) VALUE "VERDE LIMA".
               03 PIC X(10) VALUE "INDIGO".
           01 BUSCACOR REDEFINES CORES.
               02 BUSCA PIC X(10) OCCURS 12 TIMES.

           SCREEN SECTION.
       01  TELAVEICULO.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "ษอออออออออออออออออออออออออออออCADASTRO D".
           05  LINE 02  COLUMN 41
               VALUE  "E VEICULOออออออออออออออออออออออออออออออป".
           05  LINE 03  COLUMN 01
               VALUE  "บ".
           05  LINE 03  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 04  COLUMN 01
               VALUE  "บ MARCA:".
           05  LINE 04  COLUMN 41
               VALUE  "MODELO:                                บ".
           05  LINE 05  COLUMN 01
               VALUE  "บฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ".
           05  LINE 05  COLUMN 41
               VALUE  "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤบ".
           05  LINE 06  COLUMN 01
               VALUE  "บ CPF PROPRIETARIO:             NOME:".
           05  LINE 06  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 07  COLUMN 01
               VALUE  "บ".
           05  LINE 07  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 08  COLUMN 01
               VALUE  "บ E-MAIL:".
           05  LINE 08  COLUMN 41
               VALUE  " TELEFONE:(  )    -                    บ".
           05  LINE 09  COLUMN 01
               VALUE  "บฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ".
           05  LINE 09  COLUMN 41
               VALUE  "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤบ".
           05  LINE 10  COLUMN 01
               VALUE  "บ ANO DE FABRICACAO:                  AN".
           05  LINE 10  COLUMN 41
               VALUE  "O DO MODELO:                           บ".
           05  LINE 11  COLUMN 01
               VALUE  "บ".
           05  LINE 11  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 12  COLUMN 01
               VALUE  "บ COR:".
           05  LINE 12  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 13  COLUMN 01
               VALUE  "บฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ".
           05  LINE 13  COLUMN 41
               VALUE  "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤบ".
           05  LINE 14  COLUMN 01
               VALUE  "บ VALOR DE VENDA:                     CO".
           05  LINE 14  COLUMN 41
               VALUE  "MISSAO:                                บ".
           05  LINE 15  COLUMN 01
               VALUE  "บ".
           05  LINE 15  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 16  COLUMN 01
               VALUE  "บ VALOR DE PAGAMENTO:                 VA".
           05  LINE 16  COLUMN 41
               VALUE  "LOR DA COMISSAO:                       บ".
           05  LINE 17  COLUMN 01
               VALUE  "ฬอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 17  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออน".
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
           05  TV-MARCA
               LINE 04  COLUMN 09  PIC 9(03)
               USING  V-MARCA
               HIGHLIGHT     BLANK ZERO.
           05  TV-MODELO
               LINE 04  COLUMN 48  PIC 9(03)
               USING  V-MODELO
               HIGHLIGHT     BLANK ZERO.
           05  TP-CPF
               LINE 06  COLUMN 20  PIC Z(11)
               USING  P-CPF
               HIGHLIGHT.
           05  T-NOME
               LINE 06  COLUMN 38  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  T-MAIL
               LINE 08  COLUMN 10  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  T-TELEFONE.
               06 DDDTEL
                   LINE 08  COLUMN 52  PIC 9(2)
                   USING  DDD
                   HIGHLIGHT     BLANK ZERO.
               06 TELFONE
                   LINE 08  COLUMN 55  PIC 9(8)
                   USING  NUMERO
                   HIGHLIGHT     BLANK ZERO.
           05  T-AFAB
               LINE 10  COLUMN 21  PIC 9(04)
               USING  ANOFABRIC
               HIGHLIGHT     BLANK ZERO.
           05  T-AMODEL
               LINE 10  COLUMN 53  PIC 9(04)
               USING  ANOMODELO
               HIGHLIGHT     BLANK ZERO.
           05  T-COR
               LINE 12  COLUMN 07  PIC 9(02)
               USING  COR
               HIGHLIGHT     BLANK ZERO.
           05  T-VVENDA
               LINE 14  COLUMN 18  PIC Z(7),99
               USING  VALORVENDA
               HIGHLIGHT.
           05  T-COMISSAO
               LINE 14  COLUMN 48  PIC ZZ,99
               USING  COMISSAO
               HIGHLIGHT.
           05  T-VPAG
               LINE 16  COLUMN 22  PIC Z(7),99
               USING  VALORPAGAMENTO
               HIGHLIGHT.
           05  T-VCOMISSAO
               LINE 16  COLUMN 57  PIC Z(7),99
               USING  VALORCOMISSAO
               HIGHLIGHT.

      *TELA DE SELECAO DE COR DO VEICULO
       01  TELACOR.
           05  LINE 17  COLUMN 01
               VALUE  "ฬอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 17  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออน".
           05  LINE 18  COLUMN 01
               VALUE  "บ".
           05  LINE 18  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 19  COLUMN 01
               VALUE  "บฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤ".
           05  LINE 19  COLUMN 41
               VALUE  "ฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤบ".
           05  LINE 20  COLUMN 01
               VALUE  "บ 1 - PRETO              ณ 5 - AZUL".
           05  LINE 20  COLUMN 41
               VALUE  "           ณ 9 - DOURADO               บ".
           05  LINE 21  COLUMN 01
               VALUE  "บ 2 - PRATA              ณ 6 - VERDE".
           05  LINE 21  COLUMN 41
               VALUE  "           ณ 10 - TURQUESA             บ".
           05  LINE 22  COLUMN 01
               VALUE  "บ 3 - CINZA              ณ 7 - VERMELHO".
           05  LINE 22  COLUMN 41
               VALUE  "           ณ 11 - VERDE LIMA           บ".
           05  LINE 23  COLUMN 01
               VALUE  "บ 4 - BRANCO             ณ 8 - LARANJA".
           05  LINE 23  COLUMN 41
               VALUE  "           ณ 12 - INDIGO               บ".
           05  LINE 24  COLUMN 01
               VALUE  "ศอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 24  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออผ".


           PROCEDURE DIVISION.
      *ABRE ARQUIVO DE DADOS DO VEICULO
       ABREARQUIVO.
           OPEN I-O CADVEIC
           IF ERRO NOT = "00"
               IF ERRO = "30" OR ERRO = "35"
                   OPEN OUTPUT CADVEIC
                   CLOSE CADVEIC
                  DISPLAY "ARQUIVO CADVEIC FOI CRIADO" AT 0622
                   GO TO ABREARQUIVO
               ELSE
                  DISPLAY "ERRO NA ABERTURA DO ARQUIVO CADCEP" AT 0622
           ELSE
               DISPLAY TELAVEICULO
               CONTINUE.

      *MENU PRINCIPAL
       MENU.
           PERFORM LIMPAVAR
           DISPLAY "F1 CADASTRAR, F2 PROCURAR, F3 SAIR" AT 1805
           ACCEPT AUX AT 0101 BACKGROUND-COLOR 12
           ACCEPT ACTKEY FROM ESCAPE KEY
           EVALUATE ACTKEY
           WHEN = 01
               PERFORM LIMPATELA
               DISPLAY TELAVEICULO
               GO TO R-MARCA
           WHEN = 02
               DISPLAY TELAVEICULO
               PERFORM LIMPATELA
               DISPLAY "MARCA: " AT 1805
               ACCEPT V-MARCA AT 1815
               DISPLAY "MODELO: " AT 1905
               ACCEPT V-MODELO AT 1915
               DISPLAY "CPF: " AT 2005
               ACCEPT P-CPF AT 2015
               PERFORM BUSCAVEICULO
           WHEN = 03
               GO TO SAIR
           WHEN OTHER
           DISPLAY "OPCAO INVALIDA" AT 1905
               GO TO MENU
           END-EVALUATE.

      *RECEBE CODIGO DA MARCA DO VEICULO
       R-MARCA.
           MOVE 0 TO BSMARCA
           ACCEPT TV-MARCA
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               PERFORM LIMPAVAR
               DISPLAY "                    " AT 0452
               DISPLAY TELAVEICULO
               GO TO MENU
           ELSE IF V-MARCA = ZEROES
               DISPLAY "CODIGO INVALIDO" AT 2005
               PERFORM LIMPAVAR
               GO TO R-MARCA
           ELSE
               PERFORM LERMARCA
               IF BSMARCA = 1
                   GO TO R-MARCA
               ELSE
                   PERFORM LIMPATELA
               CONTINUE.

      *RECEBE CODIGO DO MODELO DO VEICULO
       R-MODELO.
           MOVE 0 TO BSMODELO
           ACCEPT TV-MODELO
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               DISPLAY "                    " AT 0413
               GO TO R-MARCA
           ELSE IF V-MODELO = ZEROES
               DISPLAY "CODIGO INVALIDO" AT 2005
               GO TO R-MODELO
           ELSE
               PERFORM LERMODELO
               IF BSMODELO = 1
                   GO TO R-MODELO
               ELSE
                   PERFORM LIMPATELA
               CONTINUE.

      *RECEBE CPF DO PROPRIETARIO
       R-CPF.
           MOVE 0 TO BSCPF
           ACCEPT TP-CPF
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               GO TO R-MODELO
           ELSE IF P-CPF = SPACES
               DISPLAY "CPF INVALIDO" AT 2005
               GO TO R-CPF
           ELSE
               PERFORM LERCPF
               IF BSCPF = 1
                   GO TO R-CPF
               ELSE
                   PERFORM LIMPATELA
                   PERFORM LERVEICULO
               CONTINUE.

      *RECEBE ANO DE FABRICACAO DO VEICULO
       R-AFAB.
           ACCEPT T-AFAB
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               GO TO R-CPF
           ELSE IF ANOFABRIC < 1950 OR ANOFABRIC > 2018
               DISPLAY "ANO DE FABRICACAO INVALIDO" AT 1805
               GO TO R-AFAB
           ELSE
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE ANO DO MODELO DO VEICULO
       R-AMODEL.
           ACCEPT T-AMODEL
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               GO TO R-AFAB
           ELSE IF ANOMODELO < 1950 OR ANOMODELO > 2018
               DISPLAY "ANO DO MODELO INVALIDO" AT 1805
               GO TO R-AMODEL
           ELSE
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE COR DO VEICULO
       R-COR.
           DISPLAY TELACOR
           ACCEPT T-COR
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               DISPLAY "            " AT 1211
               GO TO R-AMODEL
           ELSE IF COR = ZEROES OR COR > 12
               DISPLAY "COR INVALIDA" AT 1805
               GO TO R-COR
           ELSE
               DISPLAY BUSCA(COR) AT 1211
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE VALOR DE VENDA DO VEICULO
       R-VVENDA.
           ACCEPT T-VVENDA
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               GO TO R-COR
           ELSE IF VALORVENDA = ZEROES
               DISPLAY "VALOR INVALIDO" AT 1805
               GO TO R-VVENDA
           ELSE
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE PORCENTAGEM DE COMISSAO PELA VENDA
       R-COMISSAO.
           ACCEPT T-COMISSAO
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               GO TO R-VVENDA
           ELSE IF COMISSAO = ZEROES OR COMISSAO > 100
               DISPLAY "VALOR INVALIDO" AT 1805
               GO TO R-COMISSAO
           ELSE
               PERFORM LIMPATELA
               PERFORM R-VALORVC
               GO TO VERIFICAOPC.

      *CALCULA E EXIBE VALOR DE COMISSAO E PAGAMENTO
       R-VALORVC.
           COMPUTE VALORCOMISSAO = VALORVENDA * (COMISSAO / 100).
           COMPUTE VALORPAGAMENTO = VALORVENDA - VALORCOMISSAO.
           DISPLAY T-VPAG
           DISPLAY T-VCOMISSAO.

      *VERIFICA EXISTENCIA DA VENDA NO ARQUIVO
       VERIFICAOPC.
           IF EXISTENCIA = 0
               GO TO GRAVAVEICULO
           ELSE
               GO TO ALTERACHAVE
           END-IF
               CONTINUE.

      *FAZ LEITURA DE DADOS DO VEICULO
       LERVEICULO.
           READ CADVEIC
              IF ERRO NOT = "23"
                 IF ERRO = "00"
                   PERFORM LIMPATELA
                   DISPLAY TELAVEICULO
                   PERFORM LERMARCA
                   PERFORM LERMODELO
                   PERFORM LERCPF
                   PERFORM R-VALORVC
                   DISPLAY "VEICULO JA CADASTRADO" AT 2010
                   DISPLAY "F1 ALTERAR, F2 CANCELAR" AT 2110
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
                   DISPLAY "ERRO NA LEITURA ARQUIVO CADVEIC" AT 2210
                   GO TO SAIR
                 END-IF
              END-IF
           CONTINUE.

      *GRAVA DADOS DO VEICULO NO ARQUIVO
       GRAVAVEICULO.
           PERFORM LIMPATELA
           DISPLAY "GRAVAR DADOS?" AT 2005
           DISPLAY "F1 - SIM, F2 - NAO" AT 2105
           ACCEPT AUX AT 0101
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               WRITE REGVEIC
               IF ERRO = "00" OR "02"
                   PERFORM LIMPATELA
                   CLOSE CADVEIC
                   OPEN I-O CADVEIC
                   DISPLAY "DADOS GRAVADOS" AT 2210
                   GO TO MENU
               ELSE IF ERRO = "22"
                   DISPLAY "CADVEIC JA EXISTE " AT 2210
                   GO TO MENU
               ELSE
                  DISPLAY "ERRO NA GRAVACAO DO ARQUIVO CADVEIC" AT 2210
                   GO TO MENU
               END-IF
           ELSE
               PERFORM LIMPAVAR
               PERFORM LIMPATELA
           GO TO MENU.

      *ALTERA DADOS DO VEICULO NO ARQUIVO
       ALTERACHAVE.
           REWRITE REGVEIC
           IF ERRO = "00" OR "02"
               CLOSE CADVEIC
               OPEN I-O CADVEIC
               DISPLAY "INFORMACOES DE VEICULO ALTERADAS" AT 2010
               GO TO MENU
           ELSE
               DISPLAY "ERRO AO ALTERAR DADOS DO VEICULO" AT 2010
           GO TO MENU.

      *BUSCA DADOS DO VEICULO NO ARQUIVO
       BUSCAVEICULO.
           READ CADVEIC
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   PERFORM LIMPATELA
                   DISPLAY TELAVEICULO
                   PERFORM LERCPF
                   PERFORM LERMARCA
                   PERFORM LERMODELO
                   PERFORM R-VALORVC
                   DISPLAY "F1 DELETAR VEICULO, F2 VOLTAR" AT 1805
                   ACCEPT AUX AT 0101
                   ACCEPT ACTKEY FROM ESCAPE KEY
                   IF ACTKEY = 01
                       DELETE CADVEIC
                       IF ERRO = "00"
                           PERFORM LIMPATELA
                           CLOSE CADVEIC
                           OPEN I-O CADVEIC
                           DISPLAY "DADOS DO VEICULO EXCLUIDOS" AT 2010
                           GO TO MENU
                       ELSE
                           PERFORM LIMPATELA
                          DISPLAY "ERRO AO REMOVER VEICULO" AT 2010
                           GO TO MENU
                       END-IF
                   ELSE
                       PERFORM LIMPATELA
                       GO TO MENU
                   END-IF
               END-IF
           ELSE
               PERFORM LIMPATELA
               DISPLAY "DADOS DO VEICULO NAO ENCONTRADO" AT 2010
               GO TO MENU
           END-IF.

      *BUSCA DADOS DA MARCA POR CODIGO
       LERMARCA.
           MOVE V-MARCA TO CODIGO
           OPEN INPUT CADMARCA
           READ CADMARCA
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   DISPLAY MARCA AT 0413
                   CLOSE CADMARCA
               ELSE
                   DISPLAY "ERRO AO LER A MARCA" AT 1905
                   PERFORM LIMPAVAR
                   CLOSE CADMARCA
                   GO TO MENU
               END-IF
           ELSE
               MOVE 1 TO BSMARCA
               DISPLAY "MARCA NAO ENCONTRADA" AT 1905
               CLOSE CADMARCA
           END-IF.

      *BUSCA DADOS DO MODELO POR CODIGO
       LERMODELO.
           MOVE V-MARCA TO CODIGO1
           MOVE V-MODELO TO CODIGO2
           OPEN INPUT CADMODEL
           READ CADMODEL
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   DISPLAY MODELO AT 0452
                   CLOSE CADMODEL
               ELSE
                   DISPLAY "ERRO AO LER O MODELO" AT 2005
                   PERFORM LIMPAVAR
                   CLOSE CADMODEL
                   GO TO MENU
               END-IF
           ELSE
               MOVE 1 TO BSMODELO
               DISPLAY "MODELO NAO ENCONTRADO" AT 2005
               CLOSE CADMODEL
           END-IF.

      *BUSCA DADOS DO PROPRIETARIO POR CPF
       LERCPF.
           MOVE P-CPF TO CPF
           OPEN INPUT CADPROPR
           READ CADPROPR
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   DISPLAY T-NOME
                   DISPLAY T-MAIL
                   DISPLAY T-TELEFONE
                   CLOSE CADPROPR
               ELSE
                   DISPLAY "ERRO AO LER CPF" AT 2105
                   PERFORM LIMPAVAR
                   CLOSE CADPROPR
                   GO TO MENU
               END-IF
           ELSE
               MOVE 1 TO BSCPF
               DISPLAY "CPF NAO ENCONTRADO" AT 2105
               CLOSE CADPROPR
           END-IF.


      *LIMPA DADOS NAS VARIAVEIS
       LIMPAVAR.
           MOVE ZEROES TO V-MODELO V-MARCA COR VENDA V-ANO P-CPF BSCPF
           MOVE ZEROES TO BSMARCA BSMODELO EXISTENCIA ACTKEY TELEFONE
           MOVE ZEROES TO VALORPAGAMENTO VALORCOMISSAO
           MOVE SPACES TO ERRO AUX EMAIL NOME.

      *LIMPA A TELA - PREENCHE COM ESPACOS EM BRANCO
       LIMPATELA.
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
           CLOSE CADVEIC.
           END PROGRAM P172904.
