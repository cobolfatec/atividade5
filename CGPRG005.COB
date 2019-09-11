       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG005.
      *AUTHOR.       ALVARO PEREIRA DO NASCIMENTO.
      *DATE-WRITTEN. 10/09/2019.
      *--------------------------------------------------------------*
      * DISCIPLINA PROGRAMACAO MAINFRAME
      *--------------------------------------------------------------*
      * OBJETIVO: RECEBER DADOS DA SYSIN(ACCEPT)
      *           CALCULAR A MEDIA ARITMETICA BIMESTRAL
      *--------------------------------------------------------------*
      *------------------> HISTORICO - MANUTENCAO <------------------*
      * VERSAO  MES/ANO  NR.DOC  IDENT.  DESCRICAO
      * ------  -------  ------  ------  -------------------------   *
      *  V01    FEV/2013 010001  SISTEMA MOSTRA SYSOUT
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *====================*
       CONFIGURATION SECTION.
      *---------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           CURRENCY SIGN IS "R$ " WITH PICTURE SYMBOL "$"
           .
       INPUT-OUTPUT SECTION.
      *---------------------*
       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
       WORKING-STORAGE SECTION.
      *-----------------------*
       01  FILLER                 PIC X(35)        VALUE
           '**** INICIO DA WORKING-STORAGE ****'.

      *-----> VARIAVEIS AUXILIARES UTILIZADA NO PROCESSAMENTO
       01  WS-AREA-AUX.
           05  WS-FIM                 PIC X(01).
           05  WS-CTLIDO              PIC 9(02).
           05  WS-MEDIA               PIC 9(02)V99.
       77  AS-MEDIA                   PIC 9(02)V99.
       77  WS-SEXF                    PIC 99.
       77  WS-SEXM                    PIC 99.
       77  AS-MEDG                    PIC 9999V99.
       77  WS-MEDG                    PIC ZZZ9,99.
       77  WS-MSEIS                   PIC ZZZ9.
       77  AS-MSEIS                   PIC 9999.
       77  WS-MMED                    PIC ZZZ9,99.
       77  AS-MMED                    PIC 9999V99.
      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-NUMERO-IN        PIC 9(04).
           05 WS-NOME-IN          PIC X(20).
           05 WS-SEXO-IN          PIC X(01).
           05 WS-IDADE-IN         PIC 9(02).
           05 WS-CURSO-IN         PIC X(12).
           05 WS-NOTA1-IN         PIC 9(02)V99.
           05 WS-NOTA2-IN         PIC 9(02)V99.

       01  FILLER                 PIC X(35)        VALUE  SPACES.
      *----> SAIDA DE DADOS VIA SYSOUT
       01  WS-REGSYSOUT.
           05 WS-NUM              PIC 9(04).
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-NOME             PIC X(20).
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-SEX              PIC X(01).
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-IDA              PIC Z9.
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-CUR              PIC X(12).
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-NT1              PIC Z9,99.
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-NT2              PIC Z9,99.
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-MED              PIC Z9,99.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-RSPRG002.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    LEITURA DADOS DA SYSIN
      *--------------------------------------------------------------*
       025-LER-SYSIN.

           ACCEPT WS-REG-SYSIN  FROM SYSIN

           IF WS-REG-SYSIN = ALL '9'
              MOVE   'S'     TO  WS-FIM
           ELSE
              ADD 1  TO WS-CTLIDO
           MOVE  WS-NUMERO-IN  TO WS-NUM
           MOVE  WS-NOME-IN    TO WS-NOME
           MOVE  WS-SEXO-IN    TO WS-SEX
           MOVE  WS-IDADE-IN   TO WS-IDA
           MOVE  WS-CURSO-IN   TO WS-CUR
           MOVE  WS-NOTA1-IN   TO WS-NT1
           MOVE  WS-NOTA2-IN   TO WS-NT2
           MOVE  AS-MEDIA      TO WS-MED
      *--->   CONDICAO PARA DETERMINAR O SEXO                          *
             IF WS-SEXO-IN  = 'F'
               ADD 1  TO WS-SEXF
             ELSE
               ADD 1 TO  WS-SEXM
           END-IF
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN ATE FIM DOS REGISTROS
      *--------------------------------------------------------------*
       030-PROCESSAR.

           COMPUTE AS-MEDIA = (WS-NOTA1-IN + WS-NOTA2-IN) / 2
      *----> CALCULO DA MEDIA GERAL DOS ALUNOS
           ADD     AS-MEDIA TO AS-MEDG
      *----> CONDICAO PARA ALUNOS ABAIXO DE SEIS
           IF  AS-MEDIA < 6
             ADD 1 TO AS-MSEIS
           END-IF
           DISPLAY WS-REGSYSOUT

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           COMPUTE AS-MEDG = AS-MEDG / WS-CTLIDO
           COMPUTE AS-MMED = (AS-MSEIS * 100 ) / WS-CTLIDO
           MOVE AS-MEDG  TO WS-MEDG
           MOVE AS-MSEIS TO WS-MSEIS
           MOVE AS-MMED  TO WS-MMED
           DISPLAY ' *========================================*'
           DISPLAY ' *   TOTAIS DE CONTROLE - CGPRG005        *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * REGISTROS LIDOS = ' WS-CTLIDO
           DISPLAY ' * TOTAL MULHERES  = ' WS-SEXF
           DISPLAY ' * TOTAL HOMENS    = ' WS-SEXM
           DISPLAY ' * MEDIA GERAL DOS ALUNOS = ' WS-MEDG
           DISPLAY ' * TOTAL DE ALUNOS ABAIXO DA MEDIA = ' WS-MSEIS
           DISPLAY ' * PROCENTAGEM DE ALUNOS ABAIXO DA MEDIA = ' WS-MMED
           DISPLAY ' *========================================*'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *      TERMINO NORMAL DO CGPRG005        *'
           DISPLAY ' *----------------------------------------*'
           .
      *---------------> FIM DO PROGRAMA RSPRG002 <-------------------*
