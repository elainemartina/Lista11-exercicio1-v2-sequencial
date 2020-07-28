      $set sourceformat"free"
      *>Divisão de Identificação do Programa
       identification division.
       program-id. "lista11ex1arqseq".
       author. "Elaine Martina Andre".
       installation. "PC".
       date-written. 24/07/2020.
       date-compiled. 24/07/2020.

      *>Divisão Para Configuração do Ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos Recursos Externos
       input-output section.
       file-control.

           select arqTemperaturas assign to "arqTemperaturas.txt" *>  Select - Add o Nome do Arquivo (Arquivo Logico) e...  Assign - Associa o Arquivo Fisico
           organization is line sequential                        *> Organization - Forma de Organização dos Dados
           access mode is sequential                              *> Acess - Como Vou Acessar os Dados
           lock mode is automatic                                 *> Para mais de Um Usuario usar ao Mesmo Tempo - Evita Perda de Dados
           file status is ws-fs-arqTemperaturas.                  *> File Status- O Status da Ultima Operação

       i-o-control.

      *>Declaração de Variáveis
       data division.

      *>----Variaveis de Arquivos
       file section.
       fd arqTemperaturas.
       01 fd-temperaturas.
          05 fd-temp                               pic s9(02)v99.

      *>----Variaveis de Trabalho
       working-storage section.
       77  ws-fs-arqTemperaturas                   pic  9(02).

       01 ws-temperaturas occurs 30.
          05 ws-temp                               pic s9(02)v99.

       77 ws-media-temp                            pic s9(02)v99.
       77 ws-temp-total                            pic s9(03)v99.

       77 ws-dia                                   pic 9(02).
       77 ws-ind-temp                              pic 9(02).

       01 ws-msn-erro.
          05 ws-msn-erro-ofsset                    pic 9(04).
          05 filler                                pic x(01) value "-".
          05 ws-msn-erro-cod                       pic 9(02).
          05 filler                                pic x(01) value space.
          05 ws-msn-erro-text                      pic x(42).

       77 ws-sair                                  pic x(01).


      *>----Variaveis Para Comunicação Entre Programas
       linkage section.


      *>----Declaração de Tela
       screen section.

      *>Declaração do Corpo do Programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  Procedimentos de Inicialização
      *>------------------------------------------------------------------------
       inicializa section.

      *>   Open Input - Abre o Arquivo Para Leitura
           open input arqTemperaturas.
      *>   Tratamento de Erro - Caso File Status Seja Diferente de Zero é Por que Ocorreu Erro ao Abrir o Arquivo
           if  ws-fs-arqTemperaturas <> 0 then
               move 1                                     to ws-msn-erro-ofsset
               move ws-fs-arqTemperaturas                 to ws-msn-erro-cod
               move "Erro ao Abrir Arq. arqTemperaturas " to ws-msn-erro-text
               perform finaliza-anormal
           end-if

      *>   Executar Variando o Indice de Temperatura Ate Que Chegue a 30 Temperaturas ou o File Status Seja Igual a 10 (Fim do Arquivo)
           perform varying ws-ind-temp from 1 by 1 until ws-fs-arqTemperaturas = 10
                                                      or ws-ind-temp > 30
      *>       Ler o Arquivo
               read arqTemperaturas into ws-temp(ws-ind-temp)
      *>       Tratamento de Erro - Caso File Status Seja Diferente de Zero e 10 é Por que Ocorreu Erro ao Ler o Arquivo
               if  ws-fs-arqTemperaturas <> 0
               and ws-fs-arqTemperaturas <> 10  then
                   move 2                                      to ws-msn-erro-ofsset
                   move ws-fs-arqTemperaturas                  to ws-msn-erro-cod
                   move "Erro ao Ler Arq. arqTemperaturas "    to ws-msn-erro-text
                   perform finaliza-anormal
               end-if

           end-perform

      *>   Fechar o Arquivo
           close arqTemperaturas.
      *>   Tratamento de Erro - Caso File Status Seja Diferente de Zero é Por que Ocorreu Erro ao Fechar o Arquivo
           if  ws-fs-arqTemperaturas <> 0 then
               move 3                                      to ws-msn-erro-ofsset
               move ws-fs-arqTemperaturas                  to ws-msn-erro-cod
               move "Erro ao Fechar Arq. arqTemperaturas " to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           .
       inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento Principal
      *>------------------------------------------------------------------------
       processamento section.

      *>   Chamando a Section de Calculo da Média da Temperatura
           perform calculo-media-temp

      *>   Menu do Sistema - Executar Até Que a Opção Sair Seja Igual a "S" ou "s"
           perform until ws-sair = "S"
                      or ws-sair = "s"
      *>       Limpar Tela
               display erase

               display "Dia a Ser Testado: "
               accept ws-dia

      *>       Verificando se o Dia Informado Está Dentro do Intervalo de 1 a 30
               if  ws-dia >= 1
               and ws-dia <= 30 then
      *>           Verificando se o Dia Informado Está Acima, Abaixo ou Está na Média
                   if ws-temp(ws-dia) > ws-media-temp then
                       display "A Temperatura do Dia " ws-dia " Esta Acima da Media"
                   else
                       if ws-temp(ws-dia) < ws-media-temp then
                           display "A Temperatura do Dia " ws-dia " Esta Abaixo da Media"
                       else
                           display "A Temperatura Esta na Media"
                       end-if
                   end-if
               else
                   display "Dia Fora do Intervalo Valido (1 - 30)"
               end-if

               display "'T'estar Outra Temperatura"
               display "'S'air"
               accept ws-sair

           end-perform
           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>                     Calculo da Média de Temperatura
      *>------------------------------------------------------------------------
       calculo-media-temp section.

      *>   Inicializando a Variavel de Temperatura Total
           move 0 to ws-temp-total

      *>   Executar Variando o Indice de Temperatura Ate Que o Indice Seja Maior Que 30
           perform varying ws-ind-temp from 1 by 1 until ws-ind-temp > 30
      *>       Calculando a Temperatura Total
               compute ws-temp-total = ws-temp-total + ws-temp(ws-ind-temp)
           end-perform
      *>   Calculando a Media das Temperaturas
           compute ws-media-temp = ws-temp-total/30

           .
       calculo-media-temp-exit.
           exit.
      *>------------------------------------------------------------------------
      *>                     Finalização  Anormal
      *>------------------------------------------------------------------------
       finaliza-anormal section.

      *>   Caso Finalize de Forma Anormal a Mensagem de Erro Aparecerá
           display erase
           display ws-msn-erro

           stop run
           .
       finaliza-anormal-exit.
           exit.

      *>------------------------------------------------------------------------
      *>                            Finalização
      *>------------------------------------------------------------------------
       finaliza section.

           stop run

           .
       finaliza-exit.
           exit.

