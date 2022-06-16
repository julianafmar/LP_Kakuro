%99261 Juliana Fernandes Marcelino

:- [codigo_comum].

% 3.1.1

% N corresponde a um numero inteiro, Els a uma lista de inteiros
% e Soma um inteiro. Assim, Combs sera uma lista ordenada cujos
% elementos sao combinacoes N a N dos elementos de Els cuja soma e Soma.

combinacoes_soma(N, Els, Soma, Combs) :-
    findall(Y, combinacao(N, Els, Y), Lst),
    findall(L, (member(L, Lst), soma_elementos(L, S), S =:= Soma), Combs).

% Recebe uma lista R de numeros inteiros. S e a soma de todos os
% elementos da lista.

soma_elementos([], 0).
soma_elementos([Y|R], S) :-
    soma_elementos(R, NS),
    S is NS + Y.

% 3.1.2

% N e um inteiro, Els e uma lista de inteiros e Soma e um
% inteiro. Perms correspode a uma lista ordenada cujos elementos sao as
% permutacoes das combinacoes N a N dos elementos de Els cuja soma e
% Soma.

permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(L, (member(X, Combs), permutation(X, L)), Lst),
    sort(Lst, Perms).

%3.1.3

% Fila e uma linha ou coluna de um puzzle, sendo H_V o atomo h ou v que
% identifica se a fila e horizontal ou vertical, respetivamente. Esp
% sera um espaco da fila.

espaco_fila(Fila, Esp, H_V) :-
    H_V == h,
    findall(S, (member(X, Fila), is_list(X), nth1(2, X, S), S =\= 0), Somas),
    espaco_fila_aux2(Fila, Espacos),
    espaco_fila_aux3(Somas, Espacos, L),
    member(T, L),
    Esp = T.
espaco_fila(Fila, Esp, H_V) :-
    H_V == v,
    findall(S, (member(X, Fila), is_list(X), nth1(1, X, S), S =\= 0), Somas),
    espaco_fila_aux2(Fila, Espacos),
    espaco_fila_aux3(Somas, Espacos, L),
    member(T, L),
    Esp = T.

% tira_lista recebe uma lista e se houver listas no inicio da mesma,
% elas sao retiradas.

tira_lista([X|R], [X|R]) :-
    var(X), !.
tira_lista([X|R], P) :-
    is_list(X),
    tira_lista(R, P).

espaco_fila_aux([], [], []).
espaco_fila_aux([Z|Res], [], Res) :-
    \+ var(Z).
espaco_fila_aux([Z|Res], [Z | Y], P) :-
    var(Z),
    espaco_fila_aux(Res, Y, P).

espaco_fila_aux2([], []).
espaco_fila_aux2(Fila, [Esp1|_]) :-
    tira_lista(Fila, Aux),
    espaco_fila_aux(Aux, Esp1, LisAux),
    length(LisAux, N),
    N == 1,
    nth1(1, LisAux, L),
    is_list(L),
    !.
espaco_fila_aux2(Fila, [Esp1|Esp]) :-
    tira_lista(Fila, Aux),
    espaco_fila_aux(Aux, Esp1, LisAux),
    espaco_fila_aux2(LisAux, Esp).

espaco_fila_aux3([], [], []).
espaco_fila_aux3([X|S], [Y|E], [espaco(X, Y)| Lis]) :-
    espaco_fila_aux3(S, E, Lis).

% 3.1.4

% Fila e uma linha ou coluna e H_V corresponde ao atomo h ou v que
% identifica se a fila e horizontal e vertical. Espacos sera a lista de
% todos os espacos da Fila.

espacos_fila(_, Fila, Espacos) :-
    length(Fila, N1),
    findall(X, (member(X, Fila), is_list(X)), Lis),
    length(Lis, N2),
    N1 == N2,
    Espacos = [], !.
espacos_fila(H_V, Fila, Espacos) :-
    bagof(X, espaco_fila(Fila, X, H_V), Aux),
    Espacos = Aux.

%3.1.5

% Puzzle e um puzzle e espacos sera a lista de espacos correspondente a
% esse mesmo puzzle.

espacos_puzzle(Puzzle, Espacos) :-
    mat_transposta(Puzzle, Transp),
    bagof(EspH, FilaH^EspsH^(member(FilaH, Puzzle), espacos_fila(h, FilaH, EspsH), member(EspH, EspsH)), AuxH),
    bagof(EspV, FilaV^EspsV^(member(FilaV, Transp), espacos_fila(v, FilaV, EspsV), member(EspV, EspsV)), AuxV),
    append(AuxH, AuxV, AuxFinal),
    bagof(Y, (member(Y, AuxFinal)), Espacos).

% 3.1.6

% Espacos e uma lista de espacos e Esp e um espaco. Esps_com e a lista
% de espacos com variaveis em comum com Esp (exceto Esp).

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    bagof(E, (member(E, Espacos), E \= Esp), N_Espacos),
    espaco_elementos(Esp, _, L),
    espacos_com_posicoes_comuns_aux(N_Espacos, L, Esps_com), !.

espacos_com_posicoes_comuns_aux([], _, []).
espacos_com_posicoes_comuns_aux([E|Espacos], L, [E|Res]) :-
    espaco_elementos(E, _, LE),
    pertence(L, LE),
    espacos_com_posicoes_comuns_aux(Espacos, L, Res).
espacos_com_posicoes_comuns_aux([E|Espacos], L, Res) :-
    espaco_elementos(E, _, LE),
    \+ pertence(L, LE),
    espacos_com_posicoes_comuns_aux(Espacos, L, Res).

% Devolve true caso pelo menos um elemento da lista L pertenca a lista
% LE e false caso contrario.

pertence([], []).
pertence([El|_], LE) :-
    membro(El, LE), !.
pertence([El|L], LE) :-
    \+ membro(El, LE),
    pertence(L, LE).

% Devolve true caso o elemento El seja membro da lista L e false caso
% contrario.

membro(_, []) :- fail.
membro(El, [El2|_]) :-
    El == El2, !.
membro(El, [El2|L]) :-
    El \== El2,
    membro(El, L).

% 3.1.7

% Espacos e uma lista de espacos e Perms_soma e a lista de 2 elementos
% em que o primeiro elemento e um espaco de Espacos e o segundo a lista
% ordenada de permutacoes cuja some e igual a soma do espaco.

permutacoes_soma_espacos(Espacos, Perms_soma) :-
    findall(Perms, (member(E, Espacos), espaco_elementos(E, S, L), length(L, N), permutacoes_soma(N, [1, 2, 3, 4, 5, 6, 7, 8, 9], S, Perms)), Aux),
    permutacoes_soma_espacos_aux(Espacos, Aux, Perms_soma).

permutacoes_soma_espacos_aux([], [], []).
permutacoes_soma_espacos_aux([X|Esp], [Y|Perm], [[X, Y] | Lista]) :-
    permutacoes_soma_espacos_aux(Esp, Perm, Lista).

% Identifica os elementos Soma e Lista do espaco dado.

espaco_elementos(espaco(S, L), Soma, Lista) :-
    Lista = L,
    Soma = S.

% 3.1.8

% Perm e uma permutacao, Esp e um espaco, Espacos e uma lista de espacos
% e Perms_soma e uma lista de listas. Assim, Perm e uma permutacao
% possivel para o espaco Esp.

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
    findall(Esp_com_perm, (member(Esp_com, Esps_com), espaco_permutacoes(Perms_soma, Esp_com, Esp_com_perm)), Esps_com_perm),
    espaco_permutacoes(Perms_soma, Esp, Esp_perm),
    espaco_comum(Esp, Esps_com, EspLN),
    nth1(2, Esp_perm, EspP),
    verifica_perms_in(EspP, Esps_com_perm, EspLN, Aux),
    member(Perm, Aux).

espaco_permutacoes([], _, []).
espaco_permutacoes([EspP|_], Esp2, EspP) :-
    nth1(1, EspP, Esp1),
    Esp1 == Esp2.
espaco_permutacoes([EspP|Perms_soma], Esp2, EspF) :-
    nth1(1, EspP, Esp),
    Esp \= Esp2,
    espaco_permutacoes(Perms_soma, Esp2, EspF).

espaco_comum(_, [], []).
espaco_comum(Esp, [Esp2|Esps_com], [Res|Indices]) :-
    espaco_elementos(Esp, _, L1),
    espaco_elementos(Esp2, _, L2),
    espaco_comum_aux(L1, L2, 1, Res2),
    espaco_comum_aux(L2, L1, 1, Res1),
    append(Res2, Res1, Res),
    espaco_comum(Esp, Esps_com, Indices).

espaco_comum_aux([], [], _, _).
espaco_comum_aux([El|_], L2, Cont, [Cont]) :-
    membro(El, L2).
espaco_comum_aux([El|L1], L2, Cont, Res) :-
    \+ membro(El, L2),
    NCont is Cont + 1,
    espaco_comum_aux(L1, L2, NCont, Res).

verifica_perms_in([], _, _, []).
verifica_perms_in([EP|EspP], Esps_com_perm, EspLN, [EP|P]) :-
    \+ verifica_perms(EP, Esps_com_perm, EspLN),
    verifica_perms_in(EspP, Esps_com_perm, EspLN, P).
verifica_perms_in([EP|EspP], Esps_com_perm, EspLN, P) :-
    verifica_perms(EP, Esps_com_perm, EspLN),
    verifica_perms_in(EspP, Esps_com_perm, EspLN, P).

verifica_perms([], [], []).
verifica_perms(EP, [Esp_com|Esps_com_perm], [E_LN|EspLN]) :-
    nth1(2, Esp_com, EspCP),
    verifica_perms_aux(EP, EspCP, E_LN),
    verifica_perms(EP, Esps_com_perm, EspLN).
verifica_perms(EP, [Esp_com|_], [E_LN|_]) :-
    nth1(2, Esp_com, EspCP),
    \+ verifica_perms_aux(EP, EspCP, E_LN), !.

verifica_perms_aux(EP, EspCP, E_LN) :-
    nth1(1, E_LN, N1),
    nth1(2, E_LN, N2),
    verifica_perms_aux2(EP, EspCP, N1, N2).

verifica_perms_aux2([], [], _, _).
verifica_perms_aux2(P1, [P2|_], N1, N2) :-
    nth1(N1, P1, El1),
    nth1(N2, P2, El2),
    El1 == El2, !.
verifica_perms_aux2(P1, [P2|EspCP], N1, N2) :-
    nth1(N1, P1, El1),
    nth1(N2, P2, El2),
    El1 \= El2,
    verifica_perms_aux2(P1, EspCP, N1, N2).

% 3.1.9

% Espacos e uma lista de espacos, Perms_soma e uma lista de listas e Esp
% e um espaco. Perms_poss e uma lista de 2 elementos em que o primeiro e
% a lista de variaveis de Esp e o segundo e a lista ordenada de
% permutacoes possiveis para o espaco Esp.

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) :-
    bagof(P, permutacao_possivel_espaco(P, Esp, Espacos, Perms_soma), Perms),
    espaco_elementos(Esp, _, Esps),
    append([Esps], [Perms], Perms_poss).

% 3.1.10

% Espacos e uma lista de espacos e Perms_poss_esps e a lista de
% permutacoes possiveis.

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    permutacoes_possiveis_espacos(Espacos, Espacos, Perms_soma, Perms_poss_esps).

permutacoes_possiveis_espacos(_, [], _, []).
permutacoes_possiveis_espacos(Espacos, [Esp|Esps], Perms_soma, [P|Perms_poss_esps]) :-
    permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, P),
    permutacoes_possiveis_espacos(Espacos, Esps, Perms_soma, Perms_poss_esps).


% 3.1.11

% Lst_Perms e uma lista de permutacoes e Numeros_comuns e uma lista de
% pares (pos, numero). Isto significa que todas as listas de Lst_Perms
% contem o numero numero na posicao pos.

numeros_comuns(Lst_perms, Numeros_comuns) :-
    nth1(1, Lst_perms, L1),
    length(L1, NMax),
    NNMax is NMax + 1,
    numeros_comuns(Lst_perms, Numeros_comuns, 1, NNMax), !.
numeros_comuns(_, [], N, NMax) :-
    N == NMax.
numeros_comuns(Lst_perms, [(N, El)|Numeros_comuns], N, NMax) :-
    nth1(1, Lst_perms, L1),
    nth1(N, L1, El),
    numeros_comuns_aux(El, Lst_perms, Numeros_comuns, N),
    NewN is N + 1,
    numeros_comuns(Lst_perms, Numeros_comuns, NewN, NMax).
numeros_comuns(Lst_perms, Numeros_comuns, N, NMax) :-
    nth1(1, Lst_perms, L1),
    nth1(N, L1, El),
    \+ numeros_comuns_aux(El, Lst_perms, Numeros_comuns, N),
    NewN is N + 1,
    numeros_comuns(Lst_perms, Numeros_comuns, NewN, NMax).

numeros_comuns_aux(_, [], _, _) :- !.
numeros_comuns_aux(El, [P|Lst_perms], Numeros_comuns, N) :-
    nth1(N, P, El1),
    El == El1,
    numeros_comuns_aux(El, Lst_perms, Numeros_comuns, N).
numeros_comuns_aux(El, [P|_], _, N) :-
    nth1(N, P, El1),
    El \= El1, !, fail.

% 3.1.12

% Perms_Possiveis e uma lista de permutacoes possiveis. Este predicado
% atualiza a lista atribuindo a cada espaco numeros comuns a todas as
% permutacoes possiveis para esse espaco.

atribui_comuns(Perms_Possiveis) :-
    maplist(atribui_comuns_aux, Perms_Possiveis).

atribui_comuns_aux(Lista) :-
    nth1(2, Lista, Perms),
    numeros_comuns(Perms, NCom),
    NCom \== [],
    nth1(1, Lista, Esp),
    substitui_esp(Esp, NCom).
atribui_comuns_aux(Lista) :-
    nth1(2, Lista, Perms),
    numeros_comuns(Perms, NCom),
    NCom == [], !.

% substitui_esp recebe um espaco Esp e substitui os seus valores.

substitui_esp(_, []).
substitui_esp(Esp, [NC|NCom]) :-
    NC = (Pos, Num),
    nth1(Pos, Esp, Num),
    substitui_esp(Esp, NCom).

% 3.1.13

% Perms_Possiveis e uma lista de permutacoes possiveis.
% Novas_Perms_Possiveis e o resultado de tirar permutacoes impossiveis
% de Perms_Possiveis.

retira_impossiveis([], []).
retira_impossiveis([Perms|Perms_Possiveis], [[Esp, Aux]|Novas_Perms_Possiveis]) :-
    nth1(1, Perms, Esp),
    tem_numeros(Esp),
    encontra_numeros(Esp, Num, 1),
    nth1(2, Perms, P),
    verifica_solucoes(P, Num, Aux),
    retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis).
retira_impossiveis([Perms|Perms_Possiveis], [Perms|Novas_Perms_Possiveis]) :-
    nth1(1, Perms, Esp),
    \+ tem_numeros(Esp),
    retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis).

% tem_numeros recebe uma lista e devolve false no caso da lista nao ter
% nenhum numero e true caso contrario

tem_numeros([]) :- !, fail.
tem_numeros([E|_]) :-
    integer(E), !.
tem_numeros([E|R]) :-
    \+ integer(E),
    tem_numeros(R).

% encontra_numeros recebe uma lista Esp e cria uma lista Num com a
% posicao na lista onde se encontra um numero e o proprio numero.

encontra_numeros([], [], _).
encontra_numeros([E|Esp], [[N, E]|Num], N) :-
    integer(E),
    NN is N + 1,
    encontra_numeros(Esp, Num, NN).
encontra_numeros([E|Esp], Num, N) :-
    \+ integer(E),
    NN is N + 1,
    encontra_numeros(Esp, Num, NN).

% verifica_solucoes recebe as permutacoes possiveis P para um dado
% espaco e os numeros e respetivas posicoes Num que devem estar
% presentes nas permutacoes. Devolve apenas as permutacoes possiveis.

verifica_solucoes([], _, []).
verifica_solucoes([El|P], Num, [El|Res]) :-
    verifica_solucoes_aux(El, Num),
    verifica_solucoes(P, Num, Res).
verifica_solucoes([El|P], Num, Res) :-
    \+ verifica_solucoes_aux(El, Num),
    verifica_solucoes(P, Num, Res).

verifica_solucoes_aux(_, []).
verifica_solucoes_aux(L, [N|Num]) :-
    nth1(1, N, P),
    nth1(2, N, E),
    nth1(P, L, E2),
    E == E2,
    verifica_solucoes_aux(L, Num), !.
verifica_solucoes_aux(L, [N|_]) :-
    nth1(1, N, P),
    nth1(2, N, E),
    nth1(P, L, E2),
    E \= E2, !, fail.

%3.1.14

% Perms_Possiveis e uma listade permutacoes possiveis.
% Novas_Perms_Possiveis e o resultado de simplificar Perms_Possiveis.

simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Aux),
    Perms_Possiveis \== Aux,
    simplifica(Aux, Novas_Perms_Possiveis).
simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Aux),
    Perms_Possiveis == Aux,
    Novas_Perms_Possiveis = Aux.

%3.1.15

% Puzzle e um e Perms_Possiveis e a lista de permutacoes possiveis
% simplificada para Puzzle.

inicializa(Puzzle, Perms_Possiveis) :-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_poss_esps),
    simplifica(Perms_poss_esps, Perms_Possiveis).

%3.2.1

% Perms_Posiveis e uma lista de permutacoes possiveis. Escolha e o
% elemento de Perms_Possiveis.

escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
    perms_unit(Perms_Possiveis),
    escolhe_menos_alternativas_1(Perms_Possiveis, Aux),
    comp_listas(Aux, Comps),
    min_list(Comps, Menor),
    escolhe_menos_alternativas_2(Aux, Menor, Lista),
    nth1(1, Lista, Escolha), !.
escolhe_menos_alternativas(Perms_Possiveis, _) :-
    \+ perms_unit(Perms_Possiveis), !, fail.

escolhe_menos_alternativas_1([], _).
escolhe_menos_alternativas_1([Perm|Perms_Possiveis], [Perm|Aux]) :-
    nth1(2, Perm, P),
    length(P, N),
    N \= 1,
    escolhe_menos_alternativas_1(Perms_Possiveis, Aux).
escolhe_menos_alternativas_1([Perm|Perms_Possiveis], Aux) :-
    nth1(2, Perm, P),
    length(P, N),
    N == 1,
    escolhe_menos_alternativas_1(Perms_Possiveis, Aux).

escolhe_menos_alternativas_2([], _, []).
escolhe_menos_alternativas_2([P|Perms], Menor, [P|Lst]) :-
    nth1(2, P, L),
    length(L, N),
    N == Menor,
    escolhe_menos_alternativas_2(Perms, Menor, Lst).
escolhe_menos_alternativas_2([P|Perms], Menor, Lst) :-
    nth1(2, P, L),
    length(L, N),
    N \== Menor,
    escolhe_menos_alternativas_2(Perms, Menor, Lst).

% perms_unit recebe a lista Perms_Possiveis e verifica se sao unitarias.
% Devolve false caso todos os elementos sejam unitarios e true caso
% contrario.

perms_unit([]) :- !, fail.
perms_unit([P|Perms_Possiveis]) :-
    nth1(2, P, P1),
    length(P1, N1),
    N1 == 1,
    perms_unit(Perms_Possiveis).
perms_unit([P|_]) :-
    nth1(2, P, P1),
    length(P1, N1),
    N1 \== 1.

% comp_listas recebe a lista Perms e devolve uma lista Comps com todos
% os comprimentos de cada lista de Perms.

comp_listas([], []).
comp_listas([P|Perms], [Len|Comps]) :-
    nth1(2, P, C),
    length(C, Len),
    comp_listas(Perms, Comps).

%3.2.2

% Perms_Possiveis e uma lista de permutacoes possiveis. Escolha e um dos
% seus elementos.

experimenta_perm(_, [], []).
experimenta_perm(Escolha, [P|Perms_Possiveis], [[Esp, [Perm]]|Novas_Perms_Possiveis]) :-
    P == Escolha,
    nth1(1, Escolha, Esp),
    nth1(2, Escolha, Lst_Perms),
    member(Perm, Lst_Perms),
    Esp = Perm,
    experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis).
experimenta_perm(Escolha, [P|Perms_Possiveis], [P|Novas_Perms_Possiveis]) :-
    P \= Escolha,
    nth1(1, Escolha, Esp),
    nth1(2, Escolha, Lst_Perms),
    member(Perm, Lst_Perms),
    Esp = Perm,
    experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis).

%3.2.3
%
% Perms_Possiveis e uma lista de permutacoes possiveis.
% Novas_Perms_Possiveis e o resultado de aplicar o algoritmo da seccao
% 2.2 a Perms_Possiveis.

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
    \+ verifica_perm(Perms_Possiveis),
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    experimenta_perm(Escolha, Perms_Possiveis, Auxiliar),
    simplifica(Auxiliar, Novo_Aux),
    resolve_aux(Novo_Aux, Novas_Perms_Possiveis).
resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
    verifica_perm(Perms_Possiveis),
    Novas_Perms_Possiveis = Perms_Possiveis.

% verifica_perm recebe a lista de permutacoes e caso todas as
% permutacoes tenham apenas um elemento devolve true. No caso de pelo
% menos uma delas ter tamanho maior que um devolve false.

verifica_perm([]).
verifica_perm([P|_]) :-
    nth1(2, P, LstPerm),
    length(LstPerm, Len),
    Len \= 1,
    !, fail.
verifica_perm([P|Perms]) :-
    nth1(2, P, LstPerm),
    length(LstPerm, Len),
    Len == 1,
    verifica_perm(Perms).

%3.3.1
%
% Puz e um puzzle e o predicado resolve-o. No final, a grelha de Puz tem
% todas as variaveis substituidas por numeros que respeitam as
% restricoes de Puz.

resolve(Puz) :-
    inicializa(Puz, Perms_Possiveis),
    resolve_aux(Perms_Possiveis, _).
