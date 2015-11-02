% Data: 02/11/2015
% Nome: Nicholas



%Pretende-se praticar aqui a representação de bases de dados em Prolog. Neste
%caso, o enunciado já explicita qual o formato da representação. A resolução é
%conseguida à custa dos (poderosos) predicados findall e member. O fato revendedora
%tem de ser definido como dinâmico, uma vez que é manipulado via predicados
%assert e retract.

%Predicados novos (auxiliares):
%flatten(L1,L2)– remove todas as sublistas de L1, devolvendo o resultado em L2
%?-flatten([[1],[2,3]], X). resultado X=[1,2,3]
%list_to_set(L1,L2)– remove elementos repetidos de L1, devolvendo L2.
%?-list_to_set([1,2,2,3],X). resultado X=[1,2,3].

:- dynamic(revendedora/2).

% 1: representacao da base de dados
revendedora(sao_paulo, [
                cliente(rui,2324,23,medico,[
                            carro(audi,a2,200000),
                            carro(bmw,serie3,300000)
                        ]),
                cliente(rita,2325,32,advogado,[
                            carro(audi,a3,300000)
                        ]),
                cliente(joao,2326,26,professor,[
                            moto(honda,gl1800,260000)
                        ]),
                cliente(ana,2327,49,medico,[
                            carro(audi,a4,400000),
                            carro(bmw,serie3,320000),
                            carro(ford,focus,240000)
                        ])
            ]).

revendedora(curitiba, [
                cliente(rui,3333,33,administrador,[
                            carro(fiat,panda,120000)
                        ]),
                cliente(paulo,3334,22,advogado,[
                            carro(audi,a4,360000)
                        ]),
                cliente(pedro,3335,46,advogado,[
                            carro(honda,accord,320000),
                            carro(audi,a2,200000)
                        ])
            ]).


% devolve a lista LC com o nome de todos clientes da revendora X
listar_clientes(X,LC):-
    revendedora(X,L),
    findall(C,member(cliente(C,_,_,_,_),L),LC).

% devolve a lista LD com todos dados (i.e.: numero, idade e profissão) do
% cliente de nome C da revendedora X
listar_dados(X,C,D):-
    revendedora(X,L),
    findall((N,ID,P),member(cliente(C,N,ID,P,_),L),D).

% devolve a lista LM com nome de todas as marcas de carros vendidos pela
% revendora X
listar_carros(X,LM):-
    revendedora(X,L),
    findall(C,member(cliente(_,_,_,_,C),L),LC),
    flatten(LC,LCC),
    findall(M,member(carro(M,_,_),LCC),LM1),
    list_to_set(LM1,LM).

% devolve a lista LA com nome de todos os advogados de todas as revendedoras
listar_advogados(LA):-
    findall(L,revendedora(_,L),LL),
    flatten(LL,LL2),
    findall(C,member(cliente(C,_,_,advogado,_),LL2),LA1),
    list_to_set(LA1,LA).

% predicados auxiliares:
tamanho([], 0).
tamanho([_ | R], N) :- tamanho(R, N1), N is N1+1.

soma([], 0).
soma([X | Y], S) :- soma(Y, R), S is R+X.

media(X,[X]).
media(M,L):- soma(L,S), tamanho(L,T), M is S/T.

% devolve o preço médio (Med) de todos carros vendidos pela revendedora X
preco_medio(X,Med):-
    revendedora(X,L),
    findall(C,member(cliente(_,_,_,_,C),L),LP),
    flatten(LP,LP2),
    findall(P,member(carro(_,_,P),LP2),LP3),
    media(Med,LP3).

% predicado auxiliar:
incrementa_id(L,L2,C):-
    select(cliente(C,N,ID,P,V),L,L1),
    NID is ID + 1,
    append([cliente(C,N,NID,P,V)],L1,L2).

% incrementa a idade do cliente C da revendedora X (faz niver)
incrementa_id(X,C):-
    retract(revendedora(X,L)),   % retira clausula da base
    incrementa_id(L,L2,C),
    assertz(revendedora(X,L2)).  % grava clausula no inicio da base

incrementa_id_Q:-
    write('nome do revendedora: '),  read(X),
    write('nome do cliente: '),  read(C),
    retract(revendedora(X,L)),   % retira clausula da base
    incrementa_id(L,L2,C),
    assertz(revendedora(X,L2)).  % grava clausula no fim da base

% exemplo de um teste deste programa:
teste:-
    write('Ana faz niver - mudar sua idade\nde:'),
    listar_dados(vegas,ana,D),write(D),
    incrementa_id(vegas,ana),listar_dados(vegas,ana,D1), % nao muda a idade no predicado, simula apenas
    write(' para: '),write(D1).



% 1 - incluir_revendedora
incluir_revendedora(X) :- assertz(revendedora(X, [])).

% 2 - excluir_revendedora
excluir_revendedora(X) :- retract(revendedora(X, _)).

% 3 - incluir_cliente
incluir_cliente(X, C) :- retract(revendedora(X, L)),
                         write('Numero: '), read(No),
                         write('Idade: '), read(Idade),
                         write('Profissao: '), read(Prof),
                         assertz(revendedora(X, [ cliente(C, No, Idade, Prof, []) | L ])).

% 4 - excluir_cliente
excluir_cliente(X, C) :- retract(revendedora(X, L)),
                         select(cliente(C, _, _, _, _), L, LR),
                         assertz(revendedora(X, LR)).

% 6 - faz_compra
faz_compra(X, C, V) :- retract(revendedora(X, L)),
                       select(cliente(C, N, M, O, Compras), L, L1),
                       write('Marca: '), read(Marca),
                       write('Nome: '), read(Nome),
                       write('Preco: '), read(Preco),
                       Pred =.. [V, Marca, Nome, Preco],
                       append([cliente(C, N, M, O, [Pred | Compras])], L1, L2),
                       assertz(revendedora(X, L2)).

% 7 - exclui_compra
exclui_compra(X, C, V) :- retract(revendedora(X, L)), % Retira
                          select(cliente(C, N, M, O, Compras), L, L1), % Pega compras de um cliente
                          select(V, Compras, NCompras), % Retira compras da lista de compras
                          append([cliente(C, N, M, O, NCompras)], L1, L2), % Readd cliente a lista de clientes
                          assertz(revendedora(X, L2)). % Readd revendedora

% 8 - Listar_revendedoras
listar_revendedoras(LR) :- findall(R, revendedora(R, _), LR).

% 9 - listar_motos
listar_motos(X, LM) :- revendedora(X,L),
                       findall(C, member(cliente(_, _, _, _, C), L), LC),
                       flatten(LC, LCC),
                       findall(M, member(moto(M, _, _), LCC), LM1),
                       list_to_set(LM1, LM).

% 10 - listar_professionais
listar_profissionais(P, LP) :- findall(L, revendedora(_, L), LL),
                               flatten(LL, LL2),
                               findall(C, member(cliente(C, _, _, P, _), LL2), LP1),
                               list_to_set(LP1, LP).

faz_compra :- write('Revendedora?'), read(R),
              write('Cliente?'), read(C),
              write('carro ou moto?'), read(V),
              faz_compra(R, C, V).

exclui_compra :- write('Qual Revendedora? '), read(R),
                 write('Qual cliente? '), read(C),
                 write('Qual compra? '), read(V),
                 write('Qual Marca? '), read(M),
                 write('Qual modelo? '), read(Mod),
                 write('Qual preco? '), read(P),
                 Venda =.. [V, M, Mod, P],
                 exclui_compra(R, C, Venda).

incluir_revendedora :- write('Nome? '), read(R),
                       incluir_revendedora(R).

excluir_revendedora :- write('Nome? '), read(R),
                       excluir_revendedora(R).

incluir_cliente :- write('Revendedora? '), read(R),
                   write('Cliente? '), read(C),
                   incluir_cliente(R, C).

excluir_cliente :- write('Revendedora? '), read(R),
                   write('Cliente? '), read(C),
                   excluir_cliente(R, C).
