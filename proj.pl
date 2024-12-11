% Joao Silva

%working_directory(_, 'C:/Users/jonas/Desktop/LP/proj').
%:- [codigo_comum, puzzles_publicos].

:- [codigo_comum].


% # # # # # # # # # # UTIL # # # # # # # # # #
pop([P|_], P).
%---------------------------------------------
maximo(X, Y, X) :- X >= Y, !.
maximo(_, Y, Y).
%---------------------------------------------
member_var(_, []):- !, fail.
member_var(Elem, [P|_]):-
	Elem == P,
	!.
member_var(Elem, [_|R]):-
	member_var(Elem, R).
%---------------------------------------------
memberchk_var(Elem, List) :- once(member_var(Elem, List)).
%---------------------------------------------
filtra([] ,_, []).
filtra([P | R], Tst, L) :-
	Lit =.. [Tst, P],
	call(Lit),
	!,
	L = [P | S],
	filtra(R, Tst, S).
filtra([_ | R], Tst, S) :-
	filtra(R, Tst, S).
% # # # # # # # # # # # # # # # # # # # # # #


% # # # # # # # # # # ESPACO # # # # # # # # # # # #
soma_de_espaco(espaco(S, _), S).
lst_de_espaco(espaco(_, L), L).

faz_espaco(Soma, LstEsp, espaco(Soma, LstEsp)):-!.
faz_espaco([P|_], v, LstEsp, Esp):-
	faz_espaco(P, LstEsp, Esp).
faz_espaco([_|ULst], h, LstEsp, Esp):-
	pop(ULst, U),
	faz_espaco(U, LstEsp, Esp).
% # # # # # # # # # # # # # # # # # # # # # # # # # 


% # # # # # # # # # # # # # # PROGRAMA # # # # # # # # # # # # # # #
combinacoes_soma(N, Els, Soma, Combs):-
	findall(Comb, combinacao(N, Els, Comb), AllCombs),
	filtra_comb_soma(Soma, AllCombs, Combs).

filtra_comb_soma(_, [], []):-!.
filtra_comb_soma(Soma, [P|R], [P|CombSoma]):-
	sum_list(P,Soma),
	!,
	filtra_comb_soma(Soma,R,CombSoma).
filtra_comb_soma(Soma, [_|R], CombSoma):-
	filtra_comb_soma(Soma, R, CombSoma).
%-------------------------------------------------------------------
permutacoes_soma(N, Els, Soma, PermsOrdenadas):-
	combinacoes_soma(N, Els, Soma, Combs),
	permutacoes_soma(Combs, Comb_Perms),
	Comb_Perms \== [],
	append(Comb_Perms, Perms),
	sort(Perms, PermsOrdenadas).

permutacoes_soma([],[]).
permutacoes_soma([P|R], [P_Perms|Perms]):-
	permutacoes_soma(R, Perms),
	findall(Perm, permutation(P, Perm), P_Perms).
%-------------------------------------------------------------------
percorre_espaco(LstEsp, [], Head, Esp, H_V):-
	!,
	faz_espaco(Head, H_V, LstEsp, Esp).
percorre_espaco(LstEsp, [P|R], Head, Esp, H_V):-
	var(P), 
	!,
	append(LstEsp, [P], NovaListaEsp),
	percorre_espaco(NovaListaEsp, R, Head, Esp, H_V).
percorre_espaco(LstEsp, [P|_], Head, Esp, H_V):-
	\+var(P),
	faz_espaco(Head, H_V, LstEsp, Esp).
percorre_espaco(_, Fila, _, Esp, H_V):-
	pop(Fila, P),
	\+var(P),
	espaco_fila(Fila, Esp, H_V).

espaco_fila([], _, _):- fail, !.
espaco_fila([P|R], Esp, H_V):- 
	P\=@= [0,0],
	\+var(P),
	pop(R, Next),
	var(Next),
	!,
	percorre_espaco([], R, P, Esp, H_V).
espaco_fila([_|R], Esp, H_V):-
	espaco_fila(R, Esp, H_V).
%-------------------------------------------------------------------
espacos_fila(H_V, Fila, Espacos):-
	bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos).
espacos_fila(_, _, []):-!.
%-------------------------------------------------------------------
espacos_puzzle([], []):-!.
espacos_puzzle(Puzzle, Esp):-
	espacos_puzzle(Puzzle, h, EspLinhas, []),
	mat_transposta(Puzzle, PuzzleTrans),
	espacos_puzzle(PuzzleTrans, v, EspCols, []),
	append(EspLinhas, EspCols, Esp). 

espacos_puzzle([], _, Esp, Esp):- !.
espacos_puzzle([P|R], H_V, Esp, EspLst):-
	espacos_fila(H_V, P, EspFila),
	append(EspLst, EspFila, NovaEspFila),
	!,
	espacos_puzzle(R, H_V, Esp, NovaEspFila).
%-------------------------------------------------------------------
/*espacos_com_posicoes_comuns(Espacos, Esp, Esps_com):-
	lst_de_espaco(Esp, L), !,
	espacos_com_posicoes_comuns(Espacos, L, Esps_com_Lst, []),
	list_to_set(Esps_com_Lst, Esps_com_incl_Esp),
	subtract(Esps_com_incl_Esp, [Esp], Esps_com).

espacos_com_posicoes_comuns([], _, Esps_com, Esps_com):- !.
espacos_com_posicoes_comuns([P|R], Lst_Esp_obj, Esps_com, EspLst):-
	lst_de_espaco(P, Lst_Esp_a_testar),
	tem_posicao_comum(Lst_Esp_a_testar, Lst_Esp_obj),
	!,
	append(EspLst, [P], NovaEspLst),
	espacos_com_posicoes_comuns(R, Lst_Esp_obj, Esps_com, NovaEspLst).
espacos_com_posicoes_comuns([_|R], Lst_Esp_obj, Esps_com, EspLst):-
	espacos_com_posicoes_comuns(R, Lst_Esp_obj, Esps_com, EspLst).

tem_posicao_comum([], _):- !, fail.
tem_posicao_comum([P|_], Esp):-
	memberchk_var(P, Esp),
	!.
tem_posicao_comum([_|R], Esp):-
	tem_posicao_comum(R,Esp).*/


espacos_com_posicoes_comuns(Espacos, Esp, Esps_com):-
	espacos_com_posicoes_comuns(Espacos, Esp, Esps_com_Lst, []),
	list_to_set(Esps_com_Lst, Esps_com).

espacos_com_posicoes_comuns([], _, Esps_com, Esps_com):- !.
espacos_com_posicoes_comuns([P|R], Esp, Esps_com, Esps_com_Temp):-
	P \== Esp,
	lst_de_espaco(Esp, Lst_Esp),
	lst_de_espaco(P, Lst_Esp_a_testar),
	tem_posicao_comum(Lst_Esp_a_testar, Lst_Esp),
	!,
	append(Esps_com_Temp, [P], NovaEspLst),
	espacos_com_posicoes_comuns(R, Esp, Esps_com, NovaEspLst).
espacos_com_posicoes_comuns([_|R], Esp, Esps_com, Esps_com_Temp):-
	espacos_com_posicoes_comuns(R, Esp, Esps_com, Esps_com_Temp).

tem_posicao_comum([], _):- !, fail.
tem_posicao_comum([P|_], Esp):-
	memberchk_var(P, Esp),
	!.
tem_posicao_comum([_|R], Esp):-
	tem_posicao_comum(R,Esp).
%-------------------------------------------------------------------
permutacoes_soma_espacos(Espacos, Perms_soma):-
	permutacoes_soma_espacos(Espacos, Perms_soma, []).

permutacoes_soma_espacos([], Perms_soma, Perms_soma):- !.
permutacoes_soma_espacos([P|R], Perms_soma, LstPerms):-
	soma_de_espaco(P, SomaEsp),
	lst_de_espaco(P, LstEsp),
	length(LstEsp, Compr),
	!,
	permutacoes_soma(Compr, [1,2,3,4,5,6,7,8,9], SomaEsp, Perms),
	append(LstPerms, [[P, Perms]], NovaLstPerms),
	!,
	permutacoes_soma_espacos(R, Perms_soma, NovaLstPerms).
%-------------------------------------------------------------------
/*encontra_perm(_, [], _):- !, fail.
encontra_perm(Esp, [[Esp,Perms]|_], [Esp,Perms]):- !.
encontra_perm(Esp, [_|R], Perm):-
	encontra_perm(Esp, R, Perm).*/

encontra_perm(_, [], _):- !, fail.
encontra_perm(Esp, [P|_], P):-
	pop(P, P_Esp),
	Esp == P_Esp,
	!.
encontra_perm(Esp, [_|R], Perm):-
	encontra_perm(Esp, R, Perm).

encontra_perms_de_esp_com([], _, PermsEspCom, PermsEspCom).
encontra_perms_de_esp_com([P|R], Perms_soma, PermsEspCom, PermsEspComLst):-
	encontra_perm(P, Perms_soma, PermEspCom),
	append(PermsEspComLst, [PermEspCom], NovaPermsEspComLst),
	encontra_perms_de_esp_com(R, Perms_soma, PermsEspCom, NovaPermsEspComLst).

sem_conflitos(Perm, LstEsp, LstEspCom, [P|_]):-
	LstEsp = Perm,
	LstEspCom = P,
	!.
sem_conflitos(Perm, LstEsp, LstEspCom, [_|R]):-
	sem_conflitos(Perm, LstEsp, LstEspCom, R).

permutacao_possivel(_, _, []):- !.
permutacao_possivel(Perm, LstEsp, [P|R]):-
	pop(P, EspCom),
	lst_de_espaco(EspCom, LstEspCom),
	last(P, PermsEspCom),
	sem_conflitos(Perm, LstEsp, LstEspCom, PermsEspCom),
	!,
	permutacao_possivel(Perm, LstEsp, R),!.

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma):-
	espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
	encontra_perm(Esp, Perms_soma, PermsEsp),
	last(PermsEsp, Perms),
	lst_de_espaco(Esp, LstEsp),
	encontra_perms_de_esp_com(Esps_com, Perms_soma, PermsEspCom, []),
	member(Perm, Perms),
	permutacao_possivel(Perm, LstEsp, PermsEspCom).
%-------------------------------------------------------------------
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss):-
	findall(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), LstPermsPoss),
	lst_de_espaco(Esp, LstEsp),
	Perms_poss = [LstEsp, LstPermsPoss].
%-------------------------------------------------------------------
permutacoes_possiveis_espacos_aux(_, [], _, Perms_poss_esps, Perms_poss_esps):- !.
permutacoes_possiveis_espacos_aux(Espacos, [P|R], Perms_soma, Perms_poss_esps, PermsPossEspsLst):-
	permutacoes_possiveis_espaco(Espacos, Perms_soma, P, Perms_poss),
	append(PermsPossEspsLst, [Perms_poss], NovaLstPermsPossEsps),
	!,
	permutacoes_possiveis_espacos_aux(Espacos, R, Perms_soma, Perms_poss_esps, NovaLstPermsPossEsps).

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):-
	permutacoes_soma_espacos(Espacos, Perms_soma),
	permutacoes_possiveis_espacos_aux(Espacos, Espacos, Perms_soma, Perms_poss_esps, []).
%-------------------------------------------------------------------
numero_comum([], _, _):- !.
numero_comum([P|R], Numero, Index):-
	nth1(Index, P, Numero),
	!,
	numero_comum(R, Numero, Index).

numeros_comuns([], _, _, PosCom, PosCom):- !.
numeros_comuns([P|R], Perms, Index, PosCom, LstPosCom):-
	numero_comum(Perms, P, Index),
	Prox_Index is Index + 1,
	append(LstPosCom, [(Index, P)], NovaLstPosCom),
	!,
	numeros_comuns(R, Perms, Prox_Index, PosCom, NovaLstPosCom).
numeros_comuns([_|R], Perms, Index, PosCom, LstPosCom):-
	Prox_Index is Index + 1, 
	!,
	numeros_comuns(R, Perms, Prox_Index, PosCom, LstPosCom).

numeros_comuns([], []).
numeros_comuns([P|R], NumerosComuns):-
	numeros_comuns(P, R, 1, NumerosComuns, []).
%-------------------------------------------------------------------
atribui_comuns_perm(_, []).
atribui_comuns_perm(Esp, [P|R]):-
	(Pos, Num) = P,
	!,
	nth1(Pos, Esp, Num),
	!,
	atribui_comuns_perm(Esp, R).

atribui_comuns_perm([Esp, Perms]):-
	numeros_comuns(Perms, NumCom),
	atribui_comuns_perm(Esp, NumCom).

atribui_comuns([]):- !.
atribui_comuns([P|R]):-
	atribui_comuns_perm(P),
	atribui_comuns(R).
%-------------------------------------------------------------------
retira_impossiveis_aux(_, [], NovasPermsLst, NovasPermsLst):- !.
retira_impossiveis_aux(Esp, [P|R], NovasPermsLst, PermsLstAux):-
	subsumes_term(Esp, P),
	!,
	append(PermsLstAux, [P], NovasPermsLstAux),
	retira_impossiveis_aux(Esp, R, NovasPermsLst, NovasPermsLstAux).
retira_impossiveis_aux(Esp, [_|R], NovasPermsLst, PermsLstAux):-
	retira_impossiveis_aux(Esp, R, NovasPermsLst, PermsLstAux).

retira_impossiveis([], NovasPerms, NovasPerms):- !.
retira_impossiveis([[Esp, Perms]|R], NovasPerms, NovasPermsAux):-
	retira_impossiveis_aux(Esp, Perms, NovasPermsLst, []),
	NovasPermsLst \== [],
	append(NovasPermsAux, [[Esp, NovasPermsLst]], NovasPermsLstAux),
	retira_impossiveis(R, NovasPerms, NovasPermsLstAux).
retira_impossiveis(Perms, NovasPerms):-
	retira_impossiveis(Perms, NovasPerms, []).
%-------------------------------------------------------------------
simplifica(Perms, Novas_Perms_Possiveis):-
	atribui_comuns(Perms),
	retira_impossiveis(Perms, Perms_Possiveis),
	Perms \== Perms_Possiveis,
	!,
	simplifica(Perms_Possiveis, Novas_Perms_Possiveis).
simplifica(Perms, Novas_Perms_Possiveis):-
	atribui_comuns(Perms),
	retira_impossiveis(Perms, Novas_Perms_Possiveis).
%-------------------------------------------------------------------
inicializa(Puzzle, Novas_Perms_Possiveis):-
	espacos_puzzle(Puzzle, Espacos),
	permutacoes_possiveis_espacos(Espacos, Perms_Possiveis),
	simplifica(Perms_Possiveis, Novas_Perms_Possiveis).
%-------------------------------------------------------------------
encontra_compr_max([], CompMax, CompMax):- !.
encontra_compr_max([[_, Perms]|R], CompMax, CompMaxTemp):-
	length(Perms, Comp),
	Comp >= CompMaxTemp,
	!,
	encontra_compr_max(R, CompMax, Comp).
encontra_compr_max([_|R], CompMax, CompMaxTemp):-
	encontra_compr_max(R, CompMax, CompMaxTemp).

encontra_escolha([], Escolha, Escolha, _):- !.
encontra_escolha([[Esp, Perms]|R], Escolha, _, CompMinTemp):-
	length(Perms, Comp),
	Comp < CompMinTemp,
	Comp > 1,
	!,
	encontra_escolha(R, Escolha, [Esp, Perms], Comp).
encontra_escolha([_|R], Escolha, EscolhaTemp, CompMinTemp):-
	encontra_escolha(R, Escolha, EscolhaTemp, CompMinTemp).

escolhe_menos_alternativas(Perms_Possiveis, Escolha):-
	encontra_compr_max(Perms_Possiveis, CompMax, 0),
	!,
	CompMax > 1,
	CompMaxIncr is CompMax + 1,
	encontra_escolha(Perms_Possiveis, Escolha, _, CompMaxIncr).
%-------------------------------------------------------------------
experimenta_perm(_, [], []):-!.
experimenta_perm([Esp_Esc, Perms], [[Esp_Esc, Perms]|R], [[Perm,[Perm]]|Novas_Perms_Possiveis]):-
	!,
	member(Perm, Perms),
	Esp_Esc = Perm,
	experimenta_perm([Esp_Esc, Perms], R, Novas_Perms_Possiveis).
experimenta_perm(Escolha, [P|R], [P|Novas_Perms_Possiveis]):-
	!,
	experimenta_perm(Escolha, R, Novas_Perms_Possiveis).
%-------------------------------------------------------------------
verifica_resolvido([]):- !.
verifica_resolvido([[_, Perms]|R]):-
	length(Perms, 1),
	!,
	verifica_resolvido(R).

resolve_aux(Perms_Possiveis, Perms_Escolhidas):-
	escolhe_menos_alternativas(Perms_Possiveis, Escolha),
	experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis),
	simplifica(Novas_Perms_Possiveis, Novas_Perms_Possiveis_Simplificadas),
	resolve_aux(Novas_Perms_Possiveis_Simplificadas, Perms_Escolhidas).
resolve_aux(Perms_Escolhidas, Perms_Escolhidas):- 
	verifica_resolvido(Perms_Escolhidas),
	!.
%-------------------------------------------------------------------
resolve(Puzzle) :- 
	inicializa(Puzzle, Perms_Possiveis), 
	resolve_aux(Perms_Possiveis, _).
% # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

