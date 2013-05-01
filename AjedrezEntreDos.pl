%-----------------------------------------------------
% ajedrez.ari
% Juego de ajedrez entre dos personas.
% Autor : Edgar Altamirano Carmona.
% Profesor : Manuel Lopez Michelone.
% Fundacion Arturo Rosenblueth, A.C.
% octubre, 1994.
%-----------------------------------------------------
% Rutina inicial para el juego de ajedrez.
ajedrez :-
	tablero_inicial(X), % toma el tablero inicial
	copia(X,Y), % hace una copia
	elimina, % elimina tableros anteriores.
	assertz(tablero(Y)),
	ajedrez1(0). % inicia el juego de ajedrez.
%---------------------------------------------------------------
% Copia el tablero inicial.
copia(X,X).
%-------------------------------------------------------
% Elimina tableros anteriores.
elimina :- retract(tablero(X)), elimina.
elimina.
%-------------------------------------------------------------
% Juego de ajedrez entre dos personas.
ajedrez1(Tn) :-
	tablero(X), % Posiciones Actuales del tablero.
	Ts is Tn + 1, % Numero de la tirada actual.

	jugar(X,B,blancas,Tjb,Ts), % juegan blancas
	case([Tjb = normal -> mueve(B,X,X1),
		Tjb = corto -> enroquecorto(blancas,X,X1),
		Tjb = largo -> enroquelargo(blancas,X,X1)]),
	retract(tablero(X)), % Nuevo Tablero.
	assertz(tablero(X1)),

	jugar(X1,N,negras,Tjn,Ts),% juegan negras
	case([Tjn = normal -> mueve(N,X1,X2),
		Tjn = corto -> enroquecorto(negras,X1,X2),
		Tjn = largo -> enroquelargo(negras,X1,X2)]),
	retract(tablero(X1)), % Nuevo Tablero.
	assertz(tablero(X2)),
	!,ajedrez1(Ts).
% Sig. Iteracion.
%----------------------------------------------------------
% Se pide al usuario que tire su jugada y se analiza.
%
jugar(X,B,Piezas,Tj,Ts) :-
	mostrar(X,0), juegan(B,Piezas,Ts), analiza(B,Piezas,Tj)
	;
	jugar(X,B,Piezas,Tj,Ts).
%-----------------------------------------------------------
% Analiza si es correcta una jugada.
%
% Enroque corto o largo.
analiza(B,P,E) :-
	string_term(C,B), % encuentra los comp.lexicos
	verificaenroque(C,E).
% verifica enroque.
% Jugada normal.
analiza(B,P,normal) :-
	string_term(C,B), % encuentra los comp.lexicos
	descomponer(C,C1,C2,C3,C4),
	sintaxis(C1,C2,Ren1,Col1), % analiza posic. inicio.
	coordenadas(C1,C2,Ri,Ci), % coord. posic. inicio.
	analizainicio(Ri,Ci,P),
	sintaxis(C3,C4,Ren2,Col2), % analiza posic. final
	coordenadas(C3,C4,Rf,Cf), % coord. posic. fin.
	analizafin(Rf,Cf,P),
	jugada_legal(Ri,Ci,Rf,Cf).
%--------------------------------------------------------------
% Verifica si la tirada es un enroque corto o largo.
verificaenroque(C,largo) :-
	substring(C,0,1,"o"), substring(C,2,1,"-"), substring(C,4,1,"o"), substring(C,6,1,"-"), substring(C,8,1,"o").
verificaenroque(C,corto) :-
	substring(C,0,1,"o"), substring(C,2,1,"-"), substring(C,4,1,"o").
%-------------------------------------------------------------
% Efectua el enroque corto o largo.
enroquelargo(blancas,X,X5) :-
	casillalibre(2,2), casillalibre(2,3), casillalibre(2,4),
	devuelve_valor([2,1,_], X, Valor1), Valor1 = 4,
	devuelve_valor([2,5,_], X, Valor2), Valor2 = 6,
	modificar([2,1,A1],X,X1,0),
	modificar([2,2,A2],X1,X2,0),
	modificar([2,3,A3],X2,X3,Valor2),
	modificar([2,4,A4],X3,X4,Valor1),
	modificar([2,5,A5],X4,X5,0),
	nl,write("Enroque Largo"),nl.
enroquecorto(blancas,X,X4) :-
	casillalibre(2,6), casillalibre(2,7), 
	devuelve_valor([2,8,_], X, Valor1), Valor1 = 4,
	devuelve_valor([2,5,_], X, Valor2), Valor2 = 6,
	modificar([2,5,A1],X,X1,0),
	modificar([2,6,A2],X1,X2,Valor1),
	modificar([2,7,A3],X2,X3,Valor2),
	modificar([2,8,A4],X3,X4,0),
	nl,write("Enroque Corto"),nl.
enroquelargo(negras,X,X5) :-
	casillalibre(9,2), casillalibre(9,3), casillalibre(9,4),
	devuelve_valor([9,1,_], X, Valor1), Valor1 = -4,
	devuelve_valor([9,5,_], X, Valor2), Valor2 = -6,
	modificar([9,1,A1],X,X1,0),
	modificar([9,2,A2],X1,X2,0),
	modificar([9,3,A3],X2,X3,Valor2),
	modificar([9,4,A4],X3,X4,Valor1),
	modificar([9,5,A5],X4,X5,0),
	nl,write('Enroque Largo'),nl.
enroquecorto(negras,X,X4) :-
	casillalibre(9,6), casillalibre(9,7),
	devuelve_valor([9,8,_], X, Valor1), Valor1 = -4,
	devuelve_valor([9,5,_], X, Valor2), Valor2 = -6,
	modificar([9,5,A1],X,X1,0),
	modificar([9,6,A2],X1,X2,Valor1),
	modificar([9,7,A3],X2,X3,Valor2),
	modificar([9,8,A4],X3,X4,0),
	nl,write('Enroque Corto'),nl.
%------------------------------------------------------------
% Revisa si el movimiento efectuado es v lido.
% El movimiento puede ser (en cualquier direccion) :
% m s de una casilla (movimiento libre) o
% una sola casilla (movimiento restringido).
%-----------------------------------------------
% Identifica la pieza que se mueve y enseguida revisa
% la validez del movimiento a efectuar.
%
jugada_legal(Ri,Ci,Rf,Cf) :-
	tablero(X),
	devuelve_valor([Ri,Ci,_], X, Valor),
	piezaM(Ri,Ci,Rf,Cf,Valor).
%------------------------------------------------------------
% PEON BLANCO.
% peon blanco, una casilla hacia adelante.
piezaM(Ri,Ci,Rf,Cf,1) :-
	Ci = Cf, % columna final
	Rft is Ri + 1,
	Rf = Rft, % renglon final.
	nl,write('Peon Blanco'),nl.
% peon blanco, dos casillas hacia adelante.
piezaM(Ri,Ci,Rf,Cf,1) :-
	Ri = 3, % posicion inicial unicamente.
	Ci = Cf, % columna final
	Rft is Ri + 2,
	Rf = Rft, % renglon final
	R is Ri + 1, % revisa si la casilla intermedia esta vacia.
	tablero(X),
	devuelve_valor([R,Ci,_], X, Valor),
	Valor = 0,
	nl,write('Peon Blanco'),nl.
% peon blanco, una casilla diagonal derecha hacia adelante.
piezaM(Ri,Ci,Rf,Cf,1) :-
	Cft is Ci + 1,
	Cft = Cf, % columna final
	Rft is Ri + 1,
	Rft = Rf, % renglon final.
	tablero(X),
	devuelve_valor([Rf,Cf,_], X, Valor),
	miembro(Valor,[-1,-2,-3,-4,-5,-6]),
	nl,write('Peon Blanco'),nl.


% peon blanco, una casilla diagonal izquierda hacia adelante.
piezaM(Ri,Ci,Rf,Cf,1) :-
	Cft is Ci - 1,
	Cft = Cf, % columna final
	Rft is Ri + 1,
	Rft = Rf, % renglon final.
	tablero(X),
	devuelve_valor([Rf,Cf,_], X, Valor),
	miembro(Valor,[-1,-2,-3,-4,-5,-6]),
	nl,write('Peon Blanco'),nl.
% peon al paso: una casilla adelante y una diagonal derecha
piezaM(Ri,Ci,Rf,Cf,1) :-
	Ri = 3, % posicion inicial unicamente.
	Rs is Ri + 1, % renglon siguiente
	casillalibre(Rs,Ci), % casilla intermedia libre
	Cft is Ci + 1,
	Cft = Cf, % columna final
	Rft is Ri + 2,
	Rft = Rf, % renglon final.
	tablero(X),
	devuelve_valor([Rf,Cf,_], X, Valor),
	miembro(Valor,[-1,-2,-3,-4,-5,-6]),
	nl,write('Peon Blanco al Paso'),nl.
% peon al paso: una casilla adelante y una diagonal izquierda.
piezaM(Ri,Ci,Rf,Cf,1) :-
	Ri = 3, % posicion inicial unicamente.
	Rs is Ri + 1, % renglon siguiente
	casillalibre(Rs,Ci), % casilla intermedia libre
	Cft is Ci - 1,
	Cft = Cf, % columna final
	Rft is Ri + 2,
	Rft = Rf, % renglon final.
	tablero(X),
	devuelve_valor([Rf,Cf,_], X, Valor),
	miembro(Valor,[-1,-2,-3,-4,-5,-6]),
	nl,write('Peon Blanco al Paso'),nl.
%---------------------------------------------------------
% CABALLO BLANCO.
piezaM(Ri,Ci,Rf,Cf,2) :-
	%orientacion de la casilla destino.
	orientacion(Ri,Ci,Rf,Cf,O),
	revisacb(Ri,Ci,Rf,Cf,O,b),
	nl,write('Caballo Blanco'),nl.


revisacb(Ri,Ci,Rf,Cf,1,b) :-
	Rfn is Ri + 2, Cfn is Ci + 1, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
revisacb(Ri,Ci,Rf,Cf,1,b) :-
	Rfn is Ri + 1, Cfn is Ci + 2, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
revisacb(Ri,Ci,Rf,Cf,3,b) :-
	Rfn is Ri - 1, Cfn is Ci + 2, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
revisacb(Ri,Ci,Rf,Cf,3,b) :-
	Rfn is Ri - 2, Cfn is Ci + 1, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
revisacb(Ri,Ci,Rf,Cf,5,b) :-
	Rfn is Ri - 2, Cfn is Ci - 1, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).


revisacb(Ri,Ci,Rf,Cf,5,b) :-
	Rfn is Ri - 1, Cfn is Ci - 2, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
revisacb(Ri,Ci,Rf,Cf,7,b) :-
	Rfn is Ri + 1, Cfn is Ci - 2, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
revisacb(Ri,Ci,Rf,Cf,7,b) :-
	Rfn is Ri + 2, Cfn is Ci - 1, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
%---------------------------------------------------------------
% ALFIL BLANCO.
piezaM(Ri,Ci,Rf,Cf,3) :-
	%orientacion de la casilla destino.
	orientacion(Ri,Ci,Rf,Cf,O),
	revisaab(Ri,Ci,Rf,Cf,O,b), nl,write('Alfil Blanco'),nl.
% Movimiento tipo 1 (diagonal derecha arriba).
revisaab(Ri,Ci,Rf,Cf,1,b) :-
	Rfm is Rf - 1, Cfm is Cf - 1, movlibre1(Ri,Ci,Rfm,Cfm,b).
% Movimiento tipo 3 (diagonal derecha abajo).
revisaab(Ri,Ci,Rf,Cf,3,b) :-
	Rfm is Rf + 1, Cfm is Cf - 1, movlibre3(Ri,Ci,Rfm,Cfm,b).
% Movimiento tipo 5 (diagonal izquierda abajo).
revisaab(Ri,Ci,Rf,Cf,5,b) :-
	Rfm is Rf + 1, Cfm is Cf + 1, movlibre5(Ri,Ci,Rfm,Cfm,b).
% Movimiento tipo 7 (diagonal izquierda arriba).
revisaab(Ri,Ci,Rf,Cf,7,b) :-
	Rfm is Rf - 1, Cfm is Cf + 1, movlibre7(Ri,Ci,Rfm,Cfm,b).
%------------------------------------------------------------
% TORRE BLANCA.
% Revisa si el valido el movimiento de la torre blanca.
%
piezaM(Ri,Ci,Rf,Cf,4) :-
	%orientacion de la casilla destino.
	orientacion(Ri,Ci,Rf,Cf,O), 
	revisatb(Ri,Ci,Rf,Cf,O,b), nl,write('Torre Blanca'),nl.
% Movimiento a la derecha.
revisatb(Ri,Ci,Rf,Cf,2,b) :- 
	Cfm is Cf - 1, movlibre2(Ri,Ci,Cfm,b).
%Movimiento a la izquierda.
revisatb(Ri,Ci,Rf,Cf,6,b) :-
	Cfm is Cf + 1, movlibre6(Ri,Ci,Cfm,b).
%Movimiento hacia arriba.
revisatb(Ri,Ci,Rf,Cf,8,b) :-
	Rfm is Rf - 1, movlibre8(Ri,Ci,Rfm,b).
%Movimiento hacia abajo.
revisatb(Ri,Ci,Rf,Cf,4,b) :-
	Rfm is Rf + 1, movlibre4(Ri,Ci,Rfm,b).
%-----------------------------------------------------------------
% DAMA BLANCA.
piezaM(Ri,Ci,Rf,Cf,5) :-
	%orientacion de la casilla destino.
	orientacion(Ri,Ci,Rf,Cf,O),
	revisadb(Ri,Ci,Rf,Cf,O,b), nl,write('Dama Blanca'),nl.
% Movimiento tipo 1 (diagonal derecha arriba).
revisadb(Ri,Ci,Rf,Cf,1,b) :-
	Rfm is Rf - 1, Cfm is Cf - 1, movlibre1(Ri,Ci,Rfm,Cfm,b).
% Movimiento tipo 2 (derecha).
revisadb(Ri,Ci,Rf,Cf,2,b) :-
	Cfm is Cf - 1, movlibre2(Ri,Ci,Cfm,b).
% Movimiento tipo 3 (diagonal derecha abajo).
revisadb(Ri,Ci,Rf,Cf,3,b) :-
	Rfm is Rf + 1, Cfm is Cf - 1, movlibre3(Ri,Ci,Rfm,Cfm,b).
%Movimiento tipo 4 (abajo).
revisadb(Ri,Ci,Rf,Cf,4,b) :-
	Rfm is Rf + 1, movlibre4(Ri,Ci,Rfm,b).
% Movimiento tipo 5 (diagonal izquierda abajo).
revisadb(Ri,Ci,Rf,Cf,5,b) :-
	Rfm is Rf + 1, Cfm is Cf + 1, movlibre5(Ri,Ci,Rfm,Cfm,b).
%Movimiento a la izquierda.
revisadb(Ri,Ci,Rf,Cf,6,b) :-
	Cfm is Cf + 1, movlibre6(Ri,Ci,Cfm,b).
% Movimiento tipo 7 (diagonal izquierda arriba).
revisadb(Ri,Ci,Rf,Cf,7,b) :-
	Rfm is Rf - 1, Cfm is Cf + 1, movlibre7(Ri,Ci,Rfm,Cfm,b).
% Movimiento tipo 8 (arriba).
revisadb(Ri,Ci,Rf,Cf,8,b) :-
	Rfm is Rf - 1, movlibre8(Ri,Ci,Rfm,b).
%-------------------------------------------------------------
% REY BLANCO.
piezaM(Ri,Ci,Rf,Cf,6) :-
	distancia1(Ri,Ci,Rf,Cf), tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]), nl,write('Rey Blanco'),nl.
%---------------------------------------------------------------
% PEON NEGRO.
% peon negro, una casilla hacia adelante.
piezaM(Ri,Ci,Rf,Cf,-1) :-
	Ci = Cf,
	% columna final.
	Rft is Ri - 1,
	Rf = Rft,
	% renglon final.
	nl,write('Peon Negro'),nl.
% peon negro, dos casillas hacia adelante.
piezaM(Ri,Ci,Rf,Cf,-1) :-
	Ri = 8,
	% posicion inicial unicamente.
	Ci = Cf,
	% columna final
	Rft is Ri - 2,
	Rf = Rft,
	% renglon final
	R is Ri - 1, % revisa si la casilla intermedia esta vacia.
	tablero(X),
	devuelve_valor([R,Ci,_], X, Valor),
	Valor = 0,
	nl,write('Peon Negro'),nl.
% peon negro, una casilla diagonal derecha (tipo 3) hacia abajo.
piezaM(Ri,Ci,Rf,Cf,-1) :-
	Cft is Ci + 1,
	Cft = Cf,
	% columna final
	Rft is Ri - 1,
	Rft = Rf,
	% renglon final.
	tablero(X),
	devuelve_valor([Rf,Cf,_], X, Valor),
	miembro(Valor,[1,2,3,4,5,6]),
	nl,write('Peon Negro'),nl.
% peon negro, una casilla diagonal izquierda (tipo 5) hacia abajo.
piezaM(Ri,Ci,Rf,Cf,-1) :-
	Cft is Ci - 1,
	Cft = Cf,
	% columna final
	Rft is Ri - 1,
	Rft = Rf,
	% renglon final
	tablero(X),
	devuelve_valor([Rf,Cf,_], X, Valor),
	miembro(Valor,[1,2,3,4,5,6]),
	nl,write('Peon Negro'),nl.
% peon al paso: una casilla adelante y una diagonal derecha (tipo 3).
piezaM(Ri,Ci,Rf,Cf,-1) :-
	Ri = 8,
	% posicion inicial unicamente.
	Rs is Ri - 1, % renglon siguiente
	casillalibre(Rs,Ci), % casilla intermedia libre
	Cft is Ci + 1,
	Cft = Cf,
	% columna final
	Rft is Ri - 2,
	Rft = Rf,
	% renglon final.
	tablero(X),
	devuelve_valor([Rf,Cf,_], X, Valor),
	miembro(Valor,[1,2,3,4,5,6]),
	nl,write('Peon Negro al Paso'),nl.
% peon al paso: una casilla adelante y una diagonal izquierda (tipo 5).
piezaM(Ri,Ci,Rf,Cf,-1) :-
	Ri = 8,
	% posicion inicial unicamente.
	Rs is Ri - 1, % renglon siguiente
	casillalibre(Rs,Ci), % casilla intermedia libre
	Cft is Ci - 1,
	Cft = Cf,
	% columna final
	Rft is Ri - 2,
	Rft = Rf,
	% renglon final.
	tablero(X),
	devuelve_valor([Rf,Cf,_], X, Valor),
	miembro(Valor,[1,2,3,4,5,6]),
	nl,write('Peon Negro al Paso'),nl.
%--------------------------------------------------------------
% CABALLO NEGRO.
piezaM(Ri,Ci,Rf,Cf,-2) :-
	%orientacion de la casilla destino.
	orientacion(Ri,Ci,Rf,Cf,O),
	revisacn(Ri,Ci,Rf,Cf,O,n), nl,write('Caballo Negro'),nl.
revisacn(Ri,Ci,Rf,Cf,1,n) :-
	Rfn is Ri + 2, Cfn is Ci + 1, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
revisacn(Ri,Ci,Rf,Cf,1,n) :-
	Rfn is Ri + 1, Cfn is Ci + 2, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
revisacn(Ri,Ci,Rf,Cf,3,n) :-
	Rfn is Ri - 1, Cfn is Ci + 2, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
revisacn(Ri,Ci,Rf,Cf,3,n) :-
	Rfn is Ri - 2, Cfn is Ci + 1, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
revisacn(Ri,Ci,Rf,Cf,5,n) :-
	Rfn is Ri - 2, Cfn is Ci - 1, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
revisacn(Ri,Ci,Rf,Cf,5,n) :-
	Rfn is Ri - 1, Cfn is Ci - 2, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
revisacn(Ri,Ci,Rf,Cf,7,n) :-
	Rfn is Ri + 1, Cfn is Ci - 2, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
revisacn(Ri,Ci,Rf,Cf,7,n) :-
	Rfn is Ri + 2, Cfn is Ci - 1, Rfn = Rf, Cfn = Cf, tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
%--------------------------------------------------------------
% ALFIL NEGRO.
piezaM(Ri,Ci,Rf,Cf,-3) :-
	%orientacion de la casilla destino.
	orientacion(Ri,Ci,Rf,Cf,O), revisaan(Ri,Ci,Rf,Cf,O,n), nl,write('Alfil Negro'),nl.
% Movimiento tipo 1 (diagonal derecha arriba).
revisaan(Ri,Ci,Rf,Cf,1,n) :-
	Rfm is Rf - 1, Cfm is Cf - 1, movlibre1(Ri,Ci,Rfm,Cfm,n).
% Movimiento tipo 3 (diagonal derecha abajo).
revisaan(Ri,Ci,Rf,Cf,3,n) :-
	Rfm is Rf + 1, Cfm is Cf - 1, movlibre3(Ri,Ci,Rfm,Cfm,n).
% Movimiento tipo 5 (diagonal izquierda abajo).
revisaan(Ri,Ci,Rf,Cf,5,n) :-
	Rfm is Rf + 1, Cfm is Cf + 1, movlibre5(Ri,Ci,Rfm,Cfm,n).
% Movimiento tipo 7 (diagonal izquierda arriba).
revisaan(Ri,Ci,Rf,Cf,7,n) :-
	Rfm is Rf - 1, Cfm is Cf + 1, movlibre7(Ri,Ci,Rfm,Cfm,n).
%----------------------------------------------------------
% TORRE NEGRA.
% Revisa si el valido el movimiento de la torre negra.
%
piezaM(Ri,Ci,Rf,Cf,-4) :-
	%orientacion de la casilla destino.
	orientacion(Ri,Ci,Rf,Cf,O), revisatn(Ri,Ci,Rf,Cf,O), nl,write('Torre Negra'),nl.
%Movimiento a la derecha.
revisatn(Ri,Ci,Rf,Cf,2) :-
	Cfm is Cf - 1, movlibre2(Ri,Ci,Cfm,n).
%Movimiento a la izquierda.
revisatn(Ri,Ci,Rf,Cf,6) :-
	Cfm is Cf + 1, movlibre6(Ri,Ci,Cfm,n).
%Movimiento hacia arriba.
revisatn(Ri,Ci,Rf,Cf,8) :-
	Rfm is Rf - 1, movlibre8(Ri,Ci,Rfm,n).
%Movimiento hacia abajo.
revisatn(Ri,Ci,Rf,Cf,4) :-
	Rfm is Rf + 1, movlibre4(Ri,Ci,Rfm,n).
%-----------------------------------------------------------
% DAMA NEGRA.
piezaM(Ri,Ci,Rf,Cf,-5) :-
	%orientacion de la casilla destino.
	orientacion(Ri,Ci,Rf,Cf,O), revisadn(Ri,Ci,Rf,Cf,O,n), nl,write('Dama Negra'),nl.
% Movimiento tipo 1 (diagonal derecha arriba).
revisadn(Ri,Ci,Rf,Cf,1,n) :-
	Rfm is Rf - 1, Cfm is Cf - 1, movlibre1(Ri,Ci,Rfm,Cfm,n).
% Movimiento tipo 2 (derecha).
revisadn(Ri,Ci,Rf,Cf,2,n) :-
	Cfm is Cf - 1, movlibre2(Ri,Ci,Cfm,n).
% Movimiento tipo 3 (diagonal derecha abajo).
revisadn(Ri,Ci,Rf,Cf,3,n) :-
	Rfm is Rf + 1, Cfm is Cf - 1, movlibre3(Ri,Ci,Rfm,Cfm,n).
% Movimiento tipo 4 (abajo).
revisadn(Ri,Ci,Rf,Cf,4,n) :-
	Rfm is Rf + 1, movlibre4(Ri,Ci,Rfm,n).
% Movimiento tipo 5 (diagonal izquierda abajo).
revisadn(Ri,Ci,Rf,Cf,5,n) :-
	Rfm is Rf + 1, Cfm is Cf + 1, movlibre5(Ri,Ci,Rfm,Cfm,n).
%Movimiento tipo 6 (izquierda).
revisadn(Ri,Ci,Rf,Cf,6,n) :-
	Cfm is Cf + 1, movlibre6(Ri,Ci,Cfm,n).
% Movimiento tipo 7 (diagonal izquierda arriba).
revisadn(Ri,Ci,Rf,Cf,7,n) :-
	Rfm is Rf - 1, Cfm is Cf + 1, movlibre7(Ri,Ci,Rfm,Cfm,n).
%Movimiento tipo 8 (arriba).
revisadn(Ri,Ci,Rf,Cf,8,n) :-
	Rfm is Rf - 1, movlibre8(Ri,Ci,Rfm,n).
%-------------------------------------------------------------
% REY NEGRO.
piezaM(Ri,Ci,Rf,Cf,-6) :-
	distancia1(Ri,Ci,Rf,Cf), tablero(X), devuelve_valor([Rf,Cf,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]), nl,write('Rey Negro'),nl.
%---------------------------------------------------------------
% Revisa si es valido el movimiento restringido correspondiente.
% (Se restringe el movimiento a cualquier posicion vecina)
% 1 = diagonal derecha arriba; 2 = derecha; 3 = diagonal derecha abajo;
% 4 = abajo; 5 = diagonal izquierda abajo; 6 = izquierda;
% 7 = diagonal izquierda arriba; 8 = arriba.
%---------------------------------------------------------
%
distancia1(Ri,Ci,Rf,Cf) :-
	Rfn is Ri + 1, Cfn is Ci + 1,% diag. derecha arriba.
	Rfn = Rf, Cfn = Cf
	;
	Cfn is Ci + 1,
	% derecha.
	Rf = Ri, Cfn = Cf
	;
	Rfn is Ri - 1, Cfn is Ci + 1, % diag. derecha abajo.
	Rfn = Rf, Cfn = Cf
	;
	Rfn is Ri - 1,
	% abajo.
	Rfn = Rf, Cf = Ci
	;
	Rfn is Ri - 1, Cfn is Ci - 1, % diag. izquierda abajo.
	Rfn = Rf, Cfn = Cf
	;
	Cfn is Ci - 1,
	% izquierda.
	Rf = Ri, Cfn = Cf
	;
	Rfn is Ri + 1, Cfn is Ci - 1, % diag. izquierda arriba.
	Rfn = Rf, Cfn = Cf.
%------------------------------------------------------
% Revisa si es valido el movimiento libre correspondiente.
% (Se permite el movimiento a cualquier posicion en linea)
% 1 = diagonal derecha arriba; 2 = derecha; 3 = diagonal derecha abajo;
% 4 = abajo; 5 = diagonal izquierda abajo; 6 = izquierda;
% 7 = diagonal izquierda arriba; 8 = arriba.
%
%---------------------------------------------------------
% Movimiento Libre tipo 1 (diagonal derecha arriba).
movlibre1(Ri,Ci,Rf,Cf,P) :-
	Cf > Ci, Rf > Ri, Rn is Ri + 1, Cn is Ci + 1, casillalibre(Rn,Cn), movlibre1(Rn,Cn,Rf,Cf,P).
movlibre1(Ri,Ci,Rf,Cf,b) :-
	Rf = Ri, Cf = Ci, Rif is Ri + 1, Cif is Ci + 1, tablero(X), devuelve_valor([Rif,Cif,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
movlibre1(Ri,Ci,Rf,Cf,n) :-
	Rf = Ri, Cf = Ci, Rif is Ri + 1, Cif is Ci + 1, tablero(X), devuelve_valor([Rif,Cif,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
%------------------------------------------------------
% Movimiento Libre tipo 2 (derecha).
movlibre2(Ri,Ci,Cf,P) :-
	Cf > Ci, Cn is Ci + 1, casillalibre(Ri,Cn), movlibre2(Ri,Cn,Cf,P).
movlibre2(Ri,Ci,Cf,b) :-
	Cf = Ci, Cif is Ci + 1, tablero(X), devuelve_valor([Ri,Cif,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
movlibre2(Ri,Ci,Cf,n) :-
	Cf = Ci, Cif is Ci + 1, tablero(X), devuelve_valor([Ri,Cif,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
%----------------------------------------------------------
% Movimiento Libre tipo 3 (diagonal derecha abajo).
movlibre3(Ri,Ci,Rf,Cf,P) :-
	Cf > Ci, Rf < Ri, Rn is Ri - 1, Cn is Ci + 1, casillalibre(Rn,Cn), movlibre3(Rn,Cn,Rf,Cf,P).
movlibre3(Ri,Ci,Rf,Cf,b) :-
	Rf = Ri, Cf = Ci, Rif is Ri - 1, Cif is Ci + 1, tablero(X), devuelve_valor([Rif,Cif,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
movlibre3(Ri,Ci,Rf,Cf,n) :-
	Rf = Ri, Cf = Ci, Rif is Ri - 1, Cif is Ci + 1, tablero(X), devuelve_valor([Rif,Cif,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
%----------------------------------------------------------
% Movimiento Libre tipo 4 (abajo).
movlibre4(Ri,Ci,Rf,P) :-
	Rf < Ri, Rn is Ri - 1, casillalibre(Rn,Ci), movlibre4(Rn,Ci,Rf,P).
movlibre4(Ri,Ci,Rf,b) :-
	Rf = Ri, Rif is Ri - 1, tablero(X), devuelve_valor([Rif,Ci,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
movlibre4(Ri,Ci,Rf,n) :-
	Rf = Ri, Rif is Ri - 1, tablero(X), devuelve_valor([Rif,Ci,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
%----------------------------------------------------------
% Movimiento Libre tipo 5 (diagonal izquierda abajo).
movlibre5(Ri,Ci,Rf,Cf,P) :-
	Cf < Ci, Rf < Ri, 
	Rn is Ri - 1, % casilla siguiente (intermedia)
	Cn is Ci - 1,
	casillalibre(Rn,Cn),
	movlibre5(Rn,Cn,Rf,Cf,P).
movlibre5(Ri,Ci,Rf,Cf,b) :-
	Rf = Ri, Cf = Ci, Rif is Ri - 1, Cif is Ci - 1, tablero(X), devuelve_valor([Rif,Cif,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
movlibre5(Ri,Ci,Rf,Cf,n) :-
	Rf = Ri, Cf = Ci, Rif is Ri - 1, Cif is Ci - 1, tablero(X), devuelve_valor([Rif,Cif,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
%------------------------------------------------------
% Movimiento Libre tipo 6 (izquierda).
movlibre6(Ri,Ci,Cf,P) :-
	Cf < Ci, Cn is Ci - 1, casillalibre(Ri,Cn), movlibre6(Ri,Cn,Cf,P).
movlibre6(Ri,Ci,Cf,b) :-
	Cf = Ci, Cif is Ci - 1, tablero(X), devuelve_valor([Ri,Cif,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
movlibre6(Ri,Ci,Cf,n) :-
	Cf = Ci, Cif is Ci - 1, tablero(X), devuelve_valor([Ri,Cif,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
%--------------------------------------------------------
% Movimiento Libre tipo 7 (diagonal izquierda arriba).
movlibre7(Ri,Ci,Rf,Cf,P) :-
	Cf < Ci, Rf > Ri,
	Rn is Ri + 1, % casilla siguiente (intermedia)
	Cn is Ci - 1,
	casillalibre(Rn,Cn),
	movlibre7(Rn,Cn,Rf,Cf,P).
movlibre7(Ri,Ci,Rf,Cf,b) :-
	Rf = Ri, Cf = Ci,
	Rif is Ri + 1, % casilla destino
	Cif is Ci - 1,
	tablero(X),
	devuelve_valor([Rif,Cif,_], X, Valor),
	miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
movlibre7(Ri,Ci,Rf,Cf,n) :-
	Rf = Ri, Cf = Ci,
	Rif is Ri + 1, % casilla destino
	Cif is Ci - 1,
	tablero(X),
	devuelve_valor([Rif,Cif,_], X, Valor),
	miembro(Valor,[0,1,2,3,4,5,6]).
%---------------------------------------------------------
% Movimiento Libre tipo 8 (arriba).
movlibre8(Ri,Ci,Rf,P) :-
	Rf > Ri, Rn is Ri + 1, casillalibre(Rn,Ci), movlibre8(Rn,Ci,Rf,P).
movlibre8(Ri,Ci,Rf,b) :-
	Rf = Ri, Rif is Ri + 1, tablero(X), devuelve_valor([Rif,Ci,_], X, Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
movlibre8(Ri,Ci,Rf,n) :-
	Rf = Ri, Rif is Ri + 1, tablero(X), devuelve_valor([Rif,Ci,_], X, Valor), miembro(Valor,[0,1,2,3,4,5,6]).
%--------------------------------------------------------
% Orientacion del movimiento a efectuar.
%
orientacion(Ri,Ci,Rf,Cf,1) :- Rf > Ri, Cf > Ci. %diag. arriba derecha
orientacion(Ri,Ci,Rf,Cf,2) :- Rf = Ri, Cf > Ci. %derecha
orientacion(Ri,Ci,Rf,Cf,3) :- Rf < Ri, Cf > Ci. %diag. abajo derecha
orientacion(Ri,Ci,Rf,Cf,4) :- Rf < Ri, Cf = Ci. %abajo
orientacion(Ri,Ci,Rf,Cf,5) :- Rf < Ri, Cf < Ci. %diag. abajo izquierda
orientacion(Ri,Ci,Rf,Cf,6) :- Rf = Ri, Cf < Ci. %izquierda
orientacion(Ri,Ci,Rf,Cf,7) :- Rf > Ri, Cf < Ci. %diag. arriba izquierda
orientacion(Ri,Ci,Rf,Cf,8) :- Rf > Ri, Cf = Ci. %arriba
%--------------------------------------------------------------
% Verifica si una casilla esta libre (vacia).
casillalibre(R,C) :-
	tablero(X), devuelve_valor([R,C,_], X, Valor), Valor = 0.
%------------------------------------------------------------
% Analiza Renglon y Columna correctos.
%
sintaxis(C1,C2,C1T,C2T) :-
	int_text(C2T,C2), atom_string(C1T,C1), 
	miembro(C1T,[a,b,c,d,e,f,g,h]), % renglon correcto
	miembro(C2T,[1,2,3,4,5,6,7,8]). % columna correcta
%---------------------------------------------------------------
% Analiza si Blancas mueven a blancas o viceversa.
%
analizainicio(R,C,blancas) :-
	tablero(X), devuelve_valor([R,C,_], X, Valor), miembro(Valor,[1,2,3,4,5,6]).
analizainicio(R,C,negras) :-
	tablero(X), devuelve_valor([R,C,_], X, Valor), miembro(Valor,[-1,-2,-3,-4,-5,-6]).
analizafin(R,C,blancas) :-
	tablero(X), devuelve_valor([R,C,_],X,Valor), miembro(Valor,[0,-1,-2,-3,-4,-5,-6]).
analizafin(R,C,negras) :-
	tablero(X), devuelve_valor([R,C,_],X,Valor), miembro(Valor,[0,1,2,3,4,5,6]).
%-------------------------------------------------------------
% Pide al jugador correspondiente su jugada.
%
juegan(B,blancas,Ts) :-
	nl,write('TURNO '),write(Ts), 	write(' DE LAS PIEZAS BLANCAS :'),nl, respuesta(B).
juegan(N,negras,Ts) :-
	nl,write('TURNO '),write(Ts), 	write(' DE LAS PIEZAS NEGRAS :'),nl, respuesta(N).
%--------------------------------------------------------------
% Recibe y analiza la respuesta del jugador.
respuesta(R) :-
	read(Resp), significa(Resp,R), termina(R), !
	;
	nl, write('Respuesta Invalida, intente otra vez :'),nl,
	respuesta(R).
significa(c, cancelar).
significa(cancelar,cancelar).
significa(X,X).
%------------------------------------------------------------------
% Cancela la ejecucion de la partida.
%
termina(cancelar) :- abort(1).
termina(_).
%----------------------------------------------------------------
% Muestra el tablero actualizado en la pantalla.
%
mostrar(P1,P2) :-
	nl,tab(9),li(1,220),nl,muestra(P1,P2),!.
muestra(_,N) :-
	N >= 100,
	tab(9),li(1,223),
	nl,tab(10),write(' a b c d e f g h'),nl,nl.
muestra([P|RP],N) :-
	N < 100,
	descompone(P,R,C,V),
	cuadro([R,C],V),
	N1 is N + 1,
	muestra(RP,N1).
%-------------------------------------------------------------
% Pinta el borde superior o inferior del tablero.
%
li(N,_) :- N >= 27.
li(N,C) :- N < 27, put(C), N1 is N + 1, li(N1,C).
%-------------------------------------------------------------------
% Descompone una lista [renglon,columna,valor] en sus elementos
% individuales.
%
descompone([R,C,V],R,C,V).
%---------------------------------------------------------------------
% Utileria : funcion de pertenencia a una lista.
%
miembro(X,[X|L]) :- !.
miembro(X,[Y|L]) :- miembro(X,L).
%-----------------------------------------------
% Despliega un cuadro del tablero con la pieza contenida.
% Num es el numero de cuadro dentro del tablero.
% P es la pieza contenida (por ejemplo -5 es dama negra).
%
cuadro(Num,P) :-
	miembro(Num,[[9,1],[9,3],[9,5],[9,7],[8,2],[8,4],[8,6],
	[7,1],[7,3],[7,5],[7,7],[6,2],[6,4],[6,6],
	[5,1],[5,3],[5,5],[5,7],[4,2],[4,4],[4,6],
	[3,1],[3,3],[3,5],[3,7],[2,2],[2,4],[2,6]]),
	ifthen(miembro(Num,[[9,1],[7,1],[5,1],[3,1]]),margenizq),
	cuadro_blanco(P).
cuadro(Num,P) :-
	miembro(Num,[[8,8],[6,8],[4,8],[2,8]]),
	num_renglon(Num,X,T),
	cuadro_blanco(P),put(221),
	tab(2),write(X),tab(12),write(T),nl.
cuadro(Num,P) :-
	miembro(Num,[[9,2],[9,4],[9,6],[8,1],[8,3],[8,5],[8,7],
	[7,2],[7,4],[7,6],[6,1],[6,3],[6,5],[6,7],
	[5,2],[5,4],[5,6],[4,1],[4,3],[4,5],[4,7],
	[3,2],[3,4],[3,6],[2,1],[2,3],[2,5],[2,7]]),
	ifthen(miembro(Num,[[8,1],[6,1],[4,1],[2,1]]),margenizq),
cuadro_negro(P).
cuadro(Num,P) :-
	miembro(Num,[[9,8],[7,8],[5,8],[3,8]]),
	num_renglon(Num,X,T),
	cuadro_negro(P),put(221),
	tab(2),write(X),tab(12),write(T),nl.
cuadro(_,P).
margenizq :- tab(9),put(222).
% pinta el margen izquierdo del tablero.
%-----------------------------------------------
% Despliega un cuadro blanco o negro.
%
cuadro_blanco(C) :-
	pieza(C,A),C = 0,put(32),put(32),put(32).
cuadro_blanco(C) :-
	pieza(C,A),put(32),put(A),put(32).
cuadro_negro(C) :-
	pieza(C,A),C = 0,put(219), put(219), put(219).
cuadro_negro(C) :-
	pieza(C,A),put(219), put(A), put(219).
%--------------------------------------------------------------------
% Mueve una pieza en el tablero.
%
mueve(B,X,XN) :-
	string_term(C, B),
	descomponer(C,C1,C2,C3,C4),
	coordenadas(C1,C2,X1,Y1),
	coordenadas(C3,C4,X2,Y2),
	devuelve_valor([X1,Y1,_], X, Valor),
	modificar([X2,Y2,A1], X, XNT, Valor),
	modificar([X1,Y1,A], XNT, XN, 0).
%------------------------------------------------------
% Descompone una orden en sus componentes,
% por ejemplo : e2-e4 en 'e','2','e' y '4'.
%
descomponer(C,C1,C2,C3,C4) :-
	substring(C,0,1,C1),
	substring(C,1,1,C2),
	substring(C,3,1,"-"),
	substring(C,5,1,C3),
	substring(C,6,1,C4).
%---------------------------------------------------------
% Devuelve el valor (pieza) de una posicion del tablero.
%
devuelve_valor([X1,Y1,_], [[X1,Y1,V]|R], V) :- !.
devuelve_valor([X1,Y1,A], [[X2,Y2,V2]|R], V) :-
devuelve_valor([X1,Y1,A], R, V).
%-----------------------------------------------------------
% Modifica una posicion en el tablero.
%
modificar([X1,Y1,_], [[X1,Y1,_]|R], [[X1,Y1,NV]|R], NV) :- !.
modificar([X1,Y1,A], [[X2,Y2,V]|R], [[X2,Y2,V]|R1], NV) :-
modificar([X1,Y1,A], R, R1, NV).
%----------------------------------------------------------------
% Obtiene las coordenadas X,Y de una posicion en el tablero.
%
coordenadas(C1,C2,X,Y) :-
	int_text(C2N,C2),
	X is C2N + 1,
	atom_string(C1A,C1),
	coordy(C1A,Y).
%---------------------------------------------------------
% Equivalente ASCII de cada pieza del tablero.
%
pieza(0,32). % Espacio en blanco.
pieza(1,80). % Peon Blanco (P).
pieza(2,67). % Caballo Blanco (C).
pieza(3,65). % Alfil Blanco (A).
pieza(4,84). % Torre Blanca (T).
pieza(5,68). % Dama Blanca (D).
pieza(6,82). % Rey Blanco (R).
pieza(-1,112). % peon negro (p).
pieza(-2,99). % caballo negro (c).
pieza(-3,97). % alfil negro (a).
pieza(-4,116). % torre negra (t).
pieza(-5,100). % dama negra (d).
pieza(-6,114). % rey negro (r).
%-----------------------------------------------------------
% Equivalencia entre renglon de la matriz y renglon de
% la notacion utilizada.
%
num_renglon([9,_],8,"NOTACION LARGA.").
num_renglon([8,_],7,"Ejemplo :").
num_renglon([7,_],6,"?- e2-e4.").
num_renglon([6,_],5,"Enroques :").
num_renglon([5,_],4,"?- o-o-o.").
num_renglon([4,_],3,"?- o-o.").
num_renglon([3,_],2,"Para terminar :").
num_renglon([2,_],1,"?- cancelar.").
%-------------------------------------------------------------
% Equivalencia entre letras y columnas.
%
coordy(a,1).
coordy(b,2).
coordy(c,3).
coordy(d,4).
coordy(e,5).
coordy(f,6).
coordy(g,7).
coordy(h,8).
%--------------------------------------------------------------------
% Posiciones iniciales del tablero en una matriz de 10x12.
%
tablero_inicial([[11,0,99],[11,1,99],[11,2,99],[11,3,99],[11,4,99],
	[11,5,99],[11,6,99],[11,7,99],[11,8,99],[11,9,99],
	[10,0,99],[10,1,99],[10,2,99],[10,3,99],[10,4,99],
	[10,5,99],[10,6,99],[10,7,99],[10,8,99],[10,9,99],
	[9,0,99],[9,1,-4],[9,2,-2],[9,3,-3],[9,4,-5],
	[9,5,-6],[9,6,-3],[9,7,-2],[9,8,-4],[9,9,99],
	[8,0,99],[8,1,-1],[8,2,-1],[8,3,-1],[8,4,-1],
	[8,5,-1],[8,6,-1],[8,7,-1],[8,8,-1],[8,9,99],
	[7,0,99],[7,1, 0],[7,2, 0],[7,3, 0],[7,4, 0],
	[7,5, 0],[7,6, 0],[7,7, 0],[7,8, 0],[7,9,99],
	[6,0,99],[6,1, 0],[6,2, 0],[6,3, 0],[6,4, 0],
	[6,5, 0],[6,6, 0],[6,7, 0],[6,8, 0],[6,9,99],
	[5,0,99],[5,1, 0],[5,2, 0],[5,3, 0],[5,4, 0],
	[5,5, 0],[5,6, 0],[5,7, 0],[5,8, 0],[5,9,99],
	[4,0,99],[4,1, 0],[4,2, 0],[4,3, 0],[4,4, 0],
	[4,5, 0],[4,6, 0],[4,7, 0],[4,8, 0],[4,9,99],
	[3,0,99],[3,1, 1],[3,2, 1],[3,3, 1],[3,4, 1],
	[3,5, 1],[3,6, 1],[3,7, 1],[3,8, 1],[3,9,99],
	[2,0,99],[2,1, 4],[2,2, 2],[2,3, 3],[2,4, 5],
	[2,5, 6],[2,6, 3],[2,7, 2],[2,8, 4],[2,9,99],
	[1,0,99],[1,1,99],[1,2,99],[1,3,99],[1,4,99],
	[1,5,99],[1,6,99],[1,7,99],[1,8,99],[1,9,99],
	[0,0,99],[0,1,99],[0,2,99],[0,3,99],[0,4,99],
	[0,5,99],[0,6,99],[0,7,99],[0,8,99],[0,9,99]]).
%------------------------------------------------------------------
%FIN
