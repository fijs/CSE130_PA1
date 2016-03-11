%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helpers

%isin(X,L) is true if X appears in L
isin(X,[X|_]).
isin(X,[_|T]) :- isin(X,T).

% zip(L1,L2,L3) is true if L3 is the result of interleaving L1 and L2
% e.g. zip([1,2],[3,4],[1,3,2,4])   is true
zip([],[],[]).
zip([H1|T1],[H2|T2],[H1,H2|T]) :- zip(T1,T2,T).

% assoc(L,K,V) is true if L is a list of 2-element lists and one of them is [K,V]
% e.g. assoc([[key1,value1],['a',1],[3,4]], 3, 4) is true
assoc([[X,Y]|_],X,Y).
assoc([_|T],X,Y) :- assoc(T,X,Y).

% remove_duplicates(L1,L2) is true if L2 is the result of removing all duplicate elements from L1.
% The remaining elements should be in the original order.
% e.g. remove_duplicates([1,1,2,2,3,3,4,4],[1,2,3,4]) is true
clean([],Soln,Y) :- reverse(Y,Soln).
clean([H|T],Soln,Y) :- isin(H,Y),!,clean(T,Soln,Y).
clean([H|T],Soln,Y) :- clean(T,Soln,[H|Y]).
remove_duplicates(L1,L2) :- clean(L1,L2,[]). 

% union(L1,L2,L3) is true if L3 is the set union of L1 and L2. 
% There should be no duplicates in L3.
% e.g. union([1,2,3],[2,3,4],[1,2,3,4]) is true
union(L1,L2,L3) :- append(L1,L2,L),remove_duplicates(L,L3). 

% intersection(L1,L2,L3) is true if L3 is the set intersection of L1 and L2.
% There should be no duplicates in L3.
% e.g. intersection([1,2,3],[2,3,4],[2,3]) is true
its([],_,X,Y) :- reverse(X,Y).
its([H|T],L,X,Y) :- isin(H,L),!,its(T,L,[H|X],Y).
its([_|T],L,X,Y) :- its(T,L,X,Y).
intersection(L1,L2,L3) :- its(L1,L2,[],L3).

% worked_at(X,Y) is true if employee X is in the list of Employees E for taqueria Y
worked_at(X,Y) :- taqueria(Y,E,_),isin(X,E).

% sumIngredients(L,K) is true if the sum of the ingredients in L is equal to K.
sumIngredients([],0).
sumIngredients([H|T],C) :- sumList(T,Ct),cost(H,ingCost), C is Ct + ingCost.

% checkIngredients(I,L) is true if every element in L is in I.
checkIngredients([],_).
checkIngredients([H|T],L) :- isin(H,L),checkIngredients(T,L).

% checkIngredients2(I,L) is true if none of the items in L are found in I.
checkIngredients2([],_).
checkIngredients2([H|T],L) :- not(isin(H,L)),checkIngredients2(T,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 1: Facts

cost(carne_asada,3).
cost(lengua,2).
cost(birria,2).
cost(carnitas,2).
cost(adobado,2).
cost(al_pastor,2).
cost(guacamole,1).
cost(rice,1).
cost(beans,1).
cost(salsa,1).
cost(cheese,1).
cost(sour_cream,1).
cost(taco,1).
cost(tortilla,1).
cost(sopa,1).


ingredients(carnitas_taco, [taco,carnitas, salsa, guacamole]).
ingredients(birria_taco, [taco,birria, salsa, guacamole]).
ingredients(al_pastor_taco, [taco,al_pastor, salsa, guacamole, cheese]).
ingredients(guacamole_taco, [taco,guacamole, salsa,sour_cream]).
ingredients(al_pastor_burrito, [tortilla,al_pastor, salsa]).
ingredients(carne_asada_burrito, [tortilla,carne_asada, guacamole, rice, beans]).
ingredients(adobado_burrito, [tortilla,adobado, guacamole, rice, beans]).
ingredients(carnitas_sopa, [sopa,carnitas, guacamole, salsa,sour_cream]).
ingredients(lengua_sopa, [sopa,lengua,beans,sour_cream]).
ingredients(combo_plate, [al_pastor, carne_asada,rice, tortilla, beans, salsa, guacamole, cheese]).
ingredients(adobado_plate, [adobado, guacamole, rice, tortilla, beans, cheese]).

taqueria(el_cuervo, [ana,juan,maria], 
        [carnitas_taco, combo_plate, al_pastor_taco, carne_asada_burrito]).

taqueria(la_posta, 
        [victor,maria,carla], [birria_taco, adobado_burrito, carnitas_sopa, combo_plate, adobado_plate]).

taqueria(robertos, [hector,carlos,miguel],
        [adobado_plate, guacamole_taco, al_pastor_burrito, carnitas_taco, carne_asada_burrito]).

taqueria(la_milpas_quatros, [jiminez, martin, antonio, miguel],  
        [lengua_sopa, adobado_plate, combo_plate]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 1: Rules

% available_at(X,Y) is true if the item X is available at taqueria Y
% calls the taqueria structure to get the list of items available (L) at Y
% then, it checks to see if X is in L using isin().
available_at(X,Y) :- taqueria(Y,_,L),isin(X,L).

% multi_available(X) is true if we can make a list "Bag" of size >= 2
% and Bag contains taquerias Y.  
% Bag=[_,_|_] means "Bag has two or more"
multi_available(X) :- bagof(Y,available_at(X,Y),Bag),Bag=[_,_|_].

% overworked(X) is true if we can make a list "Bag" of size >= 2
% and Bag contains taquerias Y. Uses function worked_at(X,Y) which 
% is defined in the helper functions and checks that a worker X is an 
% employee at taqueria Y.
overworked(X) :- bagof(Y,worked_at(X,Y),Bag),Bag=[_,_|_].

% total_cost(X,K) is true if the sum of the costs of the ingredients for X is
% equal to K. Gets the ingredients for X, then calls sumIngredients to get the 
% sum of costs of ingredients and compares to K.
total_cost(X,K) :- ingredients(X,L),sumIngredients(L,K).

% has_ingredients(X,L) is true if the item X has all the ingredients listed in L 
% Intersection of L,I should be equal to L
has_ingredients(X,L) :- ingredients(X,I),checkIngredients(L,I).

% avoids_ingredients(X,L) is true if the item X has none the ingredients listed in L
% Intersection of L,I should be empty 
avoids_ingredients(X,L) :- ingredients(X,I),checkIngredients2(I,L).

% p1(L,X) is true if the item I has all the ingredients listed in X. 
% Then, we put item I in list L.
% p1(L,X) :- bagof(I,has_ingredients(_,X),L).
% p1(L,X) :- bagof(_,has_ingredients(I,X),L).
p1(L,X) :- bagof(I,has_ingredients(I,X),L).

% p2(L,Y) is true if the item I has none of the ingredients listed in Y. 
% Then, we put item I in list L.
p2(L,Y) :- bagof(I,avoids_ingredients(I,Y),L).

% find_items(L,X,Y) is true if the list L contains all of the items that have 
% all of the ingredients in list X and none of the ingredients in list Y.
% This rule makes use of helper predicates p1 and p2, then gets the intersection
% of the lists that p1 and p2 return.
find_items(L,X,Y) :- p1(L1,X),p2(L2,Y),intersection(L1,L2,L). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
