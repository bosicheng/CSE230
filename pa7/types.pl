envtype(Bindings,Var,Ty) :- fail.

int_op(plus).
int_op(minus).
int_op(mul).
int_op(div).

comp_op(eq).
comp_op(neq).
comp_op(lt).
comp_op(leq).

bool_op(and).
bool_op(or).

typeof(_,const(_),X) :- fail.
typeof(_,boolean(_),X) :- fail.
typeof(_,nil,X) :- fail.
typeof(Env,var(X),T) :- fail.

typeof(Env,bin(E1,Bop,E2),int) :- int_op(Bop), fail.
typeof(Env,bin(E1,Bop,E2),bool) :- comp_op(Bop), fail.
typeof(Env,bin(E1,Bop,E2),bool) :- bool_op(Bop), fail.
typeof(Env,bin(E1,cons,E2),list(T)) :- fail.

typeof(Env,ite(E1,E2,E3),T) :- fail.
typeof(Env,let(X,E1,E2),T) :- fail.
typeof(Env,letrec(X,E1,E2),T) :- fail.
typeof(Env,fun(X,E),arrow(T1,T2)) :- fail.
typeof(Env,app(E1,E2),T) :- fail.