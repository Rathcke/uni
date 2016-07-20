$ONTEXT
We want to solve the LP problem

   max. 250 x_1 + 300 x_2
   s.t. 100 x_1 - 200 x_2 <= 325
         50 x_1 +  70 x_2 <= 300
         80 x_1 +  30 x_2 <= 275
        200 x_1 + 400 x_2 >= 300
              x_1,x_2 >= 0

or in matrix form

   max. c^T x
   s.t. A_ x <= b_
         x>=0

Note that we do not need to convert >= to <= when solving the problem in GAMS.
Therefore b_ and A_ is not the same as what we write in the GAMS model,
we just write down the matrix and the right hand side
when defining A and b.
$OFFTEXT

$onempty
* allow empty sets

Sets
    nodes  / a*j /
    j      / 1*4 /
    j1(j)  / 1*3 /
    j2(j)  / 4 /
    j3(j)  /  /
*   i = {1,2}          , 2 variables
*   j = {1,2,3,4}      , 4 constraints
*   j1 is {1,2,3} of j , constraint 1,2,3 "<=" inequalities
*   j2 is {4} of j     , constraint 4 ">=" inequality
*   j3 is empty        , no "=" constraints

set
nodes /a*j/;

Alias(nodes,nodes2);

set arcs(nodes,nodes2);

Parameters
    c(i)   / 1 2
             2 4 
             3 2
             4 2
             5 1
             6 3
             7 3
             8 2
             9 2
             10 2
             11 1
             12 2
             13 2
             14 3
             15 3
             16 1
             17 1
             18 1
             19 2 /
    b(j)   / 1 325
             2 300
             3 275
             4 300 /
*    c = (250,300)
*    b = (325,300,275,300)
* IMPORTANT: KEEP NUMBERS UNDER EACH OTHER!!!

Table a(j,i)
      1    2    3    4    5    6    7    8    9    10
   1       2              4              2
   2  2         2         1
   3       2         3    3    2
   4            3              2    2
   5  4    1    3              1         3    2
   6            2    2    1         3         3    1
   7                 2         3                   1
   8  2                   3                   1
   9                      2    3         1         2
   10                          1    1         2

* a = 100 -200
*     50  70
*     80  30
*     200 400
* IMPORTANT: KEEP NUMBERS UNDER EACH OTHER!!!

Variable
    z
* declare z as variable so it can be objective value

Positive variable
    x(i)
* Declare x and constraint x>=0

Equations
    obj            objective function
    leq_const(j1)  less than constraints
    geq_const(j2)  greater than constraints
    eq_const(j3)   equal to constraints;
* declare objective function equation and constraints

    obj..       z =e= sum(i, c(i)*x(i));
    leq_const(j1) ..  sum(i, a(j1,i)*x(i)) =l= b(j1);
    geq_const(j2) ..  sum(i, a(j2,i)*x(i)) =g= b(j2);
    eq_const(j3)  ..  sum(i, a(j3,i)*x(i)) =e= b(j3);
* define objective function equation and constraints

Model my_model /obj, leq_const, geq_const, eq_const/ ;
* define the model "my_model" using the given equations

solve my_model using lp maximizing z;
* solve my_model using lp maximization
* use "minimizing z" for min problem

$Ontext
Advanced part begins here
$Offtext


Parameters
    x_(i)        primal decision variables
    y_(j)        dual decision variables
    w_(j)        primal slack variables
    z_(i)        dual slack variables
    xi           objective value;
* Define parameters to save the value of primal and dual variables as well as objective value.

x_(i)=x.l(i);
z_(i)=-x.m(i);
w_(j1)=b(j1)-leq_const.l(j1);
w_(j2)=-b(j2)+geq_const.l(j2);
w_(j3)=b(j3)-eq_const.l(j3);
y_(j1)=leq_const.m(j1);
y_(j2)=geq_const.m(j2);
y_(j3)=eq_const.m(j3);
xi = z.l;
* Compute or read the values of all the variables and the objective value

Display
    x_,w_,y_,z_,xi;
* Display the primal variables, the dual variables and the objective value


Parameters
    B_matrix(j,j)
    B_inv_matrix(j,j)
    B_inv_N_matrix(j,i)
    N_matrix(j,i);
* Define parameters to save the matrices in the optimal basis

alias(i,ii);
alias(j,jj);
* Allow the set i to be referenced by ii and j to be referenced by jj

Scalar
   index,s1,s2;
* Define the scalars used for indices
s1=1;
s2=1;

for (index=1 to card(i),
   loop(i,
      if(x_(i)$(ord(i)=index) >0,
         B_matrix(j,jj)$(ord(jj)=s1)=a(j,i)$(ord(i)=index);
      else
        N_matrix(j,i)$(ord(i)=s2)=a(j,i)$(ord(i)=index);
      );
   );
   if(sum(ii,x_(ii)$(ord(ii)=index)) >0,
      s1=s1+1;
   else
      s2=s2+1;
   );
);
* Advanced loop to save the first part of the B and N matrix corresponding to the optimal basis

for (index=1 to card(j),
   loop(j,
      if(y_(j)$(ord(j)=index) =0,
         B_matrix(j,jj)$((ord(j)=index) and (ord(jj)=s1))=1;
      else
         N_matrix(j,i)$((ord(j)=index) and (ord(i)=s2))=1;
      );
   );
   if(sum(jj,y_(jj)$(ord(jj)=index)) =0,
      s1=s1+1;
   else
      s2=s2+1;
   );
);
* Advanced loop to save the last part of the B and N matrix corresponding to the optimal basis

display B_matrix,N_matrix;
* Display the B and N matrix corresponding to the optimal basis

execute_unload 'gdxforinverse.gdx' j,B_matrix;
execute 'invert gdxforinverse.gdx j B_matrix gdxfrominverse.gdx B_inv_matrix';
execute_load 'gdxfrominverse.gdx' , B_inv_matrix;
* Invert the B matrix

display B_inv_matrix;
* Display the inverted B matrix

loop(i,
   loop(j,
      B_inv_N_matrix(j,i)=sum(jj,B_inv_matrix(j,jj)*N_matrix(jj,i));
   );
);
* Compute B^(-1) . N

display B_inv_N_matrix;
* Display B^(-1) . N