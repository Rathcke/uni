Sets
    i /a*j/;

alias (i,j)

Sets
    arcs(i,i);

arcs('a','b')=yes;
arcs('a','e')=yes;
arcs('a','h')=yes;
arcs('b','c')=yes;
arcs('b','e')=yes;
arcs('c','d')=yes;
arcs('c','e')=yes;
arcs('c','f')=yes;
arcs('d','f')=yes;
arcs('d','g')=yes;
arcs('e','f')=yes;
arcs('e','h')=yes;
arcs('e','i')=yes;
arcs('f','g')=yes;
arcs('f','i')=yes;
arcs('f','j')=yes;
arcs('g','j')=yes;
arcs('h','i')=yes;
arcs('i','j')=yes;

arcs('b','a')=yes;
arcs('e','a')=yes;
arcs('h','a')=yes;
arcs('c','b')=yes;
arcs('e','b')=yes;
arcs('d','c')=yes;
arcs('e','c')=yes;
arcs('f','c')=yes;
arcs('f','d')=yes;
arcs('g','d')=yes;
arcs('f','e')=yes;
arcs('h','e')=yes;
arcs('i','e')=yes;
arcs('g','f')=yes;
arcs('i','f')=yes;
arcs('j','f')=yes;
arcs('j','g')=yes;
arcs('i','h')=yes;
arcs('j','i')=yes;

parameters
   c(i,i);

c('a','b')=2;
c('a','e')=4;
c('a','h')=2;
c('b','c')=2;
c('b','e')=1;
c('c','d')=3;
c('c','e')=3;
c('c','f')=2;
c('d','f')=2;
c('d','g')=2;
c('e','f')=1;
c('e','h')=3;
c('e','i')=2;
c('f','g')=3;
c('f','i')=3;
c('f','j')=1;
c('g','j')=1;
c('h','i')=1;
c('i','j')=2;

c('b','a')=2;
c('e','a')=4;
c('h','a')=2;
c('c','b')=2;
c('e','b')=1;
c('d','c')=3;
c('e','c')=3;
c('f','c')=2;
c('f','d')=2;
c('g','d')=2;
c('f','e')=1;
c('h','e')=3;
c('i','e')=2;
c('g','f')=3;
c('i','f')=3;
c('j','f')=1;
c('j','g')=1;
c('i','h')=1;
c('j','i')=2;

Variable
    z;

Integer variable
    t(i);

Binary variable
    x(i,j);

Equations
    obj
    const1(i)
    const2(i)
    const3(i,j)
    const4(i)
    const5(i);

    obj.. z =e= sum((i,j)$arcs(i,j), c(i,j)*x(i,j));
    const1(j).. sum(i$arcs(i,j), x(i,j)) =e= 1;
    const2(i).. sum(j$arcs(i,j), x(i,j)) =e= 1;
    const3(i,j)$(arcs(i,j) and ord(i)>=1 and ord(j)>=2).. t(i)+1-card(i)*(1-x(i,j)) =l= t(j);
    const4(i).. t(i) =l= card(i);
    const5(i).. t(i) =g= 1;

Model tsp /all/;

solve tsp using mip minimizing z;

display
    t.l, z.l;