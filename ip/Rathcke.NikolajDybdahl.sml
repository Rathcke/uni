(* Nikolaj Dybdahl Rathcke *)

infixr $
fun a $ b = a b;

(* Opgave 1 *)
(* 1a - erPalindrom
 * string -> bool
 * Givet en string, returneres true hvis det er et palindrom.
 *)
fun erPalindrom xs = (implode $ rev $ explode(xs)) = xs;

(* 1b - erUdvidetPalindrom
 * string -> bool
 * Givet en string, returneres true hvis det er et palindrom, hvor der ikke er
 * forskel på store og små bogstaver og øvrige tegn tælles ikke med.
 *)
fun erUdvidetPalindrom xs =
                  erPalindrom $ implode $ (List.filter (fn p => Char.isLower p)
                               (map (fn q => Char.toLower q) (explode(xs))));

(* Opgave 2 *)
(* 2a - korrekt
 * rute -> bool
 * Givet en rute, eturneres true hvis ruten ikke indeholder en delrute
 * Frem (d,r), hvor d<=0.
 *)
datatype rute = Stop | Frem of int * rute | Drej of int * rute

fun korrekt (Frem (d,r)) = d >= 0
  | korrekt r = true;

(* 2b - laengde
 * rute -> int
 * Givet en rute, returnerer laengden af alle d på formen Frem (d,r).
 *)
fun laengde Stop = 0
  | laengde (Frem (d,Stop)) = d
  | laengde (Drej (d,r)) = 0 + laengde(r)
  | laengde (Frem (d,r)) = d + laengde(r);

(* 2c - erNormaliseret
 * rute -> bool
 * Givet en rute, returneres true hvis ruten ikke indeholder nogle delruter.
 *)
fun erNormaliseret Stop = true
  | erNormaliseret (Frem (0,r)) = false
  | erNormaliseret (Drej (0,r)) = false
  | erNormaliseret (Frem (d,(Frem (d2,r)))) = false
  | erNormaliseret (Drej (g,(Drej (g2,r)))) = false
  | erNormaliseret (Drej (g,r)) = if g > ~180 andalso g <= 180
                                  then erNormaliseret(r)
                                  else false
  | erNormaliseret (Frem (d,r)) = erNormaliseret (r);

(* 2d - normaliserRute
 * rute -> rute
 * Givet en rute, returneres den rute hvor alle delruter er fjernet eller sat
 * sammen.
 *)
fun shortenRoute (Frem (0,r)) = shortenRoute(r)
  | shortenRoute (Drej (0,r)) = shortenRoute(r)
  | shortenRoute (Frem (d,(Frem (d2,r)))) = shortenRoute(Frem (d+d2,r))
  | shortenRoute (Drej (g,(Drej (g2,r)))) = shortenRoute(Drej (g+g2,r))
  | shortenRoute (Drej (g,r)) = if g > 180
                               then shortenRoute (Drej(g-360,r))
                               else if g <= ~180
                                    then shortenRoute (Drej(360+g,r))
                                    else Drej(g,shortenRoute(r))
  | shortenRoute (Frem (d,r)) = Frem (d,shortenRoute(r))
  | shortenRoute r = Stop

fun normaliserRute r = if erNormaliseret $ shortenRoute(r)
                       then shortenRoute(r)
                       else normaliserRute $ shortenRoute(r);

(* Opgave 3 *)
(* 3a - kvadratfrit
 * int -> bool
 * Givet et heltal, returneres true hvis tallet er kvadratfrit. Der kastes
 * en exception Domain hvis ikke n er over 0.
 *)
load "Math";
load "Real";
fun primtalsFaktorisering (1,k) = []
  | primtalsFaktorisering (n,k) = if Real.fromInt k > Math.sqrt(Real.fromInt n)
                                  then [n]
                                  else if n mod k = 0
                                       then k::primtalsFaktorisering(n div k,2)
                                       else primtalsFaktorisering(n,k+1);
local
fun oneOccurence [] = true
  | oneOccurence (x::xs) = if List.exists (fn z => z=x) xs
                          then false
                          else oneOccurence(xs)
in
  fun kvadratfrit n = if n <= 0
                    then raise Domain
                    else oneOccurence $ primtalsFaktorisering(n,2)
end;

(* 3b
 * Beregningstrinne for kvadratfrit 21 og 54 er vist nedenfor.
 *
 * For kvadratfrit 21:
 * kvadratfrit 21 =
 * primtalsFaktorisering (21,2) = [3,7]
 * oneOccurence [3,7] = true
 *
 * For kvadratfrit 54:
 * kvadratfrit 54 =
 * primtalsFaktorisering (54,2) = [2,3,3,3]
 * oneOccurence [2,3,3,3] = false
 *)

(* 3c - maksKvadratfrit
 * int -> int
 * Givet et heltal, returneres den største divisor der er kvadratfrit. Der
 * kastes en exception Domain hvis ikke n er større end 0.
 *)
local
fun removeDouble [] = []
  | removeDouble (x::xs) = if List.exists (fn z => z=x) xs
                          then removeDouble xs
                          else x::removeDouble xs
in
  fun maksKvadratfrit n = if n <= 0
                          then raise Domain
                          else foldl op* 1
                              (removeDouble $ primtalsFaktorisering (n,2))
end;

(* Opgave 4 *)
(* 4a - erPermutationAf
 * ''a list * ''a list -> bool
 * Givet 2 lister, returneres true hvis de begge er permutationer af hinanden.
 *)
fun erPermutationAf ([],[]) = true
  | erPermutationAf (xs,ys) =
                not (List.exists (fn r => r = false)
                       (map (fn p => List.exists (fn q => p=q) ys) xs)) andalso
                not (List.exists (fn r => r = false)
                       (map (fn p => List.exists (fn q => p=q) xs) ys));

(* 4b - antalPermutationer
 * ''a list -> int
 * Givet en liste, returneres det mulige antal permutationer der kan laves.
 *)

fun listMinusList (x,[]) = []
  | listMinusList (x,y::ys) = if x=y
                              then listMinusList(x,ys)
                              else y::listMinusList(x,ys)
local
fun fact 0 = 1
  | fact n = n * fact (n-1)

fun numberOccurences [] = []
  | numberOccurences (x::xs) = List.length(List.filter (fn z => z=x) (x::xs))
                                ::(numberOccurences $ listMinusList(x,(x::xs)))
in
fun antalPermutationer xs =
                fact(List.length(xs)) div
                   (foldl op* 1 (map (fn z => fact z) (numberOccurences(xs))));
end;

(* 4c - antalPermutationerNy
 * ''a list -> int
 * Givet en liste beregnes hvor mange permutationer der kan laves og tager
 * hensyn for overflow hvis resultatet kan vises i Mosml.
 *
 * Ikke lavet. Dog er lavet en funktion som returnerer en liste af lister der
 * fås ved at bruge hvert element i nævneren forkortet på tælleren som er et
 * tals fakultet repræsenteret i en liste. Der bliver altså fundet gcd af
 * et element i tæller og et fra nævneren. Dette divideres tællerens element
 * med og erstatter dette element med det nye. Der bliver også returneret en
 * 'rest' som ikke er brugt, som jeg så ville køre det hele på igen.
 * Ville have den til at køre på samme liste når den var kørt igennem, så der
 * kun blev returneret en enkelt. Derved ville man også kunne køre resten på
 * det.
 *
 * fun gcd2 (0,n) = n
 *   | gcd2 (m,n) = gcd2 (n mod m,m)
 *
 * fun factToList [] = []
 *   | factToList (x::[]) = if x <> 1 then factToList(x::(x-1)::[]) else [1]
 *   | factToList (x::xs) = x::factToList(xs)
 *
 * fun replace (n,[],m) = []
 *   | replace (n,xs,m) = List.take(xs,m)@(List.drop(xs,m+1))
 *
 * fun shortenFrac ([],ys,m) = []
 *   | shortenFrac (x::xs,ys,m) = let val k = List.nth(ys,m)
 *                    in
 *                       if x <= k
 *                       then ((k div gcd2(k,x))::replace(k,ys,m),x div gcd2(k,x))::shortenFrac(xs,ys,0)
 *                       else shortenFrac (x::xs,ys,m+1)
 *                    end;
 *
 * fun antalPermutationerNy xs = shortenFrac(numberOccurences(xs),factToList[List.length(xs)],0)
 *)

(* Opgave 5 *)
(* 5a - grupper
 * ('a -> ''b) -> 'a list -> 'a list list
 * Givet en funktion samt en liste, returneres en liste af liste, hvor hver
 * liste indeholder de værdier fra den oprindelige liste der giver samme
 * funktionsværdi.
 *)
local
fun solveFun f l = map (fn q => f q) l

fun pairUp [] = []
  | pairUp (x::xs) = (List.filter (fn z => z=x) (x::xs))
                        ::(pairUp $ listMinusList(x,(x::xs)))

fun combine [] = []
  | combine (x::xs) = x@combine(xs)

fun matchUp f ([], l) = []
  | matchUp f (x::xs,l) = (combine(map (fn q => if (f q)=List.hd(x)
                                                then [q]
                                                else []) l)) ::matchUp f (xs,l)
in
  fun grupper f l = matchUp f (pairUp $ solveFun f l,l)
end;

(* 5b - gentag
 * ('a -> 'a) -> 'a -> 'a
 * givet en funktion samt en værdi, returneres den værdi der fås lige før
 * funktionen kaster en exception.
 *)
fun gentag f x = (gentag f (f x)) handle _ => f x

(* 5c - gcd
 * int * int -> int
 * Givet 2 heltal, returneres deres største divisor ved brug af funktionen
 * gentag.
 *)
fun gcd (m,n) = #2 (gentag (fn (p,q) => (q mod p,p)) (m,n))

(* Opgave 6 *)
(* 6a - t7
 * Der er konstrueret et træ på datatypen 'a trae.
 *)
datatype 'a trae = K of 'a * ('a trae list)

val t7 = K(3,
            [K(4,
                [K(7,[]),K(1,[]),K(5,
                                    [K(6,[]),K(7,[])]
                                                     )])]);

(* 6b - praeorden
 * 'a trae -> 'a list
 * Givet et træ, returneres den liste af elementerder fås ved at løbe igennem
 * træet fra 'største' element.
 * Der er brugt side 121 i H. R.
 *)
fun praeorder (K (s,[])) = [s]
  | praeorder (K (s,t)) = s::(helpFun t)

and helpFun [] = []
  | helpFun (x::xs) = (praeorder x) @ (helpFun xs);

(* 6c - erstat
 * 'a trae * 'b list -> 'b trae * 'b list
 * Givet et træ samt en liste, returnerer erstatter elementerne i træet med
 * elementerne fra listen samt den resterende liste.
 *
 * Ikke løst.
 *)


(* 6d - sorter
 * int trae -> int trae
 * Givet et træ af heltal returneres det træ, hvor heltallene er sorteret efter
 * størrelse.
 *
 * Nedenfor er en løsning til 6d givet, hvis jeg korrekt havde løst 6c og havde
 * funktionen erstat.
 *
 * load "Listsort";
 * fun intSort xs = Listsort.sort (fn (x,y) => Int.compare(x,y)) xs
 *
 * fun sorter t = #1 (erstat $ intSort $ praeorder t)
 *)

(* Opgave 7 *)
(* 7a - inverterPPM
 * string -> string -> unit
 * Producerer et inverst billede.
 *
 * Ikke løst.
 *)

(* Opgave 8 *)
(* 8a - ListeTabel
 * Struktur der implementerer signaturen SYMBOLTABEL.
 * tom er den tomme list.
 * indsaet, givet en liste samt en tupel sætter tuplen ind i en liste.
 * find, givet en liste t og en string n returnerer den værdi som matcher for n
 * i t.
 * Typen er præcis når strukturen er uigennemsigtig.
 *)
signature SYMBOLTABEL =
sig
  type 'vaerdi tabel
  val tom : 'vaerdi tabel
  val indsaet : 'vaerdi tabel -> (string * 'vaerdi) -> 'vaerdi tabel
  val find : 'vaerdi tabel -> string -> 'vaerdi option
end;

structure ListeTabel :> SYMBOLTABEL =
struct
  type 'vaerdi tabel = (string * 'vaerdi) list
  val tom = []
  fun indsaet t (x,y) = (x,y) :: t
  fun find [] n = NONE
    | find (t::ts) n = if n = (#1 t)
                       then SOME (#2 t)
                       else find ts n
end;

(* 8b - FunTabel
 * Struktur der implementerer SYMBOLTABEL så tabel er en funktionsværdi.
 *
 * indsaet er ikke løst.
 *
 * structure FunTabel :> SYMBOLTABEL =
 * struct
 * type 'vaerdi tabel = string -> 'vaerdi option
 * fun tom t = NONE
 * fun indsaet t (n, m) = (* *)
 * fun find t = t
 * end;
 *)