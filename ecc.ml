let classe u p = 
if u >= 0 then (u mod p) else p + (u mod p);;

classe 0 17;;

let inversionmodulaire2 n p =
if classe n p = 1 then 1 
else if (n mod p) = 0 then failwith "Element non inversible"
else
let i= ref n and j = ref p and c= ref 0 and l = ref [] in
while ( !i<>1 && !j<>1) do
l := ((!j/(!i),!j )::!l);
c:= !i;
i:=!j mod !i;
j:=!c;
done;
let rec bezout ((r,a),(q,b),l) =
    match l with
        | [] -> ((r,a),(q,b))
        |(k,p)::l2 -> bezout ((q,(b-k*a)),(p,a),l2)
in 
let (k,r)::q = !l in
let (_,u),(_,_)=bezout ((!c,-k),(r,1),q)in 
classe u p
;;

let inverses17 = Array.make 17 0;;

for i=1 to 16 do
inverses17.(i)<- inversionmodulaire2 i 17;
done;
inverses17;;


let inversionmodulaire n=
inverses17.(n);;

 
let test p = 
for i= 1 to p-1 do 
if i* inversionmodulaire2 i p mod p = 1 then ()
else failwith "erreur"
done;
;;
type point = O|P of  int*int ;;

type ecc = C of int*int;;

let courbep (e,p)=
  let C(a,c) = e in  
let l = ref [] in
for i = 0 to p-1 do
for j = 0 to p-1 do

if (j*j mod p) = ((i*i*i + a*i + c) mod p) then l:=P(i,j)::!l else ()

done;
done;
!l;;

let e = C (5,5);;
let p = 17;;


let l=courbep (e,p) ;;

(* On définit maintenant la fonction d'addition*)

let somme (p1,p2,e)= 
let C(a,c)= e in 
match p1,p2 with 
  |O,_-> p2
  |_,O-> p1
  |P(x1,y1), P(x2,y2) -> if (classe x1 p) = (classe x2 p) then 
if classe y1 p = classe (-y2) p then O 
else (let m= (3*x1*x1+a)*(inversionmodulaire (2*y1) ) in 
      let x3 = m*m-x1-x2 in
      let y3= m*(x3-x1)+y1 in
P((classe x3 p),(classe y3 p)))
    else let m = (y1-y2)*(inversionmodulaire (x1-x2)) in
   let x3 = (m*m)-x1-x2 in
	 let y3 = m*(x3-x1) +y1 in
P( classe x3 p , classe y3 p);;

somme (O,O,e);;
somme (P(3,8),P(4,9),C(5,5));;

(* Attaquons nous au problème du logarithme discret *)

let l = courbep(e,p);; 

let n = List.length l;;

let multiples( m,c,p) = 
  let multiple = Array.make 14 O in
multiple.(0)<-m;
for i= 1 to 13 do
multiple.(i)<- somme (multiple.(i-1),m,c,p);
done;
multiple;;

somme (P(1,1),P(16,3),C((-5),5),17);;
(*la phrase ci dessus refuse de compiler ce qui est étrange...*)
somme (P(16,3),P(16,3),C(-5,5),17);;
36849 mod p;;
classe (10- 32) 17;;
