let classe u p = 
if u > 0 then (u mod p) else p + (u mod p);;

let inversionmodulaire n p =
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

let test p = 
for i= 1 to p-1 do 
if i* inversionmodulaire i p mod p = 1 then ()
else failwith "erreur"
done;
;;

inversionmodulaire 1 17;;
test 23;;
inversionmodulaire (-1) 2;;
(-1) mod 2;;
