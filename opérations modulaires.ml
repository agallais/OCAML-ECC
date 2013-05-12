(*inversionmodulaire n p: ici p est un nombre premier, et ceci pour des besoin de commodité*)
let euklide n p =    
let i= ref n and j = ref p and l = ref [] in
    while ( !i<>1 && !j<>1) do 

      l := (!j/!i,!j )::!l;
      j:= !i;
      i:=!j mod !i;

    done;
let rec bezout (r,a) (q,b) l = 
    match l with 
        | [] -> (r,a) (q,b)
        |(k,p)::l2 -> bezout (q, (b- k*a)) (p,a) l2
        in
let (k,q)::_ = !l in
bezout (!j,-k) (q,1) !l ;; (*Le programme bezout cherche à résoudre l'équation diophantienne nu + pv =1, maintenant qu'on
a fait la suite des divisions de l'algorithme d'Euclide*)

let inversionmodulaire n p = let ((a,u), (_,_))= euklide n p in   u mod p ;; 
    
(*Je crois qu'il n'est pas nécessaire de vérifier que a est bien égal à n car l'inégalité r<q est bien vérifiée à chaque étape dans le déroulement du progeramme bezout*)
