(*inversionmodulaire n p: ici p est un nombre premier, et ceci pour des besoin de commodité*)
let euklide n p =    
let i= ref n and j = ref p and l = ref [] in
    while ( !i<>1 && !j<>1) do 

      l := (!j/!i,!j mod !i)::!l;
      j:= !i;
      i:=!j mod !i;

    done;
let rec bezout 
