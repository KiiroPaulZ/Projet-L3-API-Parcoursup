open Definitions
let algo ?(affiche_config=false) entree =
  (* DEBUT INIT *)
  (* constante *)
  let n = entree.n in (* Nombre de prétendant *)
  let omega = -1 in (* representation d'un homme célibataire *)

  let pref = ref entree.liste_appel_de in (* pointer sur le tableau modifiable *)

  (* Mes fonctions *)
  let set_up_mariages = (* fiancer les x' à omega *)
    let rec aux acc nb_couples i = match nb_couples with
      | 0 -> acc
      | _ -> aux (acc @ [(omega, i)]) (nb_couples-1) (i+1) 
    in
    aux [] n 0 in

  let mariages = ref set_up_mariages in (* pointer sur la liste à rafraichir *)

  let fiance li a a' = (* Modifie le fiancé d'une femme *)
    let rec aux preced li = match li with
    | [] -> li
    | (_, y)::tl when y = a' -> preced @ [(a, a')] @ tl 
    | ((_,_) as hd)::tl -> aux (preced @ [hd]) tl in
    aux [] li in

  let remove_first_elem arr x = (* retirer premier choix de l'homme courant *)
    let li = Array.to_list (Array.map Array.to_list arr) in (* Je simplifie la recherche et la suppression de mon element grace aux listes *)

    let rec aux li last i = match li with (* Je supprime ainsi le premier element de l'homme courant *)
    | [] -> last @ li
    | hd::tl when i = x -> last @ [List.tl hd] @ tl
    | hd::tl -> aux tl (last @ [hd]) (i + 1) in

    Array.of_list (List.map Array.of_list (aux li [] 0)) in (* List de List -> Array d'Array *)

  (* Nouvelles fonctions *)
  let rec allMarried li = match li with
    | [] -> true
    | (x, _)::_ when x = omega -> false
    | _::tl -> allMarried tl in

  let rec estFiance a li = match li with (* Regarde si l'homme est fiance *)
    | [] -> false
    | (x, _)::_ when x = a -> true
    | _::tl -> estFiance a tl in

  let rec isAlone a' li = match li with (* On retourne le copain d'une femme ou -1 si elle est célibataire *)
    | [] -> omega
    | (x, y)::_ when y = a' -> x
    | _::tl -> isAlone a' tl in


  (* Config affichage *)
  let current_choice tab = (* On cherche à savoir à l'instant t quel est le choix courant du fiancé *)
      let acc = Array.init n (fun _ -> 0) in
      let rec aux i = match i with
      | i when i = n -> acc
      | _ -> acc.(i) <- (n - Array.length tab.(i)); aux (i+1) in
      aux 0 in
    
  let current_fiance couples = (* On recupère uniquement les fiancés des femmes *)
      let rec aux acc cp = match cp with
      | [] -> Array.of_list acc
      | [(h,_)] -> aux (acc @ [h]) []
      | (h,_)::tl -> aux (acc @ [h]) tl in
      aux [] couples in  

  let convertToOption m =    
        let rec aux m last = match m with
        | [] -> last
        | (x,y)::tl when x = omega -> aux tl (last @ [(None, Some y)]) 
        | (x,y)::tl -> aux tl (last @ [(Some x, Some y)]) in
        aux m [] in

  

  (* Algo principal *)
  let couplages x =
    let li = ref [] in (* ajout d'une liste repertoriant les hommes qu'on ne parcours pas ce tour car ils ont eu un changement *)
    let rec aux x = match x with
      | x when x = n -> mariages := !mariages (* On a fini notre tour *)
      | x when (estFiance x !mariages) = true || List.mem x !li -> aux(x+1) (* L'homme courant est déjà fiancé *) (* Ajout de la condition s'il est dans la liste *)
      | x -> begin
              let x' = ref !pref.(x).(0) in (* Sa prétendante *)
              let evincable = ref (isAlone !x' !mariages) in (* La femme est elle en couple ? Si c'est le cas son fiancé est evincable *)

              if !evincable <> omega (* Si la femme est dejà en couple *)
              then 
                if entree.prefere.(!x') !evincable x (* On regarde si elle prefere son copain actuel *)
                then
                  evincable := x; (* Et donc l'homme courant est l'evincable *)

              if !evincable <> x
              then 
                mariages := fiance !mariages x !x'; (* Si l'evincable n'est pas l'homme courant alors on modifie *)

              if !evincable <> omega (* On retire le premier choix de l'evincable si ce n'est pas l'indésirable *)
              then 
                pref := remove_first_elem !pref !evincable;
                li := !li @ [!evincable]; (* Ajout de l'homme evince dans cette liste *)
              
              aux (x+1)
             end; in aux x in

  

  (* FIN INIT *)
  if n <> 0 then    
    while not (allMarried !mariages) do (* Tant qu'ils ne sont pas tous mariés *)
      (*let lastconfig = { rang_appel_de = current_choice !pref ; fiance_de = current_fiance someMariage } in*)

      couplages 0; (* Procédure *)
              
      (* Traitement de l'affichage de config courante *)
      let someMariage = convertToOption !mariages in (* Conversions des couples formés en couples d'options *)
      let config = { rang_appel_de = current_choice !pref ; fiance_de = current_fiance someMariage } in (* Configuration courante *)
      
      if affiche_config
      then print_configuration config;

    done
  else begin
    let someMariage = convertToOption !mariages in (* Conversions des couples formés en couples d'options *)
    let config = { rang_appel_de = current_choice !pref ; fiance_de = current_fiance someMariage } in (* Configuration courante *)
      
    if affiche_config
    then print_configuration config;
  end;

  !mariages;; (* celebrer les mariages *)



