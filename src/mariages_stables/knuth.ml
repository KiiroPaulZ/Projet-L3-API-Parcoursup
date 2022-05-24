open Definitions

let algo ?(affiche_config=false) entree =
  (* DEBUT INIT *)
  (* constante *)
  let n = entree.n in (* Nombre de prétendant *)
  let omega = -1 in (* representation de l'indesirable *)

  (* variables *)
  let x = ref 0 in (* Homme courant *)
  let x' = ref 0 in (* Femme courante *)
  let k = ref 0 in (* Iteration des hommes *)

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

  (* FIN INIT *)

    while !k < n do 
      x := !k; (* Fiancé courant *)
      while !x <> omega do
        x' := !pref.(!x).(0); (* x' = meilleur choix sur la liste de x *)

        let evincable, _ = (List.nth !mariages !x') in (* On recupere potentiellement l'évincé *)

        if evincable = omega
        then mariages := fiance !mariages !x !x'
        else 
          if entree.prefere.(!x') !x evincable   (* Le fiancé courant de la femme courante est il potentiellement meilleur que l'actuel ? *)
          then                                   (* NOTE: Les choix seront toujours meilleurs qu'omega *)
            mariages := fiance !mariages !x !x'; (* Les fiancer + mise à jour de mariage *)
          
        if evincable = omega || entree.prefere.(!x') !x evincable (* Il ne faut modifier l'homme courant que si le prétandant à gagner *)
        then x := evincable; (* Le fiancé evincé *)

        if !x <> omega
        then pref := remove_first_elem !pref !x; (* retirer x' de x, ce qui correspond à son premier choix *)

        (* Traitement de l'affichage de config courante *)

        let someMariage = convertToOption !mariages in (* Conversions des couples formés en couples d'options *)
        let config = { rang_appel_de = current_choice !pref ; fiance_de = current_fiance someMariage } in (* Configuration courante *)

        if affiche_config
        then print_configuration config;
      done;

      k := !k + 1;
    done;

  !mariages;; (* celebrer les mariages *)