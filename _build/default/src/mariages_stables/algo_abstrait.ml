open Definitions 

module type PIOCHE = sig
  type 'a t
    val of_list: 'a list -> 'a t 
    val pioche : 'a t -> 'a option 
    val defausse : 'a -> 'a t -> unit 
end

module Pile : PIOCHE = struct
  
  type 'a t = 'a list ref (* <- à modifier *)

  let of_list l = 
    let res = ref [] in
    let rec aux li = match li with 
    | [] -> res
    | [x] -> res := !res @ [x]; aux []
    | hd::tl -> res := !res @ [hd]; aux tl in
    aux l

  let pioche p = match !p with 
    | [] -> None
    | hd::tl -> p := tl; Some hd

  let defausse x p = 
    p := [x] @ !p

end

module File : PIOCHE = struct
  type 'a t = 'a list ref (* <- à modifier *)

  let of_list l = 
    let res = ref [] in
    let rec aux li = match li with 
    | [] -> res
    | [x] -> res := !res @ [x]; aux []
    | hd::tl -> res := !res @ [hd]; aux tl in
    aux l

  let pioche p = match !p with 
    | [] -> None
    | hd::tl -> p := tl; Some hd

  let defausse x p = 
    p := !p @ [x]
end


module Algo (P:PIOCHE) = struct
  let run entree =
  (* DEBUT INIT *)
    (* constante *)
    let n = entree.n in
    let omega = -1 in (* representation d'un homme célibataire *)

    let pref = ref entree.liste_appel_de in (* pointer sur le tableau modifiable *)

    (* Mes fonctions *)
    let set_up_mariages = (* fiancer les x' à omega *)
      let rec aux acc nb_couples i = match nb_couples with
        | 0 -> acc
        | _ -> aux (acc @ [(omega, i)]) (nb_couples-1) (i+1) in
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
    
    let rec isAlone a' li = match li with (* On retourne le copain d'une femme ou -1 si elle est célibataire *)
      | [] -> omega
      | (x, y)::_ when y = a' -> x
      | _::tl -> isAlone a' tl in

    (* Nouvelles fonctions *)
    let len_to_list i =
      let rec aux res k = match k with
      | x when x = 0 -> [x] @ res
      | x -> aux ([x] @ res) (k-1) in
      aux [] (i - 1) in

    let pile = P.of_list (len_to_list n) in (* homme list ref *)

    let elm = ref (P.pioche pile) in
    
    (* FIN INIT *)
    
    while !elm <> None do
      let x = Option.get (!elm) in
      let x' = !pref.(x).(0) in
      let evincable = ref (isAlone x' !mariages) in

      if !evincable <> omega (* Si la femme est dejà en couple *)
      then 
        if entree.prefere.(x') !evincable x (* On regarde si elle prefere son copain actuel *)
        then
          evincable := x; (* Et donc l'homme courant est l'evincable *)

      if !evincable <> x (* C'est qu'il s'agit de l'indesirable ou bien que le prétendant a gagné *)
      then 
        mariages := fiance !mariages x x'; (* Si l'evincable n'est pas l'homme courant alors on modifie *)

      if !evincable <> omega (* On retire le premier choix de l'evincable si ce n'est pas l'indésirable *)
      then begin
        pref := remove_first_elem !pref !evincable;
        P.defausse !evincable pile
      end;

      elm := P.pioche pile
    done;
    !mariages
end