
type etudiant = { mutable nom_e : string; mutable choix : (string * int option) list; mutable liste_favorable : (string * int option) list; mutable liste_attente : (string * int) list }
type formation = { mutable nom_f : string; mutable stack : int; mutable population : (string * int option) list; mutable algo_local : (candidat1:string -> candidat2: string -> bool) option }

type session = { mutable edit_enable : bool; mutable list_etudiant : etudiant list; mutable list_formation : formation list} (* <- modifier *)

(* Mes fonctions *)
let etudiant_cible (session:session) (ind:int) = (List.nth session.list_etudiant ind)
let formation_cible (session:session) (ind:int) = (List.nth session.list_formation ind)

let pos_etudiant (li:etudiant list) (nom:string) =  (* Position d'un nom dans une liste *)
  let rec aux li i = match li with
  | [] -> -1
  | {nom_e = nc ; _}::_ when nc = nom -> i
  | _::tl -> aux tl (i+1)
  in
  aux li 0

let pos_formation (li:formation list) (nom:string) =  (* Position d'un nom dans une liste *)
  let rec aux li i = match li with
  | [] -> -1
  | {nom_f = nf ; _}::_ when nf = nom -> i
  | _::tl -> aux tl (i+1)
  in
  aux li 0

let est_dans_liste_voeux (li:(string * int option) list) (nom:string) = (* Si un etudiant possede deja la formation *)
  let rec aux l i = match l with
  | [] -> -1
  | (nf, _)::_ when nf = nom -> i
  | _::tl -> aux tl (i+1) in
  aux li 0

let modifier_voeux (li:(string * int option) list) (ind:int) (nom:string) (rang:int option) = 
  List.mapi (fun i x -> if i = ind then (nom,rang) else x) li

let modifier_voeux_formation (li:(string * int option) list) (nom:string) (rang:int option) =
  List.mapi (fun i x -> 
    let hd,_ = (List.nth li i) in 
    if hd = nom then (nom,rang) else x) li


let get_pos_voeu (li:(string * int option) list) =
  let rec aux li min = match li with
  | [] -> min
  | (_, valeur)::tl when min = None || min > valeur -> aux tl valeur
  | _::tl -> aux tl min
  in
  aux li None

(*let compter_occurence li pat = 
  let rec aux li acc = match li with
  | [] -> acc
  | (_,hd)::tl when hd = pat -> aux tl (acc+1)
  | _::tl -> aux tl acc
  in
  aux li 0

let roll_choices li form rang =
  let rec aux li acc = match li with
  | [] -> acc
  | ((_,b)as hd)::tl when b = None -> aux tl (acc @ [hd])
  | (a,b)::tl when a <> form && b >= rang -> aux tl (acc @ [(a, Some ((Option.get b) + 1))])
  | hd::tl -> aux tl acc @ [hd]
  in
  aux li []*)

let remove_formation li nom_formation =
  let rec aux l acc = match l with
    | [] -> acc
    | (nom, _)::tl when nom = nom_formation -> acc @ tl 
    | hd::tl -> aux tl (acc @ [hd])
  in
  aux li []

let renonce session ~nom_candidat ~nom_formation = 
  let position_candidat = pos_etudiant session.list_etudiant nom_candidat in
  let candidat_courant = List.nth session.list_etudiant position_candidat in

  let position_formation = pos_formation session.list_formation nom_formation in
  let formation_courant = formation_cible session position_formation in

  candidat_courant.choix <- remove_formation candidat_courant.choix nom_formation;
  candidat_courant.liste_favorable <- remove_formation candidat_courant.liste_favorable nom_formation;
  candidat_courant.liste_attente <- remove_formation candidat_courant.liste_attente nom_formation;
  formation_courant.population <- remove_formation formation_courant.population nom_candidat

let nettoyer_liste_favorable session etu rang_voeu =
  if rang_voeu >= Some 0 && (List.length etu.liste_favorable) > 1
  then
    let rec aux li = match li with
    | [] -> ()
    | (formation, pos)::tl when pos = None -> renonce session ~nom_candidat:etu.nom_e ~nom_formation:formation; aux tl
    | (formation, pos)::tl when pos > rang_voeu -> renonce session ~nom_candidat:etu.nom_e ~nom_formation:formation; aux tl
    | _::tl -> aux tl
    in
    begin
      aux etu.liste_favorable; 
      true
    end
  else false 

let nettoyer_voeux session =
  let flag = ref false in
  for i = 0 to ((List.length session.list_etudiant) - 1) do
    let etudiant_courant = List.nth session.list_etudiant i in
    let pos_voeu = get_pos_voeu etudiant_courant.liste_favorable in (* Cette fonction va chercher le voeux minimal du candidat courant <> de None *)

    let flag_courant = nettoyer_liste_favorable session etudiant_courant pos_voeu in
    flag := !flag || flag_courant 
  done;
  !flag

let formation_pas_dans_liste li nom_f =
  let rec aux l = match l with
  | [] -> true
  | (n, _)::_ when n = nom_f -> false
  | _::tl -> aux tl
  in
  aux li
(* ============ *)

let nouvelle_session () : session = {edit_enable = true; list_etudiant = []; list_formation =[]} (* Liste contenant une liste d'étudiant et une liste de formation *) 

let ajoute_candidat session ~nom_candidat =
  if session.edit_enable
  then
    if pos_etudiant session.list_etudiant nom_candidat = -1 (* Si l'étudiant n'est pas dans la liste *)
    then 
      session.list_etudiant <- session.list_etudiant @ [{nom_e = nom_candidat; choix = []; liste_favorable = []; liste_attente = []}] (* Un candidat a un nom, un nombre 1 (arbitraire), et une liste vide de voeux *)

let ajoute_formation session ~nom_formation ~capacite =
  if session.edit_enable
  then
    if capacite > 0 && (pos_formation session.list_formation nom_formation = -1) (* Si la formation n'est pas dans la liste, et a une capacité > 0 *)
    then  session.list_formation <- session.list_formation @ [{nom_f = nom_formation; stack = capacite; population = []; algo_local = None }] (* Une formation a un nom, un nombre de place définis et une liste de candidats acceptée *)

let ajoute_voeu (session:session) ~rang_repondeur ~nom_candidat ~nom_formation =
  if session.edit_enable
  then
    (* Verifions que le candidat soit dans la liste de voeux avant tout *)
    let position_candidat = pos_etudiant session.list_etudiant nom_candidat in
    let position_formation = pos_formation session.list_formation nom_formation in

    if position_candidat <> -1 && (position_formation <> -1)
    then
      let etudiant_courant = etudiant_cible session position_candidat in
      let formation_courant = formation_cible session position_formation in
      let indice_formation = est_dans_liste_voeux etudiant_courant.choix nom_formation in (* Formation dans la liste de voeu de l'étudiant ? *)
      
      if (indice_formation = -1) (* S'il n'est pas dans sa liste de voeux et qu'on ne cherche pas à modifier son rang ET quen la formation existe *)
      then
        begin
          etudiant_courant.choix <- etudiant_courant.choix @ [(nom_formation, rang_repondeur)]; (* On ajoute le voeux à la fin de la liste du candidat *)
          formation_courant.population <- formation_courant.population @ [(nom_candidat, rang_repondeur)]; (* On ajoute le candidat et la valeur de son souhait dans la liste population de la formation *)
          (*if compter_occurence etudiant_courant.choix rang_repondeur > 1 (* Maintenant tous les voeux superieurs ou egaux au rang de répondeur sont incrémenté de 1 *)
          then
            etudiant_courant.choix <- roll_choices etudiant_courant.choix nom_formation rang_repondeur*)
        end
      else
        begin (* Sinon il modifie simplement le rang de sa formation *)
          etudiant_courant.choix <- modifier_voeux etudiant_courant.choix indice_formation nom_formation rang_repondeur; (* On modifie le rang du voeux dans la liste du candidat courant *)
          formation_courant.population <- modifier_voeux_formation formation_courant.population nom_candidat rang_repondeur
        end


let ajoute_commission session ~nom_formation ~(fonction_comparaison: candidat1:string -> candidat2:string -> bool) = 
  if session.edit_enable
  then
    let position_formation = pos_formation session.list_formation nom_formation in
    let formation_courant = formation_cible session position_formation in
    formation_courant.algo_local <- Some fonction_comparaison

let reunit_commissions session = (* On tri la liste avec l'algo local et on met fin à la phase d'édition *)
  session.edit_enable <- false; (* Fin de la phase d'edition *)

  for i = 0 to List.length session.list_formation - 1 do
    let formation_courante = List.nth session.list_formation i in
    let algo = (Option.get (List.nth session.list_formation i).algo_local) in

    let apply_algo (etu1,_) (etu2,_) = (* L'algo retourne -1 si l'element est deja à sa place sinon 1 pour dire qu'il doit être déplacé *)
      if (algo ~candidat1:etu1 ~candidat2:etu2) then -1 else 1 in

    formation_courante.population <- List.sort apply_algo formation_courante.population
  done

let rec nouveau_jour session =
  if session.edit_enable = false
    then
      let min = Array.make (List.length session.list_etudiant) None in (* On va stocker le voeu le plus important pour chaque candidat *)

      for i = 0 to ((List.length session.list_formation) - 1) do (* Pour chaque formation *)

        let formation_courante = List.nth session.list_formation i in
        let removed = ref 0 in (* On s'en sert pour calibrer en cas de desistement dans un voeu *)
        
        for j = 0 to ((List.length formation_courante.population) - 1) do (* Pour les n étudiants de cette formation *)

          
          let nom_candidat, pos_voeu = List.nth formation_courante.population (j - !removed) in (* Etudiant j de la liste_formation *)
          let position_candidat = pos_etudiant session.list_etudiant nom_candidat in (* Recupere sa position dans la liste des etudiants *)
          let candidat_courant = List.nth session.list_etudiant position_candidat in (* On le recupere pour le traiter *)
          
          if i = 0 (* Initialisation, on rafraichit le statut des voeux de l'étudiant *)
          then begin 
            candidat_courant.liste_favorable <- [];
            candidat_courant.liste_attente <- []
          end;

          if pos_voeu <> None && min.(position_candidat) = None (* On prend le premier voeu different de None *)
          then min.(position_candidat) <- pos_voeu;

          if pos_voeu <> None && min.(position_candidat) > pos_voeu (* S'il existe une valeur inferieur au min, il devient le min *)
          then  min.(position_candidat) <- pos_voeu;

          if (j - !removed) < formation_courante.stack (* Proposition favorable ou en attente ? *)
          then (* envoyer une proposition favorable *)
            if ((*pos_voeu = None ||*) min.(position_candidat) = pos_voeu) (* Ici on fait un tri préalable. Si un voeux avec un rang de répondeur minimal est apparu alors on a pas besoin d'ajouter ceux qui vont suivre *)
            then begin (* Modification du comportement pour les voeux sans rang de repondeur, on les supprimes du moment que le candidat est favorable a une formation de rang repondeur *)
              if formation_pas_dans_liste candidat_courant.liste_favorable formation_courante.nom_f (* On retire les doublons *)
              then candidat_courant.liste_favorable <- candidat_courant.liste_favorable @ [(formation_courante.nom_f, pos_voeu)]
            end
            else begin (* Et donc on renonce à ceux-là *)
              renonce session ~nom_candidat:candidat_courant.nom_e ~nom_formation:formation_courante.nom_f;
              removed := !removed + 1 (* Il faut recalibrer les personnes acceptées *)
            end
          else begin (* Sinon les placer en liste d'attente *)
            let rang_attente = (j - !removed) - formation_courante.stack in
            candidat_courant.liste_attente <- candidat_courant.liste_attente @ [(formation_courante.nom_f, rang_attente)];
            min.(position_candidat) <- None (* Le candidat est en liste d'attente, son voeu n'aboutira pas ce jour-ci il faut donc le rafraichir par son prochain souhait *)
          end
        done;

        let res = nettoyer_voeux session in (* Deuxieme nettoyage *)
        if res then (* S'il y a eu une modification on remet à jour *)
          nouveau_jour session

      done

let consulte_propositions session ~nom_candidat =
  let position_candidat = pos_etudiant session.list_etudiant nom_candidat in (* Recupere sa position dans la liste des etudiants *)
  let candidat_courant = List.nth session.list_etudiant position_candidat in (* On le recupere pour le traiter *)
  let li_string, _ = List.split candidat_courant.liste_favorable in
  li_string

let consulte_voeux_en_attente session ~nom_candidat = 
  let position_candidat = pos_etudiant session.list_etudiant nom_candidat in (* Recupere sa position dans la liste des etudiants *)
  let candidat_courant = List.nth session.list_etudiant position_candidat in (* On le recupere pour le traiter *)
  candidat_courant.liste_attente