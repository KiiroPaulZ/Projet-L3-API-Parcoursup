open Test_parcoursup_utils2

(*  Ce test simule le cas où un candidat qui n'est pas le préféré des formations,
    fait ses voeux avant le candidat prefere des formations.
    C'est ici que l'algo_local entre en jeu, pour trier la liste.
*)
let session = nouvelle_session ()

let () =  
  ajoute_candidat session ~nom_candidat:"Paul";
  ajoute_candidat session ~nom_candidat:"Margot";
  ajoute_formation session ~nom_formation:"Université Côte d'Azur" ~capacite:1;
  ajoute_formation session ~nom_formation:"Université de Toulon" ~capacite:1;
  ajoute_voeu session 
    ~rang_repondeur:(None) 
    ~nom_candidat:"Paul" 
    ~nom_formation:"Université Côte d'Azur";
    ajoute_voeu session 
    ~rang_repondeur:(None)
    ~nom_candidat:"Paul" 
    ~nom_formation:"Université de Toulon";
    ajoute_voeu session 
    ~rang_repondeur:None
    ~nom_candidat:"Margot" 
    ~nom_formation:"Université Côte d'Azur";
    ajoute_voeu session 
    ~rang_repondeur:(None) 
    ~nom_candidat:"Margot" 
    ~nom_formation:"Université de Toulon";
  ajoute_commission session 
    ~nom_formation:"Université Côte d'Azur" 
    ["Margot";"Paul"]; 
    ajoute_commission session 
    ~nom_formation:"Université de Toulon" 
    ["Margot";"Paul"]; 
  reunit_commissions session;
  nouveau_jour session;
  affiche_voeux_en_attente session "Paul";
  affiche_propositions_en_attente session "Paul";
  affiche_voeux_en_attente session "Margot";
  affiche_propositions_en_attente session "Margot";
  renonce session  ~nom_candidat:"Margot"  ~nom_formation:"Université Côte d'Azur";
  nouveau_jour session;
  affiche_voeux_en_attente session "Paul";
  affiche_propositions_en_attente session "Paul";
  affiche_voeux_en_attente session "Margot";
  affiche_propositions_en_attente session "Margot";