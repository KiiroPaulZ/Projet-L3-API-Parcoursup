open Test_parcoursup_utils2
(*
  Ce test permet de voir si le programme a le bon comportement en cas de renoncement pour un voeu 
  et qu'on le rajoute de nouveau plus tard.
  Ce test m'a permi de detecter un bug dans ma fonction renonce.
*)
let session = nouvelle_session ()

let () =  
  ajoute_candidat session ~nom_candidat:"Paul";
  ajoute_candidat session ~nom_candidat:"Margot";
  ajoute_formation session ~nom_formation:"Université Côte d'Azur" ~capacite:1;
  ajoute_formation session ~nom_formation:"Université de Toulon" ~capacite:1;
  ajoute_voeu session
    ~rang_repondeur:(Some 1) 
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
    renonce session  ~nom_candidat:"Paul"  ~nom_formation:"Université Côte d'Azur";
    ajoute_voeu session 
    ~rang_repondeur:(Some 1) 
    ~nom_candidat:"Margot"
    ~nom_formation:"Université de Toulon";
    ajoute_voeu session 
    ~rang_repondeur:(Some 1) 
    ~nom_candidat:"Paul" 
    ~nom_formation:"Université Côte d'Azur";
  ajoute_commission session 
    ~nom_formation:"Université Côte d'Azur" 
    ["Margot";"Paul"];
    ajoute_commission session 
    ~nom_formation:"Université de Toulon" 
    ["Paul"; "Margot"]; 
  reunit_commissions session;
  nouveau_jour session;
  affiche_voeux_en_attente session "Paul";
  affiche_propositions_en_attente session "Paul";
  affiche_voeux_en_attente session "Margot";
  affiche_propositions_en_attente session "Margot";
  renonce session  ~nom_candidat:"Paul"  ~nom_formation:"Université de Toulon";
  nouveau_jour session;
  affiche_voeux_en_attente session "Paul";
  affiche_propositions_en_attente session "Paul";
  affiche_voeux_en_attente session "Margot";
  affiche_propositions_en_attente session "Margot";