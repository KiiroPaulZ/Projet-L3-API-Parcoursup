open Test_parcoursup_utils2
let session = nouvelle_session ()

let () =  (* Ce test simule la répartition de deux candidats qui doivent répondre à leurs voeux avec Paul qui a une priorité sur les deux formations. *)
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
    ["Paul";"Margot"]; 
    ajoute_commission session 
    ~nom_formation:"Université de Toulon" 
    ["Paul";"Margot"]; 
  reunit_commissions session;
  nouveau_jour session;
  affiche_voeux_en_attente session "Paul";
  affiche_propositions_en_attente session "Paul";
  affiche_voeux_en_attente session "Margot";
  affiche_propositions_en_attente session "Margot";
  renonce session  ~nom_candidat:"Paul"  ~nom_formation:"Université Côte d'Azur";
  nouveau_jour session;
  affiche_voeux_en_attente session "Paul";
  affiche_propositions_en_attente session "Paul";
  affiche_voeux_en_attente session "Margot";
  affiche_propositions_en_attente session "Margot";