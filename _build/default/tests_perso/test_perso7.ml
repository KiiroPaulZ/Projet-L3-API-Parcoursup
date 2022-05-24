open Test_parcoursup_utils2

(* 
  Les candidat n'ont jamais réalisé de voeu et donc n'en reçoivent aucun.
*)
let session = nouvelle_session ()

let () =
  ajoute_candidat session ~nom_candidat:"Paul";
  ajoute_candidat session ~nom_candidat:"Margot";
  ajoute_formation session ~nom_formation:"Université Côte d'Azur" ~capacite:1;
  ajoute_formation session ~nom_formation:"Université de Toulon" ~capacite:1;
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