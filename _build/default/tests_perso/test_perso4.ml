open Test_parcoursup_utils2
(* 
  Ce test gère le cas où on aurait besoin de parfois rafraichir plusieurs fois
  un même jour en cas de changement significatif :
  Exemple:
   Paul souhaite Aller en UCA  mais UCA prefere Margot.
   Margot souhaite aller en UT mais UT Prefere Paul.
   Ils ont respectivement souhaité repondre manuellement pour l'université qu'ils ne visaient pas de base.
   Un jour passe et constatent tous les deux qu'ils ont chacun était accepter à leur voeu en réponse manuel.
   Mais Paul est convaincu de passer a UCA et prends le risque de renoncer à UT.
   Le jour d'après, ils sont tous les deux acceptés à leurs formations préférées.

  Si ce test ne passe pas, c'est qu'il vous faut surement iterer nouveau encore une fois ...
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
    ajoute_voeu session 
    ~rang_repondeur:(Some 1) 
    ~nom_candidat:"Margot" 
    ~nom_formation:"Université de Toulon";
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