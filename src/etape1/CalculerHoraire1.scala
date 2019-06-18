package etape1
import JaCoP.scala._

object CalculerHoraire1 extends App with jacop {
  
    /* 
     * ++++++++++++++++++++++++++
     * initialisation des intvars
     * ++++++++++++++++++++++++++    
     */    
    val x = 20
    val y = 3
    val VAL_MAX = 20

    val series = List("Serie 01", "Serie 02")
    val map_series = Map(1 -> "Serie 01" , 2 -> "Serie 02")
    val s01 = 1
    val s02 = 2

    val iLocaux = for (i <- List.range(0, VAL_MAX * series.length)) yield new IntVar("locaux", 0, 2)
    val locaux = List("Fourche", "017", "019")
    val lFourche = 0
    val l017 = 1
    val l019 = 2

    val iProfesseurs = for (i <- List.range(0, VAL_MAX * series.length)) yield new IntVar("prof", 0, 4)
    val professeurs = List("Fourche", "Grolaux", "Damas", "Vander Meulen", "Ninane")
    val pFourche = 0
    val pGrolaux = 1
    val pDamas = 2
    val pVanderMeulen = 3
    val pNinane = 4
    val map_profs = Map(pGrolaux ->  4, pDamas -> 4, pVanderMeulen-> 4, pNinane -> 2)

    val iCours = for (i <- List.range(0, VAL_MAX * series.length)) yield new IntVar("cours", 0, 4)
    val cours = List("Fourche", "Algo 1", "Algo 2", "Js", "Securite")
    val cFouche = 0
    val cAlgo1 = 1
    val cAlgo2 = 2
    val cJs = 3
    val cSecurite = 4
    val indicesCours = List(cAlgo1, cAlgo2, cJs, cSecurite)
    val map_cours = Map(cAlgo1 -> 4, cAlgo2-> 4, cJs-> 4, cSecurite -> 2) //contient l'indice du prof et le nombre de cours qu'il donne par serie

    val jours = Array("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi")
    val heures = List("08h30 - 10h30", "10h45 - 12h45", "13h45 - 15h45", "16h00 - 18h00");

    val vars = iLocaux ::: iProfesseurs ::: iCours
    
    
    /* 
     * ++++++++++++++++++++++++++++++
     * initialisation des contraintes   
     * ++++++++++++++++++++++++++++++  
     */
     
    /*
     * contrainte: M.Grolaux ne commence jamais à 8h30, 
     * 	le prof ne peut pas etre à 2 endroits à la fois,
     * 	pas 2 cours dans le meme local.
     */
    for (i <- List.range(0, iProfesseurs.length / series.length)) {

      // M.Grolaux ne commence jamais à 8h30
      	  if( i==0 || i%(5-1)==0){
      		  AND (iProfesseurs(i) #\= pGrolaux, iProfesseurs(i+20) #\=pGrolaux)
      	  }

      // Le prof ne peut pas etre à 2 endroits à la fois
      OR(iProfesseurs(i) #\= iProfesseurs(i + 20), iProfesseurs(i) #= pFourche, iProfesseurs(i + 20) #= pFourche)

      // Pas 2 cours dans le meme local
      OR(iLocaux(i) #\= iLocaux(i + 20), iLocaux(i) #= lFourche, iLocaux(i + 20) #= lFourche)
    }
    

    /*
     * contrainte : meme nombre de cours dans les différentes series
     */
    for (s <- List.range(0, iCours.length, iCours.length / series.length)) {
      for (item <- map_cours) {
        item._2 #= count(iCours.slice(s, s + iCours.length / series.length), item._1)
        //item._2 #= count(iCours.slice(iCours.length/series.length, iCours.length), item._1)
      }
    }

    /*
     * contrainte : chaque prof donne le meme nombre de cours dans chaque serie
     */
    for (item <- map_profs) {
      item._2 #= count(iProfesseurs.slice(0, iProfesseurs.length / 2), item._1)
      item._2 #= count(iProfesseurs.slice(iProfesseurs.length / 2, iProfesseurs.length), item._1)
    }
    
    /* 
     * +++++++++++++++++++++
     * recherche de solution
     * +++++++++++++++++++++
     */
    
    
    //Recherche des solutions qui satisfait nos contraintes
    //indomain_middle = choose the intvar with middle domain value
    //first_fail = choose the variable with the smallest domain
    val result = satisfy(search(vars, first_fail, indomain_middle))
 
  
}