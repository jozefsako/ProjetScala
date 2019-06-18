package etape4


import JaCoP.scala._

object CalculerHoraire4 extends App with jacop {

  def getSeries: List[String] = {
    List("Serie 01", "Serie 02")
  }

  def getResultat: List[Map[String, List[List[String]]]] = {

    val x = 20
    val y = 3
    val VAL_MAX = 20

    val series = List("Serie 01", "Serie 02")
    val map_series = Map(1 -> "Serie 01", 2 -> "Serie 02")
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
    
    /*
     * Contraintes: Hard
     * Le nombre d'heures qu'un prof doit prester / Serie
     * Ex : Mr D. Grolaux doit prester 5 x 2 = 10 Cours <=> 20h en total 
     */
    val map_profs = Map(pGrolaux -> 5, pDamas -> 4, pVanderMeulen -> 4, pNinane -> 2)

    val iCours = for (i <- List.range(0, VAL_MAX * series.length)) yield new IntVar("cours", 0, 4)
    val cours = List("Fourche", "Algo 1", "Algo 2", "Js", "Securite")
    val cFouche = 0
    val cAlgo1 = 1
    val cAlgo2 = 2
    val cJs = 3
    val cSecurite = 4
    val indicesCours = List(cAlgo1, cAlgo2, cJs, cSecurite)
    val map_cours = Map(cAlgo1 -> 4, cAlgo2 -> 4, cJs -> 4, cSecurite -> 2)

    val jours = Array("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi")
    val heures = List("08h30 - 10h30", "10h45 - 12h45", "13h45 - 15h45", "16h00 - 18h00");

    val vars = iLocaux ::: iProfesseurs ::: iCours
    
    for (i <- List.range(0, VAL_MAX)) {

      /*
       * Contrainte: Hard
       * Le prof ne peut pas etre à 2 endroits à la fois
       */
      OR(iProfesseurs(i) #\= iProfesseurs(i + 20), iProfesseurs(i) #= pFourche)
      
      /*
       * Contrainte: Hard
       * Si un prof donne cours => cours != Fourche
       * Sinon cours = Fourche && Prof = Fouche 						
       */          
      OR( AND(iProfesseurs(i) #= pFourche, iCours(i) #= cFouche),
          AND(iProfesseurs(i) #\= pFourche, iProfesseurs(i) #\= pFourche))

      OR( AND(iProfesseurs(i+20) #= pFourche, iCours(i+20) #= cFouche),
          AND(iProfesseurs(i+20) #\= pFourche, iProfesseurs(i+20) #\= pFourche))

      /*
       * Contrainte: Hard
       * Pas 2 cours dans le meme local
       */
      OR(iLocaux(i) #\= iLocaux(i + 20), iLocaux(i) #= lFourche, iLocaux(i + 20) #= lFourche)
    }

    /*
     * Contrainte: Hard 
     * Meme nombre de cours dans les différentes series
     */
    for (s <- List.range(0, iCours.length, VAL_MAX)) {
      for (item <- map_cours) {
        item._2 #= count(iCours.slice(s, s + VAL_MAX), item._1)
      }
    }

    /*
     * Contrainte: Hard
     * Chaque prof donne le meme nombre de cours dans chaque serie
     */
    for (s <- List.range(0, iProfesseurs.length, VAL_MAX)) {
      for (professeur <- map_profs) {
        professeur._2 #= count(iProfesseurs.slice(s, s + VAL_MAX), professeur._1)
      }
    }
    
    /*
     * Contrainte
     * Interdire la présence du prof :p avant l'heure :h
     */
    def coursAvantH(p: Int, h: Int): List[BoolVar] = {    		

    		val serie1 = for(i <- List.range(0, h * jours.length + heures.length + 1)) yield {
    			val statement = new BoolVar("")
    					statement <=> (iProfesseurs(i) #\= p)
    					statement
    		}

    		val serie2 = for(i <- List.range(0, h * jours.length + heures.length + 1)) yield {
    			val statement = new BoolVar("")
    					statement <=> (iProfesseurs(i+ VAL_MAX) #\= p)
    					statement
    		}

    		serie1 ::: serie2
    }
    
    /*
     * Contrainte
     * Interdire la présence du prof :p apres l'heure :h
     */
    def coursApresH(p: Int, h: Int): List[BoolVar] = {    		

    		val serie1 = for(i <- List.range(h * jours.length, VAL_MAX)) yield {
    			val statement = new BoolVar("")
    					statement <=> (iProfesseurs(i) #\= p)
    					statement
    		}

    		val serie2 = for(i <- List.range(h * jours.length, VAL_MAX)) yield {
    			val statement = new BoolVar("")
    					statement <=> (iProfesseurs(i+ VAL_MAX) #\= p)
    					statement
    		}

    		serie1 ::: serie2
    }
    
    def afficherSolution(): Unit = {
    		for (s <- List.range(0, series.length)) {
    			for (h <- List.range(0, heures.length)) {
    				for (j <- List.range(0, jours.length)) {
    					print("[ ")
    					print(locaux(iLocaux(s * 20 + j + h * 5).value) + " - ")
    					print(professeurs(iProfesseurs(s * 20 + j + h * 5).value) + " - ")
    					print(cours(iCours(s * 20 + j + h * 5).value) + " ")
    					print(" ] ")
    				}
    				println()
    			}
    			println()
    		}
    }
    
    /*
     * Contrainte: Soft
     * Mr D. Grolaux ne souhaite pas donner cours ( < 10h30) && ( > 13h00)
     */
    val contrainte_grolaux_1 = coursAvantH(pGrolaux, 0)
    val contrainte_grolaux_2 = coursAvantH(pGrolaux, 2)
    val contraintes_grolaux = contrainte_grolaux_1 ::: contrainte_grolaux_2
    val contraintes_soft = count(contraintes_grolaux, 0)

    //indomain_middle = choose the intvar with middle domain value
    //first_fail = choose the variable with the smallest domain
    //val result = satisfy(search(vars, first_fail, indomain_middle))

    val result = minimize(search(vars, most_constrained, indomain_middle), contraintes_soft, afficherSolution)

    if(!result){
      println("Pas de solution")
    }

    val mapResult = for (s <- List.range(0, series.length)) yield {
      val liste = for (i <- List.range(0, VAL_MAX)) yield {
        List(locaux(iLocaux((s * 20) + i).value), professeurs(iProfesseurs((s * 20) + i).value), cours(iCours((s * 20) + i).value))
      }
      Map(series(s) -> liste)
    }

    return mapResult
  }

}