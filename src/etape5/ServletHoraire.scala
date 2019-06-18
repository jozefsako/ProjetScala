package etape5

import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import spray.json._
import DefaultJsonProtocol._

class ServletHoraire extends HttpServlet{
  
  	override def doPost(req : HttpServletRequest, resp : HttpServletResponse) = {
	   
	  req.getParameter("action") match{
	    
	    case "getSeries" => {
	      println("\nAction ---> getSeries()")
	      
	      val data = CalculerHoraire5.getSeries
	      val json = data.toJson.prettyPrint
	      
	      resp.setContentType("application/json")
	      resp.setCharacterEncoding("UTF-8")
	      resp.getWriter().write(json)
	    }
	    
	    case "getHoraire"  => {
	      println("\nAction ---> getHoraire()");
	      
	      val id_horaire = req.getParameter("horaire").trim()
	      println(id_horaire)
	      val data = CalculerHoraire5.getResultat
	      
        val liste = for(el <- data) yield el.get(id_horaire)
        val json = liste.toJson.prettyPrint
        
        println(json)
	      
        resp.setStatus(200)
        resp.setContentType("application/json")
        resp.setCharacterEncoding("UTF-8")
        resp.getWriter().write(json)
	    }
	    
	    case _  => { 
	       
	    }
	    
	  }
	}
}
//val data = CalculerHoraire.getResultat
//val js = for(el <- data) yield el.toJson.prettyPrint
//resp.setStatus(200)
//resp.setContentType("application/json")
//resp.setCharacterEncoding("UTF-8")
//resp.getWriter().write(json)

// OR 

//resp.getOutputStream().write(js.toString.getBytes("utf-8"))