package etape3

import org.eclipse.jetty.server.{Server}
import org.eclipse.jetty.webapp.WebAppContext
import org.eclipse.jetty.servlet.DefaultServlet;
import javax.servlet.http.HttpServlet
import org.eclipse.jetty.server.handler.ContextHandler
import org.eclipse.jetty.servlet.ServletHolder;

object ServeurHoraire extends App {
  val server = new Server(8080)

  val context = new WebAppContext()
  context.setServer(server)
  context.setWar("www")
  context.addServlet(new ServletHolder(new DefaultServlet()), "/")
  context.addServlet(new ServletHolder(new ServletHoraire()), "/Horaire")

  val context0: ContextHandler = new ContextHandler();
  context0.setHandler(context)
  server.setHandler(context0)

  try {
    server.start()
  } catch {
    case exc: Exception => {
      exc.printStackTrace()
      System.exit(100)
    }
  }
}