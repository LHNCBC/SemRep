<%@ page import = "se.sics.prologbeans.*" %>
<html>
<head><title>Evaluator</title></head>
<body bgcolor="white">
<h1>Prolog Evaluator, enter expression to evaluate!</h1>
<form><input type=text name=query></form>
<%
   // Find the PrologSession!
   PrologSession pSession = PrologSession.getPrologSession("prolog/PrologSession");
   String evQuery = request.getParameter("query");
   String output = "";
   if (evQuery != null) {
     QueryAnswer answer = pSession.executeQuery("evaluate(E,R)",
                          new Bindings().bind("E",evQuery + '.'));
     Term result = answer.getValue("R");
     if (result != null) {
	output = "<h4>Result =" + result + " </h4>";
     } else {
        output = "<h4>Error: " + answer.getError() + "</h4>";
     }
  }
%>
<%= output %>
<p><hr>Powered by SICStus Prolog
</body>
</html>
