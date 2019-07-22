<%@ page import = "se.sics.prologbeans.*" %>
<html>
<head><title>Summa Calculator</title></head>
<body bgcolor="white">
<font size=4>Prolog Sum Calculator, enter expression to evaluate and sum-up!
<form><input type=text name=query></form>

<%
   PrologSession pSession = PrologSession.getPrologSession("prolog/PrologSession", session);
   pSession.connect(); // [PD] 4.0.4+ there is no autoconnect in PB 4.

   String evQuery = request.getParameter("query");
   String output = "";
   if (evQuery != null) {
     Bindings bindings = new Bindings().bind("E",evQuery + '.');
     QueryAnswer answer = pSession.executeQuery("sum(E,Sum,Average,Count)",
			                        bindings);

     PBTerm average = answer.getValue("Average");
     if (average != null) {
        PBTerm sum = answer.getValue("Sum");
        PBTerm count = answer.getValue("Count");

	output = "<h4>Average =" + average + ", Sum = "
	+ sum + " Count = " + count + "</h4>";
     } else {
        output = "<h4>Error: " + answer.getError() + "</h4>";
     }
  }
%>
<%= output  %><br>
</font>
<p><hr>Powered by SICStus Prolog
</body>
</html>
