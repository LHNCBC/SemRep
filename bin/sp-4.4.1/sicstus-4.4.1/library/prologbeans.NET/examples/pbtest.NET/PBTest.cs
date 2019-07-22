// Copyright (c) 2004 SICS AB. All rights reserved.
/// <summary> pbtest.cs
/// <version>  1.0
/// 
/// </version>
using System;
using System.IO;
using System.Threading;
//using System.Windows.Forms;
using se.sics.prologbeans;

public class PBTest
{
  virtual public int Port
    {
      get
	{
	  lock (this)
	    {
	      // Get the port from the running PBtest server (if not received within
	      // 60 seconds, -1 will be returned.
	      if (port == - 1)
		{
		  Monitor.Wait(this, TimeSpan.FromMilliseconds(60000));
		}
	      return port;
	    }
	}
		
    }
  private static int error = 0;
  private int port = - 1;
  private System.Diagnostics.Process process;
  private bool isShutdown = false;
  private System.String prolog;
	
  public void  Run()
    {
      try
	{
	  // [PD] This is portable (i.e. works both for SICStus and Quintus)
	  // [PD] Correction: not quite portable. SICStus does not have +l.
	  //      SICStus has -l instead. Sigh.
	  /* if QUINTUS
	  String commandArgs = " +l pbtest_run";
	  /* endif QUINTUS */
	  /* if SICSTUS */
	  System.String commandArgs = " -l pbtest_run";
	  /* endif SICSTUS */
	  System.Console.Error.WriteLine("DBG: Launching SICStus with \"" + prolog + commandArgs + "\"");
	  // Load and start the Prolog example
//	  System.Diagnostics.Process.GetCurrentProcess(); /* [PD] FIXME:Why? */
	  process = new System.Diagnostics.Process();
	  if (prolog.Equals("")) { prolog = "sicstus"; }
	  process.StartInfo.FileName = prolog;
	  process.StartInfo.Arguments = commandArgs;
	  process.StartInfo.UseShellExecute = false;
	  process.StartInfo.RedirectStandardError = true;
	  process.Start();
          
	  // Write all the error output that has no % in the start of the line
			
	  StreamReader err = new StreamReader(new StreamReader(process.StandardError.BaseStream).BaseStream, System.Text.Encoding.UTF7);
	  System.String line;
	  while ((line = err.ReadLine()) != null)
	    {
	      if (line.Length > 0 && line[0] != '%')
		{
		  System.Console.Error.WriteLine(line);
					
		  // When port is found, set it and notify that SICStus is running!
		  if (line.StartsWith("port:"))
		    {
		      port = System.Int32.Parse(line.Substring(5)); // e.g, port:4711
System.Console.Error.WriteLine("DBG: port==" + port);
		      lock (this)
			{
			  Monitor.Pulse(this);
			}
		    }
		}
	    }
	}
      catch (System.Exception e)
	{
	  Console.Error.WriteLine("PBTest.Run caught an exception.");
	  System.Console.Error.WriteLine(e);
//	  SupportClass.WriteStackTrace(e, Console.Error);
	  Console.Error.WriteLine(e.ToString());
	  Console.Error.Flush();
	  port = - 2;
	}
      finally
	{
	  lock (this)
	    {
	      isShutdown = true;
	      Monitor.Pulse(this);
	    }
	}
    }
	
	
  public virtual bool waitForShutdown()
    {
      lock (this)
	{
	  // Wait at most 10 seconds for the Prolog Server to shutdown
	  if (!isShutdown)
	    {
	      Monitor.Wait(this, TimeSpan.FromMilliseconds(10000));
	    }
	  return isShutdown;
	}
    }
	
  public virtual void  shutdown()
    {
      try {
        // [PM] 3.12.0 this can barf (as can process.HasExited) if
        // the process did not start e.g., the path was not found.
	process.Kill();
      }
      catch (System.Exception) {
        // ignore all errors
      }
    }
	
  [STAThread]
    public static void  Main(System.String[] args)
    {
      // Startup the prolog and show its err output!
      int test = 1;
      PrologSession session = null;
      PBTest evalTest = new PBTest();
      evalTest.prolog = args[0];

      try
	{
//	  SupportClass.ThreadClass t = new SupportClass.ThreadClass(new ThreadStart(evalTest.Run));
	  Thread t = new Thread(new ThreadStart(evalTest.Run));
	  t.IsBackground = true;
	  t.Start();
			
	  // Get the port from the SICStus process (and fail if port is an error value)
	  int port = evalTest.Port;
	  if (port <= 0)
	    {
	      evalTest.fail("could not start sicstus", test);
	    }
			
	  session = new PrologSession();
System.Console.Error.WriteLine("DBG: setting port; port==" + port);
//	  session.Port = port;		// PrologBeans.NET.c# -version
	  session.setPort(port);	// PrologBeans.NET.j# -version

	  {
	    System.Console.Error.WriteLine("DBG: setting timeout=0");
//	    session.Timeout = 0; 	// PrologBeans.NET.c# -version
	    session.setTimeout(0); 	// PrologBeans.NET.j# -version
	  }
			
	  // Test 1. - evaluation!
	  Bindings bindings = new Bindings().bind("E", "10+20.");
	  session.connect();	/* This will connect if neccessary. */
	  QueryAnswer answer = session.executeQuery("evaluate(E,R)", bindings);
	  PBTerm result = answer.getValue("R");
	  if (result != null)
	    {
	      if (result.intValue() == 30)
		{
		  success("10+20=" + result, test++);
		}
	      else
		{
		  evalTest.fail("Execution failed: " + result, test);
		}
	    }
	  else
	    {
	      evalTest.fail("Error " + answer.getError(), test);
	    }
			
	  // Test 2 - list reverse!
	  bindings = new Bindings().bind("E", "reverse");
	  answer = session.executeQuery("reverse(E,R)", bindings);
	  result = answer.getValue("R");
	  if (result != null)
	    {
//	      if ("esrever".Equals(result.ToString()))
		if (listcompare(result,"esrever"))
		{
		  success("rev(reverse) -> " + result, test++);
		}
	      else
		{
		  evalTest.fail("Execution failed: " + result, test);
		}
	    }
	  else
	    {
	      evalTest.fail("Error " + answer.getError(), test);
	    }

	  // Test 2b - SPRM 13863 transfer lists of small integers
	  PBTerm NIL = PBTerm.makeAtom("[]");
	  PBTerm eTerm = PBTerm.makeTerm(PBTerm.makeTerm((int)127), PBTerm.makeTerm(PBTerm.makeTerm((int)128), PBTerm.makeTerm(PBTerm.makeTerm((int)129), NIL)));
	  PBTerm rTerm = PBTerm.makeTerm(PBTerm.makeTerm((int)129), PBTerm.makeTerm(PBTerm.makeTerm((int)128), PBTerm.makeTerm(PBTerm.makeTerm((int)127), NIL)));
	  bindings = new Bindings().bind("E", eTerm);
	  answer = session.executeQuery("reverse(E,R)", bindings);
	  result = answer.getValue("R");
	  if (result != null) {
	    if (listcompare(result,rTerm)) {
	      success("rev(" + eTerm + ") -> " + result, test++);
	    } else {
	      evalTest.fail("Execution failed: " + "rev(" + eTerm + ") -> " + result, test);
	    }
	  } else {
	    evalTest.fail("Error " + answer.getError(), test);
	  }


			
	  // Test 3 - show developers
	  // [PD] 3.12.3 Test non-ascii character in query name
	  answer = session.executeQuery("devel\x00f6pers(Dev)");
	  result = answer.getValue("Dev");
	  if (result != null)
	    {
//		if (result.ProperList)     // PrologBeans.NET.c# -version
		if (result.isProperList()) // PrologBeans.NET.j# -version
		{
		  PBTerm list = result;
		  if (list.length() == 4 &&
		      "Joakim".Equals(list.head().ToString()) &&
		      "Niclas".Equals(list.tail().head().ToString()) &&
		      "Per".Equals(list.tail().tail().head().ToString()) &&
// [PD] 3.12.2 Do not use non-ASCII literals
//		      "åäöÅÄÖ".Equals(list.getTermAt(4).ToString()))
	      "\u00e5\u00e4\u00f6\u00c5\u00c4\u00d6".Equals(list.tail().tail().tail().head().ToString())) {
	  // [PD] 3.12.3 Test non-ascii character in query name
		      success("devel\x00f6pers -> " + result, test++);
		    }
		  else
		    {
		      evalTest.fail("Execution failed: " + result, test);
		    }
		}
	      else
		{
		  evalTest.fail("Execution failed: " + result, test);
		}
	    }
	  else
	    {
	      evalTest.fail("Error " + answer.getError(), test);
	    }
			
	  // Test 4 - send and receive a complex string-list
	  String str = "foo\u1267bar";
	  bindings = new Bindings().bind("L1", str);
	  answer = session.executeQuery("send_receive(L1,L2)", bindings);
	  result = answer.getValue("L2");
	  if( result != null) {
//	    if (((PBString)result).equals(str)) {
	    if (listcompare(result, str)) {
	      success("send_receive(" + str + ") -> " + result, test++);
	    } else {
	      evalTest.fail("Execution failed: " + result, test);
	    }
	  } else {
	    evalTest.fail("Error " + answer.getError(), test);
	  }

	  // Test 5 - send and receive a very large atom
	  int stringLength = 100000;
	  String longStr = new String('x', stringLength);
	  bindings = new Bindings().bind("L1", longStr);
	  answer = session.executeQuery("send_receive(L1,L2)", bindings);
	  result = answer.getValue("L2");
	  if( result != null) {
	    if (result.getString().Equals(longStr)) {
	      success("OK (" + stringLength + " characters)", test++);
	    } else {
	      evalTest.fail("Execution failed: " + stringLength + " characters",
			    test);
	    }
	  } else {
	    evalTest.fail("Error " + answer.getError() + ", " + stringLength
			  + " characters", test);
	  }

          // Test 6. Attributed variables
          bindings = new Bindings();
          bindings.bind("N",1);
          bindings.bind("M",5);
          answer = session.executeQuery("newVar(X,N,M)",bindings);
          result = answer.getValue("X");
          if( result != null) {
	      if (result.isVariable()) {
	          success("OK", test++);
	      } else {
	          evalTest.fail("Execution failed: " + stringLength + " characters", test);
	      }
          } else {
	      evalTest.fail("Error " + answer.getError() + ", " + stringLength
	           + " characters", test);
          }

	  // Test 7. shutdown server...
	  session.executeQuery("shutdown");
	  if (!evalTest.waitForShutdown())
	    {
	      evalTest.fail("shutdown", test++);
	    }
	  else
	    {
	      success("shutdown", test++);
	    }
	}
      catch (System.Exception e)
	{
	  if (error == 0)
	    {
	      Console.Error.WriteLine("PBTest.Main caught an exception.");
	      System.Console.Error.WriteLine(e);
//	      SupportClass.WriteStackTrace(e, Console.Error);
              Console.Error.WriteLine(e.ToString());
	      Console.Error.Flush();
	      evalTest.fail("Exception " + e.Message, test);
	    }
	}
      finally
	{
	  if (session != null)
	    {
	      session.disconnect();
	    }
	  evalTest.shutdown();
	  System.Environment.Exit(error);
	}
    }
	
/*
  private static bool compare(PBString pbstr, String str)
    {
      PBList pbl = pbstr;
      int i;
      for (i = 0; i < str.Length && pbl.Length != 0; i++) {
	int c = pbl.getArgument(1).intValue();
	if (c != str[i]) {
	  return false;
	} else {
	  pbl = (PBList)(pbl.getArgument(2));
	}
      }
      if (i < str.Length || pbl.Length != 0) {
	return false;
      }
      return true;
    }
*/
    
  private static bool listcompare(PBTerm pbl, String str) {
      int i;
      for (i = 0; i < str.Length && pbl.length() != 0; i++) {
	  long c = pbl.getArgument(1).intValue();
	  if (c != str[i]) {
	      return false;
	  } else {
	      pbl = pbl.getArgument(2);
	  }
      }
      if (i < str.Length || pbl.length() != 0) {
	  return false;
      }
      return true;
  }


  private static bool listcompare(PBTerm t1, PBTerm t2) {
      int i;
      if (t1.length() != t2.length()) {
    	  return false;
      }
      for (i = 0; t1.length() != 0; i++) {
    	  long c1 = t1.getArgument(1).intValue();
    	  long c2 = t2.getArgument(1).intValue();
	  if (c1 != c2) {
	      return false;
	  } else {
	      t1 = t1.getArgument(2);
	      t2 = t2.getArgument(2);
	  }
      }
      return true;
  }

//  private static void  fail(System.String msg, int test)
  private void  fail(System.String msg, int test)
    {
      System.Console.Error.WriteLine("Execution failed: " + msg + " for test " + test);
      error = 1;
// [PD] 4.0.0beta2 Throwing an exception here will cause .NET to want to enter
//      the debugger. This is not very practical when running the testsuite via
//      SSH.
//      throw new System.SystemException("");
      process.Kill();		// don't leave the sicstus process around.
      System.Environment.Exit(1);
    }
	
  private static void  success(System.String msg, int test)
    {
      System.Console.Out.WriteLine("Test " + test + " succeeded: " + msg);
    }
}
// PBTest
