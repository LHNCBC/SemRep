/**
 * EvaluateGUI.java
 * A simple example showing how to connect a Java GUI to a running
 * prolog server.
 *
 * Created: Tue Aug 19 13:59:06 2003
 *
 * @Author  : Joakim Eriksson
 */
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import se.sics.prologbeans.*;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.io.BufferedReader;
import java.io.InputStreamReader;

class SyncObject {
    boolean sicstus_ready = false;
}

public class EvaluateGUI implements ActionListener {

  private JTextArea text = new JTextArea(20, 40);
  private JTextField input = new JTextField(36);
  private JButton evaluate = new JButton("Evaluate");
  private PrologSession session = new PrologSession();
  private int timeout = Integer.getInteger("se.sics.prologbeans.timeout", 0).intValue();

  private SyncObject syncObj;

  public EvaluateGUI(String[] args) throws java.io.IOException
  {
      this(null, args);
  }

  public EvaluateGUI(SyncObject so, String[] args) throws java.io.IOException
  {
    syncObj = so;
    if ((Integer.getInteger("se.sics.prologbeans.debug", 0)).intValue() != 0) {
      session.setTimeout(0);
    }
    JFrame frame = new JFrame("Prolog Evaluator");
    Container panel = frame.getContentPane();
    panel.add(new JScrollPane(text), BorderLayout.CENTER);
    JPanel inputPanel = new JPanel(new BorderLayout());
    inputPanel.add(input, BorderLayout.CENTER);
    inputPanel.add(evaluate, BorderLayout.EAST);
    panel.add(inputPanel, BorderLayout. SOUTH);
    text.setEditable(false);
    evaluate.addActionListener(this);
    input.addActionListener(this);

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.pack();

    if (args.length > 0 && args[0].equalsIgnoreCase("batch")) {
      try {
	synchronized(syncObj) {
	  long limit = System.currentTimeMillis() + timeout;
	  long timeToGo = timeout;
	  // timeToGo guards against spurious wakeup.
	  // See API doc for Object.wait(long timeout)
	  while (! syncObj.sicstus_ready && timeToGo > 0) {
	      syncObj.wait(timeToGo); // Wait for sicstus to listen to the port.
	      timeToGo = limit - System.currentTimeMillis();
	  }
	}
	session.connect();
	new Thread() {
	  public void run() {
	    batch_queries();
	  }
	}.run();
      } catch (Exception e) {
	System.err.println("Error: " + e.getMessage());
	e.printStackTrace();
	System.exit(1);
      }
      
    } else {
	session.connect();
    }

    frame.setVisible(true);
  }

  public void actionPerformed(ActionEvent event) {
    String message;
    try {
      Bindings bindings = new Bindings().bind("E", input.getText() + '.');
      QueryAnswer answer =
	session.executeQuery("evaluate(E,R)", bindings);
      PBTerm result = answer.getValue("R");
      if (result != null) {
        message = input.getText() + " = " + result + '\n';
	text.append(message);
	input.setText("");
      } else {
        message = "Error: " + answer.getError() + '\n';
	text.append(message);
      }
    } catch (Exception e) {
      message = "Error when querying Prolog Server: " + e.getMessage() + '\n';
      text.append(message);
      e.printStackTrace();
    }
    // [PM] 4.2.1 debug    
    System.err.println("Log: " + message);
  }

  void batch_queries() {
      final Runnable calculate = new Runnable() {
	      public void run() {
		  evaluate.doClick();
	      }		    
	  };

      new Thread() {
	  public void run() {
	      try {
                  // [PM] 4.0.4 The Java on Louis does not allow passing the strings as varargs
		  List ql = Arrays.asList(new String[] {"4+6", "11-5", "3*7", "9/3"});
		  for (Iterator i = ql.iterator(); i.hasNext(); ) {
		      String q = (String)i.next();
		      input.setText(q);
		      SwingUtilities.invokeAndWait(calculate);
		  }
		  session.executeQuery("shutdown");
		  session.disconnect();
		  System.exit(0);
	      } catch (Exception ex) {
		  text.append("Error when clicking Evaluate button: " +
			      ex.getMessage() + '\n');
		  ex.printStackTrace();
	      }
	  }
      }.start();
  } 


  public static void main(String[] args) throws java.io.IOException
  {
    if (args.length > 0 && args[0].equalsIgnoreCase("batch")) {
      final SyncObject o = new SyncObject();
      Thread t = new Thread() {
	public void run() {
	  SyncObject syncObj = o;
	  try {
	    String prolog = System.getProperty("se.sics.prologbeans.prolog",
					       "sicstus");
	    String[] command = {prolog, "-l", "evaluate",  "--goal", "main(batch)."};
	    // Load and start the Prolog example
	    Process process = Runtime.getRuntime().exec(command);

	    // Write all the error output that has no % in the start of the line
	    BufferedReader err =
	      new BufferedReader(new InputStreamReader(process.getErrorStream()));
	    String line;
	    while((line = err.readLine()) != null) {
	      if (line.length() > 0 && line.charAt(0) != '%') {
		System.err.println(line);
		// When port is found, notify that SICStus is running!
		if (line.startsWith("port:")) {
		  synchronized(syncObj) {
		    syncObj.sicstus_ready = true;
		    syncObj.notifyAll();
		  }
		}
	      }
	    }
	  } catch (Exception e) {
	    e.printStackTrace();
	  } finally {
	    synchronized(syncObj) {
	      syncObj.notifyAll();
	    }
	  }
	}
      };
      t.setDaemon(true);
      t.start();
      new EvaluateGUI(o, args);
    } else {
      new EvaluateGUI(args);
    }
  }
}
