// Copyright (c) 2004 SICS AB. All rights reserved.
/// <summary> EvaluateGUI.cs
/// A simple example showing how to connect a .NET (C#) GUI to a
/// running prolog server.
/// *
/// </summary>
using System;
using System.IO;
using System.Windows.Forms;
using se.sics.prologbeans;
using System.Threading;

public class EvaluateGUI : System.Windows.Forms.Form
{
  private System.Windows.Forms.TextBox textBox1;
  private System.Windows.Forms.Button button1;
  private System.Windows.Forms.TextBox textBox2;
  private PrologSession session;
  private System.Diagnostics.Process process;
  private System.String prolog;
  private string[] cmdArgs;
  /// <summary>
  /// Required designer variable.
  /// </summary>
  private System.ComponentModel.Container components = null;

  public EvaluateGUI(string[] args)
    {
      cmdArgs = args;
    }

  public void InitializeGUI()
    {
      //
      // Required for Windows Form Designer support
      //
      InitializeComponent();

      session = new PrologSession();

      if (cmdArgs.Length > 1 && cmdArgs[1].Equals("batch")) {
	// Wait one minute for sicstus to listen to the port.
	lock (this)
	  {
	    Monitor.Wait(this, TimeSpan.FromMilliseconds(60000));
	  }
	Thread batchThread = new Thread(new ThreadStart(this.batch_queries));
	batchThread.Start();
      }
    }

  /// <summary>
  /// Clean up any resources being used.
  /// </summary>
  protected override void Dispose( bool disposing )
    {
      if( disposing )
	{
	  if (components != null) 
	    {
	      components.Dispose();
	    }
	}
      base.Dispose( disposing );
    }

#region Windows Form Designer generated code
  /// <summary>
  /// Required method for Designer support - do not modify
  /// the contents of this method with the code editor.
  /// </summary>
  private void InitializeComponent()
    {
      this.textBox1 = new System.Windows.Forms.TextBox();
      this.button1 = new System.Windows.Forms.Button();
      this.textBox2 = new System.Windows.Forms.TextBox();
      this.SuspendLayout();
      // 
      // textBox1
      // 
      this.textBox1.Location = new System.Drawing.Point(0, 160);
      this.textBox1.Name = "textBox1";
      this.textBox1.Size = new System.Drawing.Size(240, 20);
      this.textBox1.TabIndex = 0;
      this.textBox1.Text = "";
      // 
      // button1
      // 
      this.button1.Location = new System.Drawing.Point(240, 160);
      this.button1.Name = "button1";
      this.button1.Size = new System.Drawing.Size(56, 23);
      this.button1.TabIndex = 1;
      this.button1.Text = "Evaluate";
      this.button1.Click += new System.EventHandler(this.evaluate_click);
      // 
      // textBox2
      // 
      this.textBox2.Location = new System.Drawing.Point(0, 0);
      this.textBox2.Multiline = true;
      this.textBox2.Name = "textBox2";
      this.textBox2.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
      this.textBox2.Size = new System.Drawing.Size(296, 160);
      this.textBox2.TabIndex = 2;
      this.textBox2.Text = "";
      // 
      // EvaluateGUI
      // 
      this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
      this.ClientSize = new System.Drawing.Size(292, 182);
      this.Controls.Add(this.textBox2);
      this.Controls.Add(this.button1);
      this.Controls.Add(this.textBox1);
      this.Name = "EvaluateGUI";
      this.Text = "Prolog Evaluator";
      this.ResumeLayout(false);

    }
#endregion

  /// <summary>
  /// The main entry point for the application.
  /// </summary>
  [STAThread]
    static void Main(string[] args) 
    {
      EvaluateGUI eg = new EvaluateGUI(args);
      if (args.Length > 1 && args[1].Equals("batch")) {
	eg.prolog = args[0];
	Thread ss = new Thread(new ThreadStart(eg.startSICStus));
	ss.Start();
      }
      eg.InitializeGUI();
      Application.Run(eg);
    }

  private void startSICStus()
    {
      try {
	  System.String commandArgs = " -l ../../../prologbeans/examples/evaluate/evaluate  --goal \"main(batch).\"";
	  System.Console.Error.WriteLine("DBG: Launching SICStus with \"" + prolog + commandArgs + "\"");
	  // Load and start the Prolog example
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
					
		  // When port is found, notify that SICStus is running!
		  if (line.StartsWith("port:"))
		    {
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
	}
      finally
	{
	  lock (this)
	    {
	      Monitor.Pulse(this);
	    }
	}
    }

  private void evaluate_click(object sender, System.EventArgs e)
    {
      try
	{
	  Bindings bindings = new Bindings().bind("E", textBox1.Text + '.');
	  session.connect();	/* This will connect if neccessary. */
	  QueryAnswer answer = session.executeQuery("evaluate(E,R)", bindings);
	  PBTerm result = answer.getValue("R");
	  if (result != null)
	    {
	      textBox2.AppendText(textBox1.Text + " = " + result + Environment.NewLine);
	      textBox1.Clear();
	    }
	  else
	    {
	      textBox2.AppendText("Error: " + answer.getError() + Environment.NewLine);
	    }
	}
      catch (System.Exception ex)
	{
	  textBox2.AppendText("Error when querying Prolog Server: " + ex.Message + Environment.NewLine);
	  Console.Error.WriteLine(ex);
	  Console.Error.Write(ex.StackTrace);
	  Console.Error.Flush();
	}

    }

  public void do_calc() {
    string[] ql = {"4+6", "11-5", "3*7", "9/3"};
    foreach (String q in ql) {
      if (!textBox1.IsHandleCreated) {
	IntPtr dummy = textBox1.Handle;	// force the Control to be created.
      }
      textBox1.Invoke(new MethodInvoker(delegate { textBox1.Text = q; }));
      evaluate_click(button1, new EventArgs());
    }
    Thread.Sleep(1000);	// Make it possible to see the GUI for a short while.
    QueryAnswer answer = session.executeQuery("shutdown");
    session.disconnect();
    process.WaitForExit(10000);	// wait 10 seconds.
    if (!process.HasExited) {
	try {
	    process.Kill();
	} catch (System.InvalidOperationException )
	    {
	    }
    }
    Environment.Exit(0);
  }

  private void batch_queries() {
    Thread calcThread = new Thread(new ThreadStart(this.do_calc));
    calcThread.Start();
  }
}
