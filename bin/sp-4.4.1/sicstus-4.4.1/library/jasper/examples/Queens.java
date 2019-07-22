/*
/src/sicstus/install/src/JDK/linux/jdk1.2.2_FCS/bin/java -native  \
	-Dsicstus.path=/home/perm/sicstus/sicstus385p/Utils/x86-linux-glibc2.1/lib/sicstus-3.8.5beta1 \
	-classpath /home/perm/sicstus/sicstus385p/Utils/x86-linux-glibc2.1/lib/sicstus-3.8.5beta1/bin/jasper.jar:. \
	-Djava.library.path=/home/perm/sicstus/sicstus385p/Utils/x86-linux-glibc2.1/lib \
	Queens
*/
import java.awt.*;
import java.awt.event.*;
import java.lang.*;
import java.io.*;
import java.net.*;

import se.sics.jasper.*;

class QueensCanvas extends Canvas
{
  public int boardSize, cellSize;
  private final int SPACING = 0;
  private final int OFFSET = 10;
  public boolean queens[][];
  private Image queen = null;

  public QueensCanvas(int boardSize, int cellSize)
  {
    int canvasSize = 
      boardSize*cellSize + 
      (boardSize-1)*this.SPACING +
      this.OFFSET*2;
    
    this.setSize(canvasSize, canvasSize + this.OFFSET*2);

    this.boardSize = boardSize;
    this.cellSize = cellSize;
      
    queens = new boolean[boardSize][boardSize];
  }

  private Color getCellColor(int i, int j)
  {
    if ((i+j) % 2 == 0)
      return Color.black;
    else
      return Color.white;
  }

  private Color getInvCellColor(int i, int j)
  {
    if (getCellColor(i,j) == Color.white)
      return Color.black;
    else
      return Color.white;
  }

  public void paint(Graphics g)
  {
    repaintBoard(g);
  }

  public void update(Graphics g)
  {
    repaintBoard(g);
  }

  public void repaintBoard(Graphics g)
  {
    int i, j;

    for (i = 0; i < boardSize; i++)
      {
	for (j = 0; j < boardSize; j++)
	  {
	    if (queens[i][j])
	      {
		drawQueen(g,i,j);
	      }
	    else
	      {
		g.setColor(getCellColor(i,j));
		g.fillRect
		  (OFFSET + i*(cellSize + SPACING),
		   OFFSET + j*(cellSize + SPACING),
		   cellSize,
		   cellSize);
	      }
	  }
      }

    /*
      for (i = 0; i < boardSize; i++)
      {
      for (j = 0; j < boardSize; j++)
      {
      drawAttacks(g,i,j);
      }
      }
    */
  }
  
  public void drawAttacks(Graphics g, int x, int y)
  {
    if (queens[x][y])
      {
	g.setColor(Color.red);
	
	g.drawLine(OFFSET,
		   OFFSET + y*cellSize + cellSize/2,
		   OFFSET + (boardSize*cellSize),
		   OFFSET + y*cellSize + cellSize/2);
	
	g.drawLine(OFFSET + x*cellSize + cellSize/2,
		   OFFSET,
		   OFFSET + x*cellSize + cellSize/2,
		   OFFSET + (boardSize*cellSize)
		   );
	
      }
  }

  public void drawQueen(Graphics g, int i, int j)
  {
    g.setColor(getCellColor(i,j));
    g.fillRect
      (OFFSET + i*(cellSize + SPACING),
       OFFSET + j*(cellSize + SPACING),
       cellSize,
       cellSize);
      
    g.setColor(Color.blue);
    g.fillOval
      (OFFSET + i*(cellSize + SPACING) + 3,
       OFFSET + j*(cellSize + SPACING) + 3,
       cellSize - 6,
       cellSize - 6);		
  }

  public void place(int x, int y, boolean hasQueen)
  {
    queens[x][y] = hasQueen;

    this.repaint();
  }

  public void removeAllQueens()
  {
    int i,j;

    for (i = 0; i < boardSize; i++)
      for (j = 0; j < boardSize; j++)
	queens[i][j] = false;
  }
}

class QueensFrame extends Frame
{
  MenuBar menubar;
  Menu filemenu;
  MenuItem itemQuit, itemRestart, itemNextSol, itemSep;
  QueensCanvas board;

  Query jquery = null;
  Term size, res, pofile;
  Term res_array[];
  Prolog pp;
  Font font;

  public QueensFrame(int boardSize, int cellSize, Prolog p)
    throws SPException,
           Exception // 3.9 *** Note: Exceptionhandling may change to throwing
                    //               more specific exceptions in future versions
                   //               of Jasper.
  {
    pp = p;
    size = pp.newTerm(boardSize);
    res = pp.newVariable();

    setLayout(new FlowLayout()); // [PM] 3.11.1 needed for MacOS X

    font = new Font("Helvetica",Font.PLAIN,12);

    menubar = new MenuBar();
    filemenu = new Menu("File");
    itemQuit = new MenuItem("Quit", new MenuShortcut(KeyEvent.VK_Q));
    itemRestart = new MenuItem("Restart", new MenuShortcut(KeyEvent.VK_R));
    itemNextSol = new MenuItem("Next Solution", new MenuShortcut(KeyEvent.VK_N));
    itemSep = new MenuItem("-");
    
    board = new QueensCanvas(boardSize, cellSize);
    this.add(board);

    itemQuit.addActionListener
      (new ActionListener() 
        { public void actionPerformed(ActionEvent e) { System.exit(0); }});
    
    itemRestart.addActionListener
      (new ActionListener() 
        { public void actionPerformed(ActionEvent e) 
          { 
            try { actionRestart(); }
//            catch ( SPException spe ) {}
            catch ( Exception spe ) { // 3.9 **** Note: Exceptionhandling may
                                     // change to throwing more specific
                                    // exceptions in future versions of Jasper.
              System.err.println("itemRestart failed to call actionRestart:");
              spe.printStackTrace(System.err);
            }
          }});
    
    itemNextSol.addActionListener
      (new ActionListener() 
        { public void actionPerformed(ActionEvent e) 
          { 
            try { actionNextSol(); }
//            catch ( SPException spe ) {}
            catch ( Exception spe ) { // 3.9 **** Note: Exceptionhandling may
                                     // change to throwing more specific
                                    // exceptions in future versions of Jasper.
              System.err.println("itemNextSol failed to call actionNextSol:");
              spe.printStackTrace(System.err);
            }
          }});
    
    this.addWindowListener
      (new WindowAdapter()
        { public void windowClosing(WindowEvent e) { System.exit(0); }});
    
    this.setMenuBar(menubar);
    menubar.add(filemenu);
    filemenu.add(itemRestart);
    filemenu.add(itemNextSol);
    filemenu.add(itemSep);
    filemenu.add(itemQuit);
    
    this.setTitle("JQueens");

    this.pack();

    this.setResizable(false);

// [PD] Post 3.12.1 show is deprecated in JDK 1.5
//    this.show();    
    this.setVisible(true);

    actionRestart();
  }

  public void placeQueen(int x, int y, boolean onoff)
  {
    board.place(x, y, onoff);
  }
  
//  public void placeQueens(SPTerm qargs[])
  public void placeQueens(Term qargs[]) // 3.9
  {
    int i, x, y;
    
    board.removeAllQueens();

    for (i = 0; i < res_array.length; i++)
      {
	x = i;
	y = Integer.valueOf(res_array[i].toString()).intValue() - 1;
	
	placeQueen(x,y,true);
      }
  }

  public void actionRestart()
    throws SPException,
           Exception // 3.9 *** Note: Exceptionhandling may change to throwing
                    //               more specific exceptions in future versions
                   //               of Jasper.

  {
    int i,j;

    if (jquery != null)
      jquery.close();
    /*
     * Note: Ideally openQuery(), all corresponding nextSolution(),
     * and the final close() or cut() should be enclosed in a
     * "synchronized (sp) {}" block. Failure to do so may not only
     * invalidate SPTerm objects created by other threads but may also
     * cause such SPTerm objects to refer to non-existing terms (this
     * may cause hard crashes in the prolog run-time system).  In this
     * example, however, after initialization the only SPTerm objects
     * are those created when nextSolution() is called in
     * actionNextSol().
     */
    {
      java.util.HashMap argmap = new java.util.HashMap();
      argmap.put("Size", size);
      argmap.put("Res", res);
      jquery = pp.openPrologQuery("jqueens:jqueens(Size, Res).", argmap);
    }
    actionNextSol();
  }

  public void actionNextSol()
    throws SPException,
           Exception // 3.9 *** Note: Exceptionhandling may change to throwing
                    //               more specific exceptions in future versions
                   //               of Jasper.

  {
    if (jquery.nextSolution())
      {
	res_array = res.toPrologTermArray();
    
	if (res_array == null) {
	  System.out.println("res is not a list");
        } else {
	  placeQueens(res_array);
        }
      }
    else
      {
        System.out.println("no more solutions");
      }
  }
}

public class Queens
{
  /* argv[0] = board size
     argv[1] = number of solutions in batch mode
  */
  public static void main(String argv[])
  {
    int bsize = ((argv.length > 0) ? (Integer.decode(argv[0])).intValue() : 8);
    int nsol = ((argv.length > 1) ? (Integer.decode(argv[1])).intValue() : 0);
    play(bsize, nsol);
  }
    
  public static void play(int boardsize, int nsol) {
    try {
      new Queens().startGame(boardsize, nsol);
//    } catch (SPException spe) {
      } catch (Exception spe) { // 3.9 **** Note: Exceptionhandling may
                               // change to throwing more specific
                              // exceptions in future versions of Jasper.
        System.err.println("Failed to create a Queens frame:");
        spe.printStackTrace(System.err);
      }
  }
    
    
  public void startGame(int boardSize, int nsol)
    throws SPException,
           Exception // 3.9 *** Note: Exceptionhandling may change to throwing
                    //               more specific exceptions in future versions
                   //               of Jasper.

  {
    // Using thread safe interface in 3.9
    SICStus sp = SICStus.getCaller();
    Prolog pp;
    if (sp == null) {
      // Java is top level, we need a Prolog client object.
      System.out.println("Queens.startGame(): creating a new Prolog");
      pp = Jasper.newProlog();
      // load jqueens.po with
      // load_files(library('jasper/examples/jqueens')).
      java.util.HashMap map = new java.util.HashMap();
      Term Path;
      String path = System.getProperty("Queens.codepath");
      if (path == null) {
        Path = pp.prologReadFromString("library('jasper/examples/jqueens').", null);
      } else {
        Path = pp.newTerm(path);
      }
 
      map.put("Path", Path); // 3.9
      pp.queryCutFail("load_files(Path).", map);
      new Solutions(pp, nsol, boardSize).run();
    } else {
      // SICStus is top level, get a client object from the SICStus object.
      System.out.println("Queens.startGame(): found an existing Prolog");
      pp = sp.newProlog();
      new Solutions(pp, nsol, boardSize).start();
      // Start a Prolog server in this thread.
      // Does not return until some other thread calls sp.stopServer().
      sp.startServer();
    }
  }

  class Solutions extends Thread
  {
    Prolog pp;
    QueensFrame qf;
    int nsol, boardSize;

    Solutions(Prolog p, int n, int bs)
    {
      pp = p;
      nsol = n;
      boardSize = bs;
    }

    public void run()
    {
      try {
        qf = new QueensFrame(boardSize, 20, pp);
        if (nsol > 0) {
          for (int i = 1; i <= nsol; i++) {
            // [PD] Maybe make sleeptime a parameter?
            Thread.currentThread().sleep(1000);
            qf.actionNextSol();
          }
          System.exit(0);
        }
// [PD] 3.10.1 catch a Throwable so that nothing gets thrown out of here
      } catch (Throwable thr) {
        System.err.println("Failed to create a Queens frame: (thr==" + thr + ")");
        thr.printStackTrace(System.err);
        System.exit(1);
      }
    }
  }

}




/** [PM] Keep the original indentation style
 *  Local variables:
 *      c-basic-offset: 2
 *      indent-tabs-mode: nil
 *  end:
 **/

