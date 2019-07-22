/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;

/**
 * <code>PBMonitor</code> is used to supervise and cancel queries that
 * takes too long time.
 */
class PBMonitor extends Thread {

  private static PBMonitor defaultMonitor = new PBMonitor();

  static PBMonitor getDefault() {
    return defaultMonitor;
  }

  private PrologSession[] sessions = new PrologSession[10];
  private int activeCount = 0;

  private PrologSession[] cancelList = new PrologSession[10];
  private int cancelCount = 0;

  private PBMonitor() {
    super("PBMonitor");
    setDaemon(true);
    start();
  } // PBMonitor constructor

  synchronized void queryStarted(PrologSession session) {
    if (activeCount == sessions.length) {
      PrologSession[] tmp = new PrologSession[activeCount + 10];
      System.arraycopy(sessions, 0, tmp, 0, activeCount);
      // [PM] 4.1.3 SPRM 11845, 11819
      sessions = tmp;
    }
    sessions[activeCount++] = session;
  }

  synchronized void queryFinished(PrologSession session) {
    for (int i = 0; i < activeCount; i++) {
      if (sessions[i] == session) {
        activeCount--;
        sessions[i] = sessions[activeCount];
        sessions[activeCount] = null;
        break;
      }
    }
  }

  public void run() {
    do {
      try {
        Thread.sleep(1000);
        checkQueries();
      } catch (Exception e) {
          if (PrologSession.debugging()) {
              System.err.println("PBMonitor: monitor caught an exception:");
              e.printStackTrace();
          }
      }
    } while (true);
  }

  // Note: may only be called with the timeout thread
  private void checkQueries() {
    long currentTime = System.currentTimeMillis();
    synchronized (this) {
      if (cancelList.length < activeCount) {
        cancelList = new PrologSession[activeCount];
      }
      // [PM] 4.1.3 FIXME: assert cancelCount == 0;
      for (int i = 0; i < activeCount; i++) {
        PrologSession sess = sessions[i];
        int timeout = sess.getTimeout();
        long startTime = sess.getQueryStartTime();
        if (currentTime > (startTime + timeout)) {
          activeCount--;
          sessions[i] = sessions[activeCount];
          sessions[activeCount] = null;
          if (startTime > 0L && timeout > 0) {
            // The query has taken too long and need to be cancelled
            cancelList[cancelCount++] = sess;
          }
          // Since we might have moved one session to this index it
          // will need to be rechecked again.
          i--;
        }
      }
    }

    if (cancelCount > 0) {
      // Notify all sessions that need to be cancelled. This should
      // not be done synchronized in case the cancellation takes time
      // and we do not want new queries to be blocked.
      for (int i = 0; i < cancelCount; i++) {
        System.err.println("PBMonitor: need to interrupt read/write!");
        cancelList[i].cancelQuery();
        cancelList[i] = null;
      }
      cancelCount = 0;
    }
  }

} // PBMonitor
