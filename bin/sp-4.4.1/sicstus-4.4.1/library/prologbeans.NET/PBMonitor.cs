using System;
using System.Threading;


namespace se.sics.prologbeans
{
	
	/// <summary> <code>PBMonitor</code> is used to supervise and cancel queries that
	/// takes too long time.
	/// </summary>
	class PBMonitor
	{
		internal static PBMonitor Default {
			get {
				return defaultMonitor;
			}
			
		}

		static PBMonitor createAndStart ()
		{
			PBMonitor m = new PBMonitor ();
			Thread t = new Thread (new ThreadStart (m.Run));
			t.IsBackground = true;
			t.Start ();
			return m;
		}

		static PBMonitor defaultMonitor = createAndStart ();

		PrologSession[] sessions = new PrologSession[10];
		int activeCount = 0;
		
		PrologSession[] cancelList = new PrologSession[10];
		int cancelCount = 0;

		public PBMonitor ()
		{
			; // empty
		}

		internal virtual void  queryStarted (PrologSession session)
		{
			lock (this) {
				if (activeCount == sessions.Length) {
					PrologSession[] tmp = new PrologSession[activeCount + 10];
					Array.Copy (sessions, 0, tmp, 0, activeCount);
					// [PM] 4.1.3 SPRM 11845, 11819
					sessions = tmp;
				}
				sessions [activeCount++] = session;
			}
		}
		
		internal virtual void  queryFinished (PrologSession session)
		{
			lock (this) {
				for (int i = 0; i < activeCount; i++) {
					if (sessions [i] == session) {
						activeCount--;
						sessions [i] = sessions [activeCount];
						sessions [activeCount] = null;
						break;
					}
				}
			}
		}
		static readonly TimeSpan oneSecond = new TimeSpan ((Int64)(10 * 1000 * 1000));

		public void  Run ()
		{
			do {
				try {
					Thread.Sleep (oneSecond);

					checkQueries ();
				} catch (Exception e) {
					if (PrologSession.debugging ()) {
						Console.Error.WriteLine ("PBMonitor: monitor caught an exception:");
						PrologSession.WriteStackTrace (e, Console.Error);
					}
				}
			} while (true);
		}
		
		// Note: may only be called within the timeout thread
		void  checkQueries ()
		{
			long currentTime = (DateTime.Now.Ticks - 621355968000000000) / 10000;
			lock (this) {
				if (cancelList.Length < activeCount) {
					cancelList = new PrologSession[activeCount];
				}
				// [PM] 4.1.3 FIXME: assert cancelCount == 0;
				for (int i = 0; i < activeCount; i++) {
					PrologSession sess = sessions [i];
					int timeout = sess.Timeout;
					long startTime = sess.QueryStartTime;
					if (currentTime > (startTime + timeout)) {
						activeCount--;
						sessions [i] = sessions [activeCount];
						sessions [activeCount] = null;
						if (startTime > 0L && timeout > 0) {
							// The query has taken too long and need to be cancelled
							cancelList [cancelCount++] = sess;
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
					Console.Error.WriteLine ("PBMonitor: need to interrupt read/write!");
					cancelList [i].cancelQuery ();
					cancelList [i] = null;
				}
				cancelCount = 0;
			}
		}
	}
}
