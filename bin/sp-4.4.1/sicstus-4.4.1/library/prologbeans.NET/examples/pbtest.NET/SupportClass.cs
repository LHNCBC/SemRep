using System;
public interface IThreadRunnable
{
	void Run();
}

public class SupportClass
{
	public static void WriteStackTrace(System.Exception throwable, System.IO.TextWriter stream)
	{
//	        stream.WriteLine(throwable.Message);
//		stream.WriteLine(throwable.StackTrace);
	        stream.WriteLine(throwable.ToString());
		stream.Flush();
	}

	/*******************************/
	public class ThreadClass:IThreadRunnable
	{
		private System.Threading.Thread threadField;

		public ThreadClass()
		{
			threadField = new System.Threading.Thread(new System.Threading.ThreadStart(Run));
		}

		public ThreadClass(System.Threading.ThreadStart p1)
		{
			threadField = new System.Threading.Thread(p1);
		}

		public virtual void Run()
		{
		}

		public virtual void Start()
		{
			threadField.Start();
		}

		public System.Threading.Thread Instance
		{
			get
			{
				return threadField;
			}
			set
			{
				threadField	= value;
			}
		}

		public System.String Name
		{
			get
			{
				return threadField.Name;
			}
			set
			{
				if (threadField.Name == null)
					threadField.Name = value; 
			}
		}

		public System.Threading.ThreadPriority Priority
		{
			get
			{
				return threadField.Priority;
			}
			set
			{
				threadField.Priority = value;
			}
		}

		public bool IsAlive
		{
			get
			{
				return threadField.IsAlive;
			}
		}

		public bool IsBackground
		{
			get
			{
				return threadField.IsBackground;
			} 
			set
			{
				threadField.IsBackground = value;
			}
		}

		public void Join()
		{
			threadField.Join();
		}

		public void Join(long p1)
		{
			lock(this)
			{
				threadField.Join(new System.TimeSpan(p1 * 10000));
			}
		}

		public void Join(long p1, int p2)
		{
			lock(this)
			{
				threadField.Join(new System.TimeSpan(p1 * 10000 + p2 * 100));
			}
		}

		public void Resume()
		{
			threadField.Resume();
		}

		public void Abort()
		{
			threadField.Abort();
		}

		public void Abort(System.Object stateInfo)
		{
			lock(this)
			{
				threadField.Abort(stateInfo);
			}
		}

		public void Suspend()
		{
			threadField.Suspend();
		}

		public override System.String ToString()
		{
			return "Thread[" + Name + "," + Priority.ToString() + "," + "" + "]";
		}

		public static ThreadClass Current()
		{
			ThreadClass CurrentThread = new ThreadClass();
			CurrentThread.Instance = System.Threading.Thread.CurrentThread;
			return CurrentThread;
		}
	}

}
