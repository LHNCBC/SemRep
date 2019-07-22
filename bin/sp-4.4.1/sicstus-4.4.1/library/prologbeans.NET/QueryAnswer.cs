
namespace se.sics.prologbeans
{
	
	/// <summary> <code>QueryAnswer</code> is the
	/// representation of an answer from the Prolog server. The
	/// <code>QueryAnswer</code> is returned by
	/// {@link se.sics.prologbeans.PrologSession} in response to a query and contains
	/// variable bindings, errors, and success/failure information. It also contains
	/// the variable bindings specified in the query.
	/// </summary>
	public class QueryAnswer:Bindings
	{
		
		// This term is on one of these forms:
		// - A list of the form "['='(VariableNameAsAtom,Value), ...]" (variable
		// bindings)
		// - The atom "no" (the prolog responded with 'no')
		// - The functor "error(ErrorReason)" (an error occurred)
		// [PM] 4.1.3 FIXME: synchronization story?
		readonly PBTerm answer;
		bool hasValues = false;
		bool bound = false;

		/// <summary> Creates a new <code>QueryAnswer</code> instance with the specified
		/// information.
		/// 
		/// </summary>
		/// <param name="answer">
		/// a {@link se.sics.prologbeans.PBTerm} value representing the
		/// Prolog response
		/// </param>
		/// <param name="bindings">the variable bindings for the query to which this is an answer
		/// </param>
		public QueryAnswer (PBTerm answer, Bindings bindings) : base (bindings)
		{
			this.answer = answer;
			// *** FIXME: this test is a bit contrived.
			// [PM] 4.2.1 FIXME: No need to check instanceof.
			hasValues = answer.ListCell && answer is PBListCell;
		}
		// QueryAnswer constructor
		
		/// <summary> Returns the value of the specified variable or <code>null</code> if the
		/// variable is not bound.
		/// 
		/// </summary>
		/// <param name="name">the name of the variable
		/// </param>
		/// <returns> the value of the variable as a {@link se.sics.prologbeans.PBTerm}
		/// or <code>null</code> if the variable is not bound
		/// </returns>
		public override PBTerm getValue (string name)
		{
			if (!bound) {
				if (hasValues) {
					// [PM] 4.1.3 No longer rely on answer being a PBListCell
					
					// copy all the new bindings into Bindings
					PBTerm list;
					// [PM] 4.1.3 for type checking only. Barfs if not a proper
					// list. (FIXME: redundant)
					answer.length ();
					for (list = answer; list.ListCell; list = list.getArgument (2)) {
						PBTerm bindTerm = list.getArgument (1);
						if (bindTerm.Name.Equals ("=")) {
							bind (bindTerm.getArgument (1).Name, bindTerm.getArgument (2));
						}
					}
					// assert list.isEmptyList();
				}
				
				bound = true;
			}
			return base.getValue (name);
		}

		/// <summary> Returns <code>true</code> if the query failed (i.e. the Prolog responded
		/// with 'no') and <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether the query failed
		/// </returns>
		public virtual bool queryFailed ()
		{
			return !hasValues && answer.Name.Equals ("no");
		}

		/// <summary> Returns <code>true</code> if an error occurred while processing the query
		/// and <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether the query threw an exception
		/// </returns>
		public virtual bool isError ()
		{
			return !hasValues && answer.Name.Equals ("error");
		}

		/// <summary> Returns the error reason or <code>null</code> if an error has not
		/// occurred or if no error reason is known.
		/// </summary>
		/// <returns> the error, or null, if none
		/// </returns>
		public virtual string getError ()
		{
			if (answer.Name.Equals ("error")) {
				return answer.getArgument (1).ToString ();
			}
			return null;
		}

		/// <summary> Returns a string description of this answer.</summary>
		public override string ToString ()
		{
			return answer.ToString ();
		}
	}
	// QueryAnswer
}
