package gov.nih.nlm.nls.util.trie;

import java.io.Serializable;

public class Node <T> implements Serializable
{
  /** serialization version unique identifier for this class. */ 
  static final long serialVersionUID = -1613143146291047523L;

  protected char c;
  
  protected T value = null;
	
  protected Node <T> child = null;
  protected Node <T> sibling = null;
}
