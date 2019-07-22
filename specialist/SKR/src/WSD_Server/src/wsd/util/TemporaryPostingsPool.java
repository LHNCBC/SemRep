
/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

package wsd.util;
import java.io.*;
import java.util.*;

/**
 * TemporaryPostingsPool.java
 *
 *
 * Created: Wed Sep 19 16:41:56 2001
 *
 * @author <a href="mailto: "Willie Rogers</a>
 * @version
 */

public class TemporaryPostingsPool implements Serializable {
  String postingsFilename = "tpost";
  transient private RandomAccessFile postingsRAF = null;
  static final int BUFFER_SIZE = 1500;
  byte[] buffer = new byte[5000];
  int lastIndex = 0;

  public TemporaryPostingsPool ()
  {
    try {
      this.postingsRAF =  new RandomAccessFile(this.postingsFilename, "rw");
    } catch (FileNotFoundException exception) {
      exception.printStackTrace(System.err);
    }
  }

  public TemporaryPostingsPool (String aPostingsFilename)
  {
    this.postingsFilename = aPostingsFilename;
    try {
      this.postingsRAF =  new RandomAccessFile(this.postingsFilename, "rw");
    } catch (FileNotFoundException exception) {
      exception.printStackTrace(System.err);
    }
  }

  public TemporaryPostingsPool (String aPostingsFilename, String mode)
  {
    this.postingsFilename = aPostingsFilename;
    try {
      this.postingsRAF =  new RandomAccessFile(this.postingsFilename, mode);
    } catch (FileNotFoundException exception) {
      exception.printStackTrace(System.err);
    }
  }

  public void openPostings()
  {
    try {
      this.postingsRAF =  new RandomAccessFile(this.postingsFilename, "rw");
    } catch (FileNotFoundException exception) {
      exception.printStackTrace(System.err);
    }
  }

  public void openPostings(String mode)
  {
    try {
      this.postingsRAF =  new RandomAccessFile(this.postingsFilename, mode);
    } catch (FileNotFoundException exception) {
      exception.printStackTrace(System.err);
    }
  }

  /**
   * @param posting string data to post 
   * @param link address of previous posting in list.
   * @return address of posting
   */
  public int add(String posting, int link)
  {
    int address = -1;
    try {
      byte[] bytes = posting.getBytes();
      this.postingsRAF.writeInt(bytes.length);
      this.postingsRAF.write(bytes);
      this.postingsRAF.writeInt(link);
      address = this.lastIndex;
      this.lastIndex = this.lastIndex + bytes.length + 8;
    } catch (Exception exception) {
      System.err.println("add(): exception: " + exception.getMessage());
      exception.printStackTrace(System.err);
    }
    return address;
  }

  public List get(int address)
  {
    List aList = new ArrayList();
    int length = 0;
    int link = address;
    try {
      while (link != -1) {
	this.postingsRAF.seek(link);
	length = this.postingsRAF.readInt();
	if (this.buffer.length < length)
	  this.buffer = new byte[length+1];
	this.postingsRAF.read(this.buffer, 0, length);
	aList.add(new String(buffer, 0, length));
	link = this.postingsRAF.readInt();
      }
    } catch (Exception exception) {
      System.err.println("get(): exception: " + exception.getMessage());
      exception.printStackTrace(System.err);
    }
    return aList;
  }

  public List getv2(int address)
  {
    return new TemporaryPostingsList(this.postingsRAF, address);
  }

  public String getPosting(int address)
  {
    try {
      this.postingsRAF.seek(address);
      int length = this.postingsRAF.readInt();
      byte[] buffer = new byte[length+1];
      this.postingsRAF.read(buffer, 0, length);
      return new String(buffer, 0, length);
    } catch (Exception exception) {
      System.err.println("getPosting(): exception: " + exception.getMessage());
      exception.printStackTrace(System.err);
    }
    return null;
  }

  public void close()
  { 
    try {
      this.postingsRAF.close();
    } catch (Exception exception) {
      System.err.println("close(): exception: " + exception.getMessage());
      exception.printStackTrace(System.err);
    }
  }


  private class TemporaryPostingsList extends AbstractList 
    implements List
  {
    int address;
    RandomAccessFile postingsRAF;
    byte[] buffer = new byte[TemporaryPostingsPool.BUFFER_SIZE];

    TemporaryPostingsList(RandomAccessFile raf, int newAddress)
    {
      this.postingsRAF = raf;
      this.address = newAddress;
    }
    public Object get(int index)
    {
      int i = 0;
      int length = 0;
      int link = this.address;
      try {
	while (link != -1) {
	  this.postingsRAF.seek(link);
	  length = this.postingsRAF.readInt();
	  this.buffer = new byte[length+1];
	  this.postingsRAF.read(this.buffer, 0, length);
	  link = this.postingsRAF.readInt();
	  if (index == i) {
	    return new String(this.buffer, 0, length);
	  }
	  i++;
	}
      } catch (Exception exception) {
	System.err.println("get(): exception: " + exception.getMessage());
	exception.printStackTrace(System.err);
      }
      return null;
    }
    public int size()
    {
      int i = 0;
      int length = 0;
      int link = this.address;
      try {
	while (link != -1) {
	  this.postingsRAF.seek(link);
	  length = this.postingsRAF.readInt();
	  this.postingsRAF.read(this.buffer, 0, length);
	  link = this.postingsRAF.readInt();
	  i++;
	}
      } catch (Exception exception) {
	System.err.println("size(): exception: " + exception.getMessage());
	exception.printStackTrace(System.err);
      }
      return i;
    }
  }

  private class PostingsListIterator implements Iterator, ListIterator
  {
    int index = 0;
    int link;
    int length = 0;
    byte[] buffer = new byte[TemporaryPostingsPool.BUFFER_SIZE];
    RandomAccessFile postingsRAF;

    PostingsListIterator (RandomAccessFile raf, int newAddress)
    {
      this.postingsRAF = raf;
      this.link = newAddress;
    }

    public void 	add(Object o) { }
    public boolean 	hasNext() {
      return this.link != -1;
    }
    public boolean 	hasPrevious() { return false; }
    public Object 	next()
    {
      try {
	this.postingsRAF.seek(link);
	length = this.postingsRAF.readInt();
	this.postingsRAF.read(buffer, 0, length);
	link = this.postingsRAF.readInt();
	index++;
      } catch (Exception exception) {
	System.err.println("next(): exception: " + exception.getMessage());
	exception.printStackTrace(System.err);
      }
      return new String(buffer, 0, length);
    }
    public int 	nextIndex() {
      return index;
    }
    public Object previous() {
      return null;
    }
    public int previousIndex() {
      return index - 1;
    }
    public void remove() { }
    public void set(Object o) {}

  }

}// TemporaryPostingsPool
