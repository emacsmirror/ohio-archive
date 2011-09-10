/*
 * Copyright (c) 2000    Paul Kinnucan
 *
 * $Revision: 1.13 $
 */

package jde.debugger;

import com.sun.jdi.*;

import java.util.*;

/**
 * Rep.java
 * <p>
 * Responsible for providing static methods used in spewing out string
 * representations.
 * <ul>
 * <li> A useful hierarchy:
 *  <ul>
 *  <li> Value
 *   <ul>
 *   <li> ObjectReference
 *    <ul>
 *    <li> StringReference
 *    <li> ArrayReference
 *    <li> ThreadReference
 *    <li> ThreadGroupReference
 *    <li> Other...
 *    </ul>
 *   <li> PrimitiveValue
 *    <ul>
 *    <li> BooleanValue
 *    <li> etc....
 *    </ul>
 *   </ul>
 *  </ul>
 * </ul> 
 * In our design, whenever we encounter an objectReference, we pass a
 * sort of summary to jde, as well as an 'id' to identify it with.
 * Whenever jde needs info about the objectReference, it uses the id to
 * uniquely identify the object.
 * <p>
 * Now, the representation that is sent across for the threads (ie to the
 * jde) depends on the context. When it is sent with reference to thread
 * commands, eg. get_threads, get_thread, get_object_monitors; it has
 * a lot of thread specific information, eg. its state and all.
 * <p>
 * When it's sent treating the thread as an object, eg. get_object, it's
 * represented differently, and a different set of information is sent.
 * <p>
 * Similary, when an array command is used, a different set of information
 * is sent across, as against when it's treated as an object.
 * <p>
 * Created: Tue Aug  3 16:36:54 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 */

public class Rep implements Protocol {

    /**
     * Returns a representation of a Location
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list "type-name" "sourcefile" lineNumber)
     * (list "type-name" nil lineNumber)
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> lineNumber is -1 if that information is not available
     * </ul>
     */
    static LispForm getLocationRep(Location loc) {
	String locationString = "(list \""+loc.declaringType().name()+"\"";
	try {
	    locationString += " \""+loc.sourceName()+"\"";
	} catch (AbsentInformationException ex) {
	    locationString += " nil";
	}
	locationString += " "+loc.lineNumber()+")";
	return new LispForm(locationString);
    }
    

    /**
     * Returns a representation of a method
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list "name of method" return-type-name
     *    (list [argument-type-name]*)
     *    ["final"] ["static"] ["native"] ["constructor"] ["abstract"]
     *    ["synchronized"] ["static_initializer"])
     * </pre>
     */
    static LispForm getMethodRep(Method m) {
	List l = m.argumentTypeNames();
	String argList = "(list";
	Iterator it = l.iterator();
	while (it.hasNext()) {
	    argList += " \""+it.next().toString()+"\"";
	}
	argList += ")";

	return new LispForm("(list \""+m.declaringType().name()+"\""
			    +" \""+m.name()+"\""
			    +" \""+m.returnTypeName()+"\""
			    + BR +argList
			    +(m.isFinal()?" \"final\"":"")
			    +(m.isStatic()?" \"static\"":"")
			    +(m.isNative()?" \"native\"":"")
			    +(m.isConstructor()?" \"constructor\"":"")
			    +(m.isAbstract()?" \"abstract\"":"")
			    +(m.isSynchronized()?" \"synchronized\"":"")
			    +(m.isStaticInitializer()
			      ?" \"static_initializer\"":"")
			    +")");
    }



    /**
     * Returns a representation of a local variable on a stack frame
     * <p>
     * <b>Syntax:</b>
     * <pre>
     * (list "name of variable" "type of variable")
     * </pre>
     */
    static public LispForm getLocalVariableRep(LocalVariable lv) {
	return new LispForm("(list"
			    + " \""+lv.name()+"\""
			    + " \""+lv.typeName()+"\")");
    }

    /**
     * Returns a representation of a (local variable, value) pair.
     * <p>
     * <b>Syntax:</b>
     * <pre>
     * ({@link #getLocalVariableRep local-variable} . {@link #getValueRep value})
     * </pre>
     */
    static public LispForm getLocalVariableValueRep(LocalVariable lv, Value v,
					     ObjectStore s) {
	return new LispForm("(cons "+getLocalVariableRep(lv)
			    +" "+getValueRep(v, s)+")");
    }

    /**
     * Returns a list of (local variable, value) pairs.
     * <p>
     * <b>Syntax:</b>
     * <pre>
     * (list [{@link #getLocalVariableValueRep (local variable, value) pair}]*)
     * </pre>
     */
    static public LispForm getLocalVariableValueMapRep(Map map, ObjectStore s) {
	String localVariablesValuesString = "(list ";
	Set keys = map.keySet();
	Iterator iter = keys.iterator();
	while (iter.hasNext()) {
	    LocalVariable localVariable = (LocalVariable)iter.next();
	    Value val = (Value)map.get(localVariable);
	    localVariablesValuesString +=
		 BR +getLocalVariableValueRep(localVariable, val, s);
	}
	localVariablesValuesString += ")";
	return new LispForm(localVariablesValuesString);
    }


    /**
     * Returns a representation of a field.
     * <p>
     * <b>Syntax:</b>
     * <pre>
     * (list "name of field" "type of field" ["transient"] ["volatile"]
     *                                       ["final"] ["static"])
     * </pre>
     */
    static LispForm getFieldRep(Field f) {
	return new LispForm("(list"
		+ " \""+f.name()+"\""
		+ " \""+f.typeName()+"\""
		+ (f.isTransient() ? " \"transient\"" : "")
		+ (f.isVolatile() ? " \"volatile\"" : "")
		+ (f.isFinal() ? " \"final\"" : "")
		+ (f.isStatic() ? " \"static\"" : "")
		+")");
    }

    /**
     * Returns a representation of a (field, value) pair.
     * <p>
     * <b>Syntax:</b>
     * <pre>
     * ({@link #getFieldRep field} . {@link #getValueRep value})
     * </pre>
     */
    static LispForm getFieldValueRep(Field f, Value v, ObjectStore s) {
	return new LispForm("(cons "+getFieldRep(f)+" "+getValueRep(v, s)+")");
    }

    /**
     * Returns a list of (field, value) pairs.
     * <p>
     * <b>Syntax:</b>
     * <pre>
     * (list [{@link #getFieldValueRep (field, value) pair}]*)
     * </pre>
     */
    static LispForm getFieldValueMapRep(Map map, ObjectStore s) {
	String fieldsValuesString = "(list ";
	Set keys = map.keySet();
	Iterator iter = keys.iterator();
	while (iter.hasNext()) {
	    Field field = (Field)iter.next();
	    Value val = (Value)map.get(field);
	    fieldsValuesString +=  BR +getFieldValueRep(field, val, s);
	}
	fieldsValuesString += ")";
	return new LispForm(fieldsValuesString);
    }
	
  private static String filterFPValue(String fpValue) {
    if (fpValue.equals("NaN"))
      return "\"NaN\"";
    else 
      if (fpValue.equals("-Infinity"))
	  return "\"-Infinity\"";
      else
	if (fpValue.equals("Infinity"))
	  return "\"Infinity\"";
        else
	  return fpValue;    
  }


    /**
     * Returns a representation of a 'value', that can be primitive
     * or an object reference, or void.
     * <p>
     * <b>Syntax:</b>
     * <pre>
     * (list "null")
     * (list "void")
     *
     * {@link #getObjectRep(ObjectReference,ObjectStore) object-rep}
     * 
     * (list "boolean" "true")      (list "boolean" "false")
     * (list "byte"    'byte-value')
     * (list "char"    'char-value')
     * (list "double"  double-value)
     * (list "float"   float-value)
     * (list "int"     int-value)
     * (list "long"    long-value)
     * (list "short"   short-value)
     * </pre>
     */
    static public LispForm getValueRep(Value value, ObjectStore store) {
	if (value == null) {
	    return new LispForm("(list \"null\")");
	} else if (value instanceof VoidValue) {
	    return new LispForm("(list \"void\")");
	} else if (value instanceof ObjectReference) {
	    return getObjectRep((ObjectReference)value, store);
	} else {
	    PrimitiveValue v = (PrimitiveValue)value;
	    if (v instanceof BooleanValue) {
		return new LispForm("(list \"boolean\" \""+v.booleanValue()+"\")");
	    } else if (v instanceof ByteValue) {
		return new LispForm("(list \"byte\" \""+v.byteValue()+"\")");
	    } else if (v instanceof CharValue) {
		return new LispForm("(list \"char\" \""+v.charValue()+"\")");
	    } else if (v instanceof DoubleValue) {
	      String sv = "" + v.doubleValue();
		return new LispForm("(list \"double\" "+filterFPValue(sv)+")");
	    } else if (v instanceof FloatValue) {
	      String sv = "" + v.floatValue();
		return new LispForm("(list \"float\" "+filterFPValue(sv)+")");
	    } else if (v instanceof IntegerValue) {
		return new LispForm("(list \"int\" \""+v.intValue()+"\")");
	    } else if (v instanceof LongValue) {
		return new LispForm("(list \"long\" \""+v.longValue()+"\")");
	    } else if (v instanceof ShortValue) {
		return new LispForm("(list \"short\" "+v.shortValue()+")");
	    }
	}
	return null;
    }

    /**
     * Returns information about an array
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * "Error message"
     * (list "type name" uniqueID ['t|nil] length [element]*)
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> The third argument (['t|nil]) indicates if the object has
     * been garbage collected in the debugee vm: it's nil if it hasn't.
     * <li> elements are only present if the index/length make sense. See
     * param list.
     * </ul>
     * <p>
     *
     * @param index if -1, represents the begin of index from where
     * elements are to be sent
     * @param length Number of elements to be sent
     */
    static public LispForm getArrayRep(ArrayReference a, ObjectStore store,
				int index, int length) {
	if (a == null) {
	    return new LispForm("\"Error!\"");
	} else {
	    store.put(a);

	    String elementsString = "";
	    try {
		if (index != -1) {
		    List elements = a.getValues(index, length);
		    Iterator it = elements.iterator();
		    while (it.hasNext()) {
			elementsString += " "+getValueRep((Value)it.next(),
							  store);
		    }
		}
	    } catch (IndexOutOfBoundsException ex) {
		elementsString = "\"Index out of bounds\"";
	    } catch (ObjectCollectedException ex) {
		elementsString = "\"The object has already been collected\"";
	    }
	    return new LispForm("(list "
				+ "\""+a.referenceType().name()+"\""
				+ " " +a.uniqueID()
				+ (a.isCollected() ? " 't":" nil")
				+ " " + a.length()
				+ elementsString + ")");
	}
    }


  /**
   * Prefix \ escapes to all \ and " characters in a string so that
   * the string can be read byte the Lisp interpreter. For efficiency,
   * if no such characters are found, the argument String itself
   * is returned.
   *
   * @param  str   String to be prefixed.
   * @return A String.
   *
   * @author David Hay
   * @author Mark Gibson
   * @author Steve Haflich
   * @author Charles Hart
   * @author David Dagon
   */
  public static String escapeString (String str) {

    if ( str.indexOf('\\') == -1 &&
	 str.indexOf('"')  == -1 )
      {
	return str;
      }
    else
      {
	StringBuffer buf = new StringBuffer(str.length() + 16);
	for ( int i = 0; i < str.length(); i++ ) {
	  char ch = str.charAt( i );
	  switch ( ch ) {
	  case '"':  buf.append("\\\"" ); break;
	  case '\\': buf.append("\\\\" ); break;
	  default:   buf.append( ch );     break;
	  }
	}
	return buf.toString();
      }
  }



    /**
     * Returns the value of a string
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * "Error message"
     * (list "java.lang.String" uniqueID ['t|nil] "string-value")
     * </pre>
     * <b>Comments:</b>
     * <ul>
     * <li> The third argument (['t|nil]) indicates if the object has
     * been garbage collected in the debugee vm: it's nil if it hasn't.
     * </ul>
     * <p>
     */
  static public LispForm getStringRep(StringReference s, ObjectStore store) {
    if (s == null) {
      return new LispForm("\"Error!\"");
    } else {
      store.put(s);

      return new LispForm("(list "
			  + "\""+s.referenceType().name()+"\""
			  + " "+s.uniqueID()
			  + (s.isCollected() ? " 't":" nil")
			  + " \"" + escapeString(s.value()) + "\")");
    }
  }
    

    /**
     * Returns a non-detailed representation of an object.
     *
     * @see #getObjectRep(ObjectReference,ObjectStore,boolean)
     */
    static public LispForm getObjectRep(ObjectReference o, ObjectStore store) {
	return getObjectRep(o, store, false);
    }

    /**
     * Returns a canonical representation of an object.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * "Error Message"
     * (list "null")
     * <i>Non-detailed</i>
     * (list "type of object" uniqueID ['t|nil])
     * <i>Detailed</i>
     * (list "type of object" uniqueID ['t|nil] {@link #getFieldValueMapRep fields-values})
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> The third argument (['t|nil]) indicates if the object has
     * been garbage collected in the debugee vm: it's nil if it hasn't.
     * </ul>
     */
    static public LispForm getObjectRep(ObjectReference o, ObjectStore store,
				 boolean detailed) {
	if (o == null) {
	    return new LispForm("(list \"null\")");
	} else {
	    store.put(o);
	    if (detailed) {
		// fields and values
		String fieldsValuesString;
		try {
		    // XXX a more complete list is available using
		    // allFields().... fyi
		    fieldsValuesString = getFieldValueMapRep(o.getValues(o.referenceType().visibleFields()), store).toString();
		} catch (ClassNotPreparedException ex) {
		    fieldsValuesString = "\"The class isn't prepared\"";
		} catch (ObjectCollectedException ex) {
		    fieldsValuesString = "\"The object has already been collected\"";
		} catch (Exception ex) {
		    fieldsValuesString = "\"Unable to access fields and values. Optimized class?\"";
		}
		
		return new LispForm("(list "
				    + "\""+o.referenceType().name()+"\""
				    + " "+o.uniqueID()
				    + (o.isCollected() ? " 't":" nil")+BR
				    + fieldsValuesString+")");
	    } else {
		return new LispForm("(list "
				    + "\""+o.referenceType().name()+"\""
				    + " "+o.uniqueID()
				    + (o.isCollected() ? " 't":" nil")
				    +")");
	    }
	}
    }
    

    /*
     * THREAD REPRESENTATIONS
     */

    /**
     * Returns information about monitors of an object.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list uniqueID "type of object" ['t|nil] {@link #getThreadRep owning-thread} (list [{@link #getThreadRep waiting-thread}]*))
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> The third argument (['t|nil]) indicates if the object has
     * been garbage collected in the debugee vm: it's nil if it hasn't.
     * </ul>
     */
    static LispForm getObjectMonitorsRep(ObjectReference o, ObjectStore store) {
	if (o == null) {
	    return new LispForm("null");
	} else {
	    store.put(o);

	    // owning thread
	    String owningThread;
	    try {
		ThreadReference t = o.owningThread();
		if (t == null) {
		    owningThread = "nil";
		} else {
		    owningThread = getThreadRep(t, store).toString();
		}
	    } catch (IncompatibleThreadStateException ex) {
		owningThread = "\"Information Not Available\"";
	    } catch (UnsupportedOperationException ex) {
		owningThread = "\"VM has no information\"";
	    }
	    
	    // waiting threads
	    String waitingThreadsString;
	    try {
		waitingThreadsString = "(list";
		List waitingThreads = o.waitingThreads();
		Iterator it = waitingThreads.iterator();
		while (it.hasNext()) {
		    waitingThreadsString +=  BR +getThreadRep((ThreadReference)it.next(), store);
		}
		waitingThreadsString += ")";
	    } catch (IncompatibleThreadStateException ex) {
		waitingThreadsString = "\"Information Not Available\"";
	    } catch (UnsupportedOperationException ex) {
		waitingThreadsString = "\"VM has no information\"";
	    }

	    
	    return new LispForm("(list "+o.uniqueID()+" "
				+"\""+o.referenceType().name()+"\""
				+ (o.isCollected() ? " 't":" nil")+BR
				+owningThread+ BR 
				+waitingThreadsString+")");
	}
    }

    /* thread information retrieval routines */

    

    /**
     * Returns a canonical representation of a given ThreadGroupReference.
     * <p>
     * <b>Syntax:</b>
     * <pre>
     * (list "ThreadGroup" uniqueID "name of threadgroup"
     *                     (list [{@link #getThreadRep(ThreadReference, ObjectStore) child thread}]*)
     *                     (list [{@link #getThreadGroupRep child threadgroup}]*))
     * </pre>
     */
    static LispForm getThreadGroupRep(ThreadGroupReference t,
				      ObjectStore store) {
	store.put(t);
	String rep = "(list \"ThreadGroup\" "+t.uniqueID()
	    +" \""+t.name()+"\" ";

	List list = t.threads();
	Iterator it = list.iterator();
	rep += BR+"(list";
	while (it.hasNext()) {
	    rep +=  BR +getThreadRep((ThreadReference)it.next(), store);
	}
	rep += ")";

	list = t.threadGroups();
	it = list.iterator();
	rep += BR+"(list";
	while (it.hasNext()) {
	    rep +=  BR +getThreadGroupRep((ThreadGroupReference)it.next(), store);
	}
	rep += ")";

	rep += ")";

	return new LispForm(rep);
    }

    /**
     * Returns a detailed thread representation.
     * @see #getThreadRep(ThreadReference, ObjectStore, boolean)
     */
    static LispForm getThreadRep(ThreadReference t, ObjectStore store) {
	return getThreadRep(t, store, true);
    }

    /**
     * Returns a canonical representation of a given ThreadReference.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * <i>Non-detailed</i>
     * (list "Thread" uniqueID "name of thread" <u>status</u> <u>currentState</u>)
     * <i>Detailed</i>
     * (list "Thread" uniqueID "name of thread" status currentState
     *                (list [{@link #getStackFrameRep stack-frame}]*)
     *                <u>owned-monitors-string</u>
     *                <u>current-contended-monitor-string</u>)
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> <u>status</u> is one of: "unknown", "waiting on monitor",
     *      "not started", "runnable", "sleeping", "waiting", and "zombie"
     *
     * <li> <u>currentState</u> is one of "normal", "suspended by debugger",
     *      and "suspended at breakpoint"

     * <li> <u>owned-monitors-string</u>:
     *  <pre>
     *  "Error Message"
     *  (list [{@link #getObjectRep(ObjectReference, ObjectStore) owned monitor}]*)
     *  </pre>
     * <li> <u>current-contended-monitor-string</u>:
     *  <pre>
     *  "Error Message"
     *  nil
     *  {@link #getObjectRep(ObjectReference, ObjectStore) current contended monitor}
     *  </pre>
     * <li>
     * Examples:
     * <pre>
     *    (list "Thread" 53 "Thread 1, continuous"
     *          "suspended by debugger" "waiting on monitor"
     *          (list 
     *             (list 0 "test.Test" "Test.java" 45))
     *          (list)
     *          (list "java.lang.String" 55))
     *
     *    (list "Thread" 54 "Thread 2"
     *          "suspended by debugger" "waiting on monitor"
     *          (list 
     *             (list 0 "java.lang.Thread" "Thread.java" -1)
     *             (list 1 "test.Test" "Test.java" 47))
     *          (list 
     *             (list "java.lang.String" 55)
     *             (list "java.lang.Integer" 61))
     *          (list))
     * </pre>
     * </ul>
     *
     * <p>
     * @param detailed True if a more detailed representation is desired:
     * includes the stackframe as well as information about the monitors.
     */
    static LispForm getThreadRep(ThreadReference t, ObjectStore store,
				 boolean detailed) {
	int status = t.status();
	String statusString = "unknown";
	switch (status) {
	case ThreadReference.THREAD_STATUS_MONITOR: {
	    statusString = "waiting on monitor";
	    break;
	}
	case ThreadReference.THREAD_STATUS_NOT_STARTED: {
	    statusString = "not started";
	    break;
	}
	case ThreadReference.THREAD_STATUS_RUNNING: {
	    statusString = "runnable";
	    break;
	}
	case ThreadReference.THREAD_STATUS_SLEEPING: {
	    statusString = "sleeping";
	    break;
	}
	case ThreadReference.THREAD_STATUS_WAIT: {
	    statusString = "waiting";
	    break;
	}
	case ThreadReference.THREAD_STATUS_ZOMBIE: {
	    statusString = "zombie";
	    break;
	}
	case ThreadReference.THREAD_STATUS_UNKNOWN: {
	    statusString = "unknown";
	    break;
	}
	default: {
	    break;
	}
	}

	// note that the above status string refers to the state of the 
	// thread *before* a suspension, if there was a suspension.

	/* Due to a bug in ThreadReference.isSuspended(), we need to
	   use suspendCount() */
	String stateString = "normal";
	if (t.isAtBreakpoint()) {
	    stateString = "suspended at breakpoint";
	} else if (t.suspendCount() > 0) {
	    stateString = "suspended by debugger";
	}

	if (detailed) {

	    // info on the stack

	    String stackString;
	    try {
		stackString = "(list";
		// a list of the stackframes is also sent...
		List stackFrames = t.frames();
		Iterator it = stackFrames.iterator();
		int index = 0;
		while (it.hasNext()) {
		    stackString +=  BR + getStackFrameRep((StackFrame)it.next(), index++);
		}
		stackString += ")";
	    } catch (IncompatibleThreadStateException ex) {
		stackString = "\"Information Not Available\"";
	    }

	    // info on the monitors

	    // owned monitors

	    String ownedMonitorsString;
	    try {
		ownedMonitorsString = "(list";
		List ownedMonitors = t.ownedMonitors();
		Iterator it = ownedMonitors.iterator();
		while (it.hasNext()) {
		    ownedMonitorsString +=  BR +getObjectRep((ObjectReference)it.next(), store);
		}
		ownedMonitorsString += ")";
	    } catch (IncompatibleThreadStateException ex) {
		ownedMonitorsString = "\"Information Not Available\"";
	    } catch (UnsupportedOperationException ex) {
		ownedMonitorsString = "\"VM has no information\"";
	    } catch (ObjectCollectedException ex) {
		ownedMonitorsString = "\"The object has been collected\"";
	    }
	    
	    // current contended monitor
	    // note, however, from the jdi api:
	    // The thread can be waiting for a monitor through entry into a
	    // synchronized method, the synchronized statement, or
	    // Object.wait(). The status() method can be used to
	    // differentiate between the first two cases and the third. 

	    String currentContendedMonitorString;
	    try {
		ObjectReference o = t.currentContendedMonitor();
		if (o == null) {
		    currentContendedMonitorString = "nil";
		} else {
		    currentContendedMonitorString =
			getObjectRep(o, store).toString();
		}
	    } catch (IncompatibleThreadStateException ex) {
		currentContendedMonitorString =
		    "\"Information Not Available\"";
	    } catch (UnsupportedOperationException ex) {
		currentContendedMonitorString =
		    "\"VM has no information\"";
	    } catch (ObjectCollectedException ex) {
		currentContendedMonitorString =
		    "\"The object has been collected\"";
	    }

	    store.put(t);
	    return new LispForm("(list \"Thread\""
				+" "+t.uniqueID()
				+" \""+t.name()+"\""
				+" \""+statusString+"\""
				+" \""+stateString+"\""
				+ BR +stackString
				+ BR +ownedMonitorsString
				+ BR +currentContendedMonitorString
				+")");
	} else {
	    store.put(t);
	    return new LispForm("(list \"Thread\""
				+" "+t.uniqueID()
				+" \""+t.name()+"\""
				+" \""+statusString+"\""
				+" \""+stateString+"\")");
	}
    }

    /**
     * Returns a canonical representation of a given StackFrame.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list "StackFrame" index "Information not available")
     * (list "StackFrame" index "type name" "source name" lineNumber "method name")
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> lineNumber is -1 for native methods
     * </ul>
     *
     * @param index Gives the index of this particular stack frame for
     * the thread. This basically goes into the string returned as a
     * convenience.
     */
    static LispForm getStackFrameRep(StackFrame s, int index) {
	try {
	    Location loc = s.location();
	    Method method = loc.method();
	    return new LispForm("(list "+index+" "
				+"\""+loc.declaringType().name()+"\" "
				+"\""+loc.sourceName()+"\" "
				+loc.lineNumber()+" "
				+"\""+method.name()+"\")");
	} catch (AbsentInformationException ex) {
	    return new LispForm("(list \"StackFrame\" "+index
				+" \"Information not available\")");
	}
    }
    
} // Rep

/*
 * $Log: Rep.java,v $
 * Revision 1.13  2000/07/28 06:26:31  paulk
 * Committing all modified files.
 *
 * Revision 1.12  2000/04/10 05:57:30  paulk
 * Publicized some methods.
 *
 * Revision 1.11  2000/04/01 06:02:37  paulk
 * Wrap NaN, Infinity, and -Infinity values in quotes to prevent Lisp evaluation errors.
 *
 * Revision 1.10  2000/03/17 03:35:23  paulk
 * Enhanced getStackFrameRep to return method. Thanks to Paul Michael Reilly <pmr@pajato.com>.
 *
 * Revision 1.9  2000/03/10 06:53:25  paulk
 * Escape quotes in strings.
 *
 * Revision 1.8  2000/03/04 08:58:11  paulk
 * Put quotes around byte, int, and long values to avoid Lisp
 * representation problems. Thanks to Charles Hart <cfhart@Z-TEL.com> for this fix.
 *
 */

// End Rep.java
