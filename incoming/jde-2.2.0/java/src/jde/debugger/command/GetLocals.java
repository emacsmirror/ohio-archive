/*
 * Copyright (c) 2000    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import jde.debugger.Etc;
import com.sun.jdi.ObjectReference;
import jde.debugger.Rep;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.StackFrame;
import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.ObjectCollectedException;
import jde.debugger.LispForm;
import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.NativeMethodException;



/**
 * Gets the local variables in a specified stack frame.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_locals threadID stackFrameIndex
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id {@link Rep#getLocalVariableValueMapRep(Map, ObjectStore) local-variables-values})
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> Note that stackFrameIndex = 0 corresponds to the
 * current stackframe.
 * <li> The threadID and stackFrameIndex can be got from the
 *	'get_threads' command. Note that many evaluations
 *  might not be possible depending on the state of the thread
 * </ul>
 *
 * @author Amit Kumar
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 */
 public class GetLocals extends DebugApplicationCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
   public void doCommand() throws JDEException {

     boolean weSuspendedThread = false;
     ThreadReference tRef = null;

     if (args.size() != 2) 
       throw new JDEException("Insufficient arguments");

     try {

       Long uniqueID = Etc.safeGetLong(args.remove(0), "thread ID");
       int frameIndex = Etc.safeGetint(args.remove(0), "frame index");
	    
       Object oRef = app.getStore().get(uniqueID);
       if (oRef == null) {
	 throw new JDEException("No such thread exists");
       } else if (!(oRef instanceof ThreadReference)) {
	 throw new JDEException("Object is not a thread");
       }
	    
       tRef = (ThreadReference)oRef;

       tRef.suspend();
       weSuspendedThread = true;

       StackFrame frame = null;
       try {
	 frame = tRef.frame(frameIndex);
       } catch (IncompatibleThreadStateException ex) {
	 throw new JDEException("Thread is not suspended");
       } catch (IndexOutOfBoundsException ex) {
	 throw new JDEException("Invalid frame");
       } catch (ObjectCollectedException ex) {
	 throw new JDEException("The frame has already been garbage collected");
       }

       if (frame == null) {
	 throw new JDEException("Error ascertaining frame");
       }

       LispForm localVariableValues = null;
       try {
	 localVariableValues = 
	   Rep.getLocalVariableValueMapRep(frame.getValues(frame.visibleVariables()), 
					   app.getStore());
       } catch (AbsentInformationException ex) {
	 throw new JDEException("Local variable information not available: compile with -g");
       } catch (NativeMethodException ex) {
	 throw new JDEException("Can't access local variables in native methods");
       }

       app.signalCommandResult(cmdID, localVariableValues);
				    
     } finally {
       if (weSuspendedThread && (tRef != null)) tRef.resume();
     }
 
   }

  public Object clone() {return new GetLocals();}
  
} // GetLocals

/*
 * $Log: GetLocals.java,v $
 * Revision 1.1  2000/04/10 05:33:28  paulk
 * Initial revision.
 *
 *
 */

// End of GetLocals.java
