/*
 * Copyright (c) 2000    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import jde.debugger.Etc;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.StringReference;
import jde.debugger.Rep;



/**
 * 'get_string' command. Returns the value of a string
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_string objectID 
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id {@link Rep#getStringRep(StringReference, ObjectStore) string-representation})
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 */
 public class GetString extends DebugApplicationCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
   public void doCommand() throws JDEException {
 
     if (args.size() < 1)
       throw new JDEException("Insufficient arguments");

     Long uniqueID = Etc.safeGetLong(args.remove(0), "object ID");
     ObjectReference oRef = app.getStore().get(uniqueID);
	    
     if (oRef == null) {
       throw new JDEException("No such object exists");
     } else if (!(oRef instanceof StringReference)) {
       throw new JDEException("Object is not a string");
     }

     app.signalCommandResult(cmdID, Rep.getStringRep((StringReference)oRef, 
						     app.getStore()));
   }

  public Object clone() {return new GetString();}
  
} // Run

/*
 * $Log: GetString.java,v $
 * Revision 1.1  2000/03/03 07:10:29  paulk
 * Initial revision.
 *
 */

// End of GetString.java
