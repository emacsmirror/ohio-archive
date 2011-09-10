
package jde.debugger.spec;

import jde.debugger.*;

import com.sun.jdi.*;
import com.sun.jdi.request.*;

/**
 * ModificationWatchpointSpec.java
 * <p>
 * 
 * <p>
 * Created: Tue Aug  3 15:42:41 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 */

public class ModificationWatchpointSpec extends WatchpointSpec {

    public ModificationWatchpointSpec(Application app, ReferenceTypeSpec refSpec, String fieldName) {
	super(app, refSpec, fieldName);
    }

    boolean resolve(ReferenceType refType) throws JDEException {
	Field field = refType.fieldByName(fieldName);
	if (field == null) {
	    throw new JDEException("'"+fieldName+"' does not exist in the class");
	}
        EventRequest er = refType.virtualMachine().eventRequestManager().createModificationWatchpointRequest(field);
	setRequest(er);
	return true;
    }

} // ModificationWatchpointSpec
