
package jde.debugger.spec;

import jde.debugger.*;

import com.sun.jdi.*;
import com.sun.jdi.request.*;

/**
 * AccessWatchpointSpec.java
 * <p>
 * 
 * <p>
 * Created: Tue Aug  3 15:34:14 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 */

public class AccessWatchpointSpec extends WatchpointSpec {
    
    public AccessWatchpointSpec(Application app, ReferenceTypeSpec refSpec,
				String fieldName) {
	super(app, refSpec, fieldName);
    }

    boolean resolve(ReferenceType refType) throws JDEException {
	Field field = refType.fieldByName(fieldName);
	if (field == null) {
	    throw new JDEException("'"+fieldName+"' does not exist in the class");
	}
        EventRequest er = refType.virtualMachine().eventRequestManager().createAccessWatchpointRequest(field);
	setRequest(er);
	return true;
    }

} // AccessWatchpointSpec
