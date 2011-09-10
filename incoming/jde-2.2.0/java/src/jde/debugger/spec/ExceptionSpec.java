
package jde.debugger.spec;

import jde.debugger.*;

import com.sun.jdi.*;
import com.sun.jdi.request.*;

/**
 * ExceptionSpec.java
 * <p>
 * 
 * <p>
 * Created: Mon Aug  2 17:01:35 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 */

public class ExceptionSpec extends EventRequestSpec {

    boolean notifyCaught;

    boolean notifyUncaught;

    public ExceptionSpec(Application app, ReferenceTypeSpec spec, 
			 boolean notifyCaught, boolean notifyUncaught) {
	super(app, spec);
	this.notifyCaught = notifyCaught;
	this.notifyUncaught = notifyUncaught;
    }

    public boolean resolve(ReferenceType refType) {
	ExceptionRequest er = refType.virtualMachine().eventRequestManager().createExceptionRequest(refType, notifyCaught, notifyUncaught);
	super.setRequest(er);
	return true;
    }

} // ExceptionSpec
