
package jde.debugger.spec;

import jde.debugger.*;

import com.sun.jdi.*;
import com.sun.jdi.request.*;

/**
 * BreakpointSpec.java
 * <p>
 *
 * <p>
 * Created: Thu Jul 15 12:59:42 1999
 *
 * @author Amit Kumar
 * @since 0.1
 */

abstract public class BreakpointSpec extends EventRequestSpec {

    public BreakpointSpec(Application app, ReferenceTypeSpec spec) {
	super(app, spec);
    }

    void setRequest(BreakpointRequest request) {
	super.setRequest(request);
    }
    
} // BreakpointSpec
