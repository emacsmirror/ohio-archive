
package jde.debugger.spec;

import com.sun.jdi.*;

/**
 * ReferenceTypeSpec.java
 *
 *
 * Created: Mon Jul 19 13:19:23 1999
 *
 * @author Amit Kumar
 * @since 0.1
 */

public interface ReferenceTypeSpec  {

    /**
     * @return true if the ref type matches this spec
     */
    public boolean matches(ReferenceType refType);

} // ReferenceTypeSpec
