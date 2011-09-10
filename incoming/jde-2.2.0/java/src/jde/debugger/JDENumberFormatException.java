
package jde.debugger;

/**
 * JDENumberFormatException.java
 * <p>
 * 
 * <p>
 * Created: Thu Aug  5 18:52:41 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 */

public class JDENumberFormatException extends JDEException {
    
    public JDENumberFormatException(String str) {
	super("Non-numeric "+str);
    }
    
} // JDENumberFormatException
