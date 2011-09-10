
package jde.debugger.spec;

import jde.debugger.*;

import com.sun.jdi.*;

import java.util.*;

/**
 * SourceNameReferenceTypeSpec.java
 *
 *
 * Created: Mon Jul 19 13:52:21 1999
 *
 * @author Amit Kumar
 * @since 0.1
 */

public class SourceNameReferenceTypeSpec implements ReferenceTypeSpec {

    final String sourceName;
    final int lineNumber;
    
    public SourceNameReferenceTypeSpec(String src, int line) {
	sourceName = src;
	lineNumber = line;
    }

    public String getSourceName() {
	return sourceName;
    }

    /**
     * Does the specified ReferenceType match this spec.
     */
  public boolean matches(ReferenceType refType) {
    try {
      if (refType.sourceName().equals(sourceName)) {
	try {
	  // Jdebug.theDebugger.signalDebug("Matching " + 
          // refType.sourceName() + ", " + lineNumber);
	  List locs = refType.locationsOfLine(lineNumber);
	  // Jdebug.theDebugger.signalDebug("  Matched");
	  // if we don't throw an exception then it was found
  
	  return (locs.size() > 0) ?  true : false;
    
	} catch(AbsentInformationException exc) {
	} catch(ObjectCollectedException  exc) {
	} catch(InvalidLineNumberException  exc) {
        } catch(ClassNotPreparedException  exc) {
	  // -- should not happen, so don't catch this ---
	}
      }
    } catch(AbsentInformationException exc) {
      // for sourceName(), fall through
    }
    return false;
  }

public String toString() { return sourceName+" "+lineNumber; }

} // SourceNameReferenceTypeSpec
