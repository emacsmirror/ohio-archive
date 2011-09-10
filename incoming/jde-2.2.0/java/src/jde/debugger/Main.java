
package jde.debugger;

import java.io.*;
import java.net.*;
import java.util.*;

/**
 * Main class that has the "main" is called by jde.
 * <p>
 * Created: Wed Jul  7 21:02:39 1999
 *
 * @author Amit Kumar
 * @since 0.1
 */

public class Main implements Protocol {

    public static void main(String[] args) {
	if (args.length > 0) {
	    System.out.println("Usage: java jde.debugger.Main");
	    //	    Etc.dump(args);
	    System.out.flush();
	    System.exit(0);
	} else {
	    System.out.println(BR+"(" + JDE_INIT_DEBUG_SESSION+")"+BR);
	    System.out.flush();
	    try {
	      Jdebug.theDebugger.init();
	      Jdebug.theDebugger.start();
	    } catch (IOException ex) {
		System.out.println("I/O Error");
		System.exit(1);
	    }
	}
    }
    
} // Main
