//
// Generated by JTB 1.2.1
//

package jde.debugger.interpret.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeChoice -> "void"
 *       | Type()
 * </PRE>
 */
public class ResultType implements Node {
   public NodeChoice nodeChoice;

   public ResultType(NodeChoice n0) {
      nodeChoice = n0;
   }

   public void accept(jde.debugger.interpret.visitor.Visitor v) {
      v.visit(this);
   }
   public Object accept(jde.debugger.interpret.visitor.ObjectVisitor v, Object argu) {
      return v.visit(this,argu);
   }
}
