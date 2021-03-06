//
// Generated by JTB 1.2.1
//

package jde.debugger.interpret.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeToken -> "null"
 * </PRE>
 */
public class NullLiteral implements Node {
   public NodeToken nodeToken;

   public NullLiteral(NodeToken n0) {
      nodeToken = n0;
   }

   public NullLiteral() {
      nodeToken = new NodeToken("null");
   }

   public void accept(jde.debugger.interpret.visitor.Visitor v) {
      v.visit(this);
   }
   public Object accept(jde.debugger.interpret.visitor.ObjectVisitor v, Object argu) {
      return v.visit(this,argu);
   }
}

