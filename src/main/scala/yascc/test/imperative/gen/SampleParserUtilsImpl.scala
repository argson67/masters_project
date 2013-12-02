package imperative

trait SampleParserUtilsImpl extends SampleParserUtils with SampleParserTrees with SampleParserTreesImpl {

  def strToInt: (
  String) => Num = (str => Num(Integer.parseInt(str)))
  
  def leftAssocAdd: (
  Seq[Expr]) => Expr = (es => leftAssoc(es, Add.apply))
  
  def leftAssocMultiply: (
  Seq[Expr]) => Expr = (es => leftAssoc(es, Times.apply))

  private def leftAssoc(es: Seq[Expr], op: (Expr, Expr) => Expr) = 
    es reduceLeft op
}