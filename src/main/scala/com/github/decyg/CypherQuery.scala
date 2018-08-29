package com.github.decyg

trait CypherQuery {
  val asCypher: String
}

// this is the root class?
case class QueryRoot(){

  def MATCH = MatchNode()
  def UNWIND = UnwindNode()

}

trait ReadingClause extends CypherQuery{
  def RETURN = {
    new ReturnNode()
  }
}

case class ReturnNode() extends CypherQuery {
  override val asCypher: String = {
    "RETURN"
  }
}

case class MatchNode() extends ReadingClause{
  override val asCypher: String = _
}

case class UnwindNode() extends ReadingClause{
  override val asCypher: String = _
}

case class InQueryCallNode() extends ReadingClause{
  override val asCypher: String = _
}


trait UpdatingClause extends CypherQuery

case class CreateNode() extends UpdatingClause{
  override val asCypher: String = _
}

case class MergeNode() extends UpdatingClause{
  override val asCypher: String = _
}

case class DeleteNode() extends UpdatingClause{
  override val asCypher: String = _
}

case class SetNode() extends UpdatingClause{
  override val asCypher: String = _
}

case class RemoveNode() extends UpdatingClause{
  override val asCypher: String = _
}

trait RegularQuery extends CypherQuery

trait StandaloneCall extends CypherQuery

trait SingleQuery extends CypherQuery{
  def UNION() = {
    this.asCypher + " UNION"
  }
}

trait SinglePartQuery extends CypherQuery with SingleQuery

trait MultiPartQuery extends CypherQuery with SingleQuery

object yeet{
  println(

    QueryRoot()
      UNWIND



  )
}