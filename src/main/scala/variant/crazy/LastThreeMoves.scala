package chess
package variant.crazy

import chess.{Color, Move, Pos, Situation}


case class LastThreeMoves(b1:Option[Pos],b2:Option[Pos],b3:Option[Pos],w1:Option[Pos],w2:Option[Pos],w3:Option[Pos]) {
  def isValidMove(situation: Situation, tuple: (Pos, List[Move])): Boolean = {
    if(situation.color.white){
      if(w3.isDefined){
        println(s"Is this piece frozen? Situation is white. NO. can move from ${tuple._1} (IS VALID)...LastThreeMoves($b1,$b2,$b3,$w1,$w2,$w3)")

        true
      }else if(w1.exists( _ == tuple._1) || w2.exists( _ == tuple._1) )  {
        println(s"Is this piece frozen? Situation is white. Yes can NOT move from ${tuple._1}.. LastThreeMoves($b1,$b2,$b3,$w1,$w2,$w3)")

        false
      } else {
        println(s"Is this piece frozen? Situation is white. NO. can move from ${tuple._1} (IS VALID)...LastThreeMoves($b1,$b2,$b3,$w1,$w2,$w3)")

        true
      }
    }else{
      if(b3.isDefined){
        println(s"Is this piece frozen? Situation is bkacl. NO. can move from ${tuple._1} (IS VALID)...LastThreeMoves($b1,$b2,$b3,$w1,$w2,$w3)")

        true
      }else if(b1.exists( _ == tuple._1) || b2.exists( _ == tuple._1) ) {
        println(s"Is this piece frozen? Situation is black. Yes can NOT move from ${tuple._1}...LastThreeMoves($b1,$b2,$b3,$w1,$w2,$w3)")

        false
      } else {
        println(s"Is this piece frozen? Situation is BLACK. NO. can move from ${tuple._1} (IS VALID)...LastThreeMoves($b1,$b2,$b3,$w1,$w2,$w3)")
        true
      }
    }
  }
  def addMove(destination: Pos, color: Color) ={
    val lastThreeMovesIfInCurrentSession = if(color.white){
      LastThreeMoves(b1,b2,b3,Some(destination),w1,w2)
    }
    else {
      LastThreeMoves(Some(destination),b1,b2,w1,w2,w3)
    }
    if(color.white){
      if(b3.isDefined) {
       LastThreeMoves(None, None, None, Some(destination), None, None)
      }else {
        lastThreeMovesIfInCurrentSession
      }
    }else {
      lastThreeMovesIfInCurrentSession
    }
  }
}
