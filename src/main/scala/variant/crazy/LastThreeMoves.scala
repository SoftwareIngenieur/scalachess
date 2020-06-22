package chess
package variant.crazy

import chess.{Color, Move, Pos, Situation}


case class LastThreeMoves(b1:Option[Pos],b2:Option[Pos],b3:Option[Pos],w1:Option[Pos],w2:Option[Pos],w3:Option[Pos]) {
  def addAMove(somePos: Option[Pos], piece: Option[UniquePiece]): LastThreeMoves = {
    (somePos, piece) match {
      case (Some(pos), Some(piece)) => addMove(pos,piece.genericPiece.color)
      case (_ , Some(piece)) if piece.genericPiece.color.white =>{
        println("OY!!! trying to add a move but didnt work. clearing out LastThreeMoves for white and partly for black")
LastThreeMoves(b1,None,None,None,None,None)
      }
      case (_ , Some(piece)) if piece.genericPiece.color.black =>{
        println("OY!!! trying to add a move but didnt work. clearing out LastThreeMoves for black")
        LastThreeMoves(None,None,None,w1,None,None)

      }

      case _ => {
        println("OY!!! trying to add a move but didnt work. clearing out LastThreeMoves for both")
        LastThreeMoves(None,None,None,None,None,None)
      }
    }
  }

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
      if(w3.isDefined) {
       LastThreeMoves(b1,b2,b3, Some(destination), None, None)
      }else {
        lastThreeMovesIfInCurrentSession
      }
    }else if(color.black){
      if(b3.isDefined) {
        LastThreeMoves(Some(destination), None, None, w1,w2,w3)
      }else {
        lastThreeMovesIfInCurrentSession
      }
    }
    else {
      println("Not white or black???? ERROR")
      lastThreeMovesIfInCurrentSession

    }
  }
}
