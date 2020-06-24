package chess
package variant.crazy

import chess.{Color, Move, Pos, Situation}


case class LastThreeMoves(b1:Option[Pos],b2:Option[Pos],b3:Option[Pos],w1:Option[Pos],w2:Option[Pos],w3:Option[Pos]) {

  def isPawnHop(piece: UniquePiece, pos: Pos, originIfPawn: Option[Pos]) = originIfPawn match {
    case Some(origin) if piece.is(Pawn) =>{
      val numberOfPositionsoriginToDestinationInclusive = origin.⬍(pos).size
      println(s"Num pawn positions: $numberOfPositionsoriginToDestinationInclusive .. 2 if moved one, 3 if hopped")
      numberOfPositionsoriginToDestinationInclusive == 3
    }
    case None => false
  }

  def occupied(b1: Option[Pos]): Option[Pos] = b1 match {
    case Some(value) => Some(Pos(value.x+ 9 , value.y+ 9, ' '))
    case None =>None
  }

  def addPawnMove(piece: UniquePiece, pos: Pos, originIfPawn: Option[Pos]): LastThreeMoves = {
    originIfPawn match {
      case Some(origin) => piece.genericPiece.color match {
        case Color.White => {
          println(s"Adding white pawn move $piece $pos $originIfPawn")

          addMove(origin.up.getOrElse(pos),Color.White  ).  addMove(pos,Color.White  )match {
            case LastThreeMoves(b1, b2, b3, w1, w2, w3) =>
              LastThreeMoves(occupied(b1), occupied(b2), occupied(b3), w1, w2, w3 )
            //if you move the same piece it is like you have ushered in the next time-term
          }
        }
        case Color.Black =>{
          println(s"Adding black pawn move $piece $pos $originIfPawn")

          addMove(origin.down.getOrElse(pos),Color.Black  ).  addMove(pos,Color.Black  ) match {
            case LastThreeMoves(b1, b2, b3, w1, w2, w3) =>
              LastThreeMoves(b1, b2, b3, occupied(w1), occupied(w2), occupied(w3))
              //if you move the same piece it is like you have ushered in the next time-term
          }
        }
      }
      case None => {
        println(s"ERROR Failed to add pawn move $piece $pos $originIfPawn")

        addMove(pos,piece.genericPiece.color )}
    }


  }

  def addAMove(somePos: Option[Pos], piece: Option[UniquePiece], originIfPawn: Option[Pos]): LastThreeMoves = {
    (somePos, piece) match {
      case (Some(pos), Some(piece)) if isPawnHop(piece,pos,originIfPawn) => addPawnMove(piece,pos,originIfPawn)
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
