package chess
package variant.crazy

import chess.variant.crazy.Crazyhouse.storableRoles

case class CrazyhouseData(
                 pockets: Pockets,
                 // in crazyhouse, a promoted piece becomes a pawn
                 // when captured and put in the pocket.
                 // there we need to remember which pieces are issued from promotions.
                 // we do that by tracking their positions on the board.
                 promoted: Set[Pos],
                 pieceMap: UniquePieceMap,
                 listOfOuts: Set[UniquePiece],
                 listOfTurnsAndUniquPiecesMoved: LastThreeMoves
               ) {
  def visualStuff: Seq[Pos] = {
    listOfTurnsAndUniquPiecesMoved.w1 match {
      case Some(pos) => Seq(pos)
      case None =>Seq()
    }.
  }


  def withOutedImpersonatorsUpdated(orig: Pos, dest: Pos, somePiece: Option[UniquePiece], board: Board): CrazyhouseData = {
    println(s"withOutedImpersonatorsUpdated: somePiece: $somePiece")
    somePiece match {
      case Some(piece) if !isOuted(piece) && piece.isMinor =>
        // if (piece.positionsBetween(orig, dest).toSeq.forall(p => pieceThreatened(board, !piece.genericPiece.color, p))) {
        CrazyhouseData(pockets, promoted, this.pieceMap, listOfOuts + piece, this.listOfTurnsAndUniquPiecesMoved)
      // } else {
      //    this
      //  }

      case None => this
    }
  }
  def withListOFRecentPiecesMoved(halfMoveClock: Int, piece: Option[UniquePiece], somePos: Option[Pos]) = {
    if(halfMoveClock <  30){

      CrazyhouseData(pockets, promoted, pieceMap, listOfOuts, listOfTurnsAndUniquPiecesMoved.addAMove(
        somePos  , piece) )

    }else
      CrazyhouseData(pockets, promoted, pieceMap, listOfOuts,LastThreeMoves(None,None,None,None,None,None))
  }
  def isOuted(piece: UniquePiece): Boolean = {
    listOfOuts.contains(piece)
  }

  def withUniquePieceMapUpdated(orig: Pos, dest: Pos): (CrazyhouseData, Option[UniquePiece], Option[Pos]) = {
    val onThatSquare = pieceMap.filter(_._2 == orig)
    val pieceThatMoved = onThatSquare.headOption.map(_._1)
    pieceThatMoved match {
      case Some(uniquePiece) if uniquePiece.is(King) && orig == chess.Pos.E1 && dest == chess.Pos.G1 =>
        println("WE GOT A WHITE KINGSIDE CASTLE")
      case Some(uniquePiece) if uniquePiece.is(King) || orig == chess.Pos.E1 || dest == chess.Pos.G1 =>
        println(s"WE ALMOST GOT A WHITE KINGSIDE CASTLE $uniquePiece $orig $dest")
case _ => println("")
    }

    pieceThatMoved match {
      case Some(uniquePiece) if uniquePiece.is(King) && orig == chess.Pos.E1 && dest == chess.Pos.G1 =>

        println("WE GOT A WHITE KINGSIDE CASTLE")
 this.withUniquePieceMapUpdated(chess.Pos.E1, chess.Pos.F1  )._1
        .withUniquePieceMapUpdated( chess.Pos.F1 ,  chess.Pos.G1 )._1
        .withListOFRecentPiecesMoved(15, Some(UniquePiece.♔), Some(chess.Pos.G1))
        .withUniquePieceMapUpdated(chess.Pos.H1,chess.Pos.F1)


      case Some(uniquePiece) => {
        val mapWithoutPieceThatMoved = pieceMap.removed(uniquePiece)
        val mapWithPieceThatMovedAtNewLocation = mapWithoutPieceThatMoved.updated(uniquePiece, dest)

        (CrazyhouseData(pockets, promoted, mapWithPieceThatMovedAtNewLocation, listOfOuts, this.listOfTurnsAndUniquPiecesMoved), Some(uniquePiece), Some(dest))
      }
      case None => {
        println(s"ERROR: withUniquePieceMapUpdated is failing... $orig $dest ")
        (this, None, None)
      }

    }

  }


  def drop(piece: Piece): Option[CrazyhouseData] =
    pockets take piece map { nps =>
      copy(pockets = nps)
    }

  def store(piece: Piece, from: Pos) = {
    val theUniquePiece = pieceMap.filter(_._2 == from).head._1
    require(theUniquePiece.genericPiece == piece)
    copy(
      pockets = pockets store promoted(from).fold(piece.color.pawn, piece),
      promoted = promoted - from,
      pieceMap = pieceMap.removed(theUniquePiece)
    )
  }

  def promote(pos: Pos) = copy(promoted = promoted + pos)

  def move(orig: Pos, dest: Pos) =
    copy(
      promoted = if (promoted(orig)) promoted - orig + dest else promoted
    )
}

object CrazyhouseData {
  def     pieceMapToUnique(standardPieceMap: PieceMap) = {
    val uniquePieceMap = standardPieceMap.map{
      case (pos,piece) => {
        val uidFromStartingPosition = pos.x + 8 * pos.y - 9
        print(s"(${uidFromStartingPosition}, ${piece.unicode} )")

        (UniquePiece(uidFromStartingPosition,piece), pos)
      }
    }
    uniquePieceMap
  }
  def init(standardPieceMap: PieceMap) = {

    val uniquePieceMap = pieceMapToUnique(standardPieceMap).updated(UniquePiece(950, Piece(White,Pawn)), Pos.posAt(1,2).get)
    //mann

    CrazyhouseData(Pockets(Pocket(Nil),
      Pocket(Nil)), Set.empty,
      uniquePieceMap,
      Set.empty[UniquePiece],
      LastThreeMoves(None,None,None,None,None,None))
  }
}

case class Pockets(white: Pocket, black: Pocket) {

  def apply(color: Color) = color.fold(white, black)

  def take(piece: Piece): Option[Pockets] =
    piece.color.fold(
      white take piece.role map { np =>
        copy(white = np)
      },
      black take piece.role map { np =>
        copy(black = np)
      }
    )

  def store(piece: Piece) =
    piece.color.fold(
      copy(black = black store piece.role),
      copy(white = white store piece.role)
    )
}

case class Pocket(roles: List[Role]) {

  def take(role: Role) =
    if (roles contains role) Some(copy(roles = roles diff List(role)))
    else None

  def store(role: Role) =
    if (storableRoles contains role) copy(roles = role :: roles)
    else this
}
