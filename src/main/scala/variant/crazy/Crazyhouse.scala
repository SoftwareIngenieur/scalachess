package chess
package variant.crazy

import chess.format.Uci
import chess.variant.Chess960.{positions, positionsMap}
import chess.variant.Standard.backRank
import chess.variant.{Chess960, Standard, Variant}
import chess.{Bishop, Board, Color, Direction, Drop, History, Knight, Move, Pawn, Piece, PieceMap, Pos, Queen, Role, Rook, Situation, UniquePiece, UniquePieceMap, Valid}
import scalaz.Validation.failureNel

case object Crazyhouse
    extends Variant(
      id = 10,
      key = "crazyhouse",
      name = "Kagemusha",
      shortName = "Crazy",
      title = "Captured pieces can be dropped back on the board instead of moving a piece.",
      standardInitialPosition = true
    ) {
//this is going to have chess960 placement
  override val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1"
//  private val positions = Chess960.positions
  def pieces = Standard.pieces


//  private val positionsMap: Map[String, Int] = positions.zipWithIndex.toMap
//  def positionNumber(fen: String): Option[Int] =
//    positionsMap.get(fen.takeWhile('/' !=))

  def filterMovesAlreadyMoved(pos: Pos, listOfMoves: List[Move]): (Pos, List[Move]) = {
    (pos, listOfMoves.filter(m => m.dest.touches(m.orig)))
  }

  // In this variant, a player cant move a piece twice in a row
  override def validMoves(situation: Situation): Map[Pos, List[Move]]  =
    super.validMoves(situation).collect {
        case (pos, listOfMoves) if situation.board.crazyData.get.listOfTurnsAndUniquPiecesMoved.isValidMove(situation, (pos, listOfMoves)) =>
        filterMovesAlreadyMoved(pos,listOfMoves)
        case (pos, listOfMoves) => (pos, listOfMoves)
      }.toMap


  override def valid(board: Board, strict: Boolean) = {
    val pieces = board.pieces.values
   val crazyHouseValid = (Color.all forall validSide(board, false) _) &&
      (!strict || (pieces.count(_ is Pawn) <= 16 && pieces.size <= 32))
  //  val kageMushaValid = board.crazyData.map(_.listOfTurnsAndUniquPiecesMoved).map(getInvalidTurnsTooSoon(_, board))
    crazyHouseValid // && kageMushaValid.getOrElse(true)


  }

  private def canDropPawnOn(pos: Pos) = pos.y != 1 && pos.y != 8




  override def fiftyMoves(history: History): Boolean = false

  override def isIrreversible(move: Move): Boolean = move.castles

  override def finalizeBoard(board: Board, uci: Uci, capture: Option[Piece]): Board =
    uci match {
      case Uci.Move(orig, dest, promOption) =>
        board.crazyData.fold(board) { data =>
          val d1 = capture.fold(data) {
            data.store(_, dest)
          }
          val d2 = promOption.fold(d1.move(orig, dest)) { _ =>
            d1 promote dest
          }

          val (d3: CrazyhouseData, piece: Option[UniquePiece], somePos : Option[Pos]) = d2 withUniquePieceMapUpdated(orig, dest)

          val d4 = d3  // withOutedImpersonatorsUpdated(orig, dest, piece, board)
          val d5 = d4 withListOFRecentPiecesMoved(board.history.halfMoveClock, piece, somePos, capture.isDefined,Some(orig))
          val d6 = board withCrazyData d5
          println(d6.visual )
          d6 withMannRevieled(orig)

        }
      case _ => //was a drop
        board
    }

  private def canDropStuff(situation: Situation) =
    situation.board.crazyData.fold(false) { (data: CrazyhouseData) =>
      val roles = data.pockets(situation.color).roles
      roles.nonEmpty && possibleDrops(situation).fold(true) { squares =>
        squares.nonEmpty && {
          squares.exists(canDropPawnOn) || roles.exists(chess.Pawn !=)
        }
      }
    }

  override def staleMate(situation: Situation) =
    super.staleMate(situation) && !canDropStuff(situation)

  override def checkmate(situation: Situation) =
    super.checkmate(situation) && !canDropStuff(situation)

  // there is always sufficient mating material in Kagemusha
  override def opponentHasInsufficientMaterial(situation: Situation) = false
  override def isInsufficientMaterial(board: Board)                  = false

  def possibleDrops(situation: Situation): Option[List[Pos]] =
    if (!situation.check) None
    else situation.kingPos.map { blockades(situation, _) }

  private def blockades(situation: Situation, kingPos: Pos): List[Pos] = {
    def attacker(piece: Piece) = piece.role.projection && piece.color != situation.color
    def forward(p: Pos, dir: Direction, squares: List[Pos]): List[Pos] =
      dir(p) match {
        case None                                                 => Nil
        case Some(next) if situation.board(next).exists(attacker) => next :: squares
        case Some(next) if situation.board(next).isDefined        => Nil
        case Some(next)                                           => forward(next, dir, next :: squares)
      }
    Queen.dirs flatMap { forward(kingPos, _, Nil) } filter { square =>
      situation.board.place(Piece(situation.color, Knight), square) exists { defended =>
        !defended.check(situation.color)
      }
    }
  }

  val storableRoles = List(Pawn, Knight, Bishop, Rook, Queen)

}
