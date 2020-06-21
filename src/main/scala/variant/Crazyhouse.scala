package chess
package variant

import format.Uci

import scalaz.Validation.FlatMap._

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
//💂
//🃏	🃟
//👑
//🛏️
//🛅
//🕐	🕑	🕒	🕓	🕔	🕕	🕖	🕗	🕘	🕙	🕚	🕛	🕜	🕝	🕞	🕟
//🕠	🕡	🕢	🕣	🕤	🕥	🕦	🕧
//🎭
override val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1"
  val ANSI_RESET = "\u001B[0m"
  val ANSI_BLACK = "\u001B[30m"
  val ANSI_RED = "\u001B[31m"
  val ANSI_GREEN = "\u001B[32m"
  val ANSI_YELLOW = "\u001B[33m"
  val ANSI_BLUE = "\u001B[34m"
  val ANSI_PURPLE = "\u001B[35m"
  val ANSI_CYAN = "\u001B[36m"

  def pieces = Standard.pieces

  val ANSI_WHITE = "\u001B[37m"

  def getInvalidTurnsTooSoon(listOfTurnsAndUniquPiecesMoved:  Map[Int, Option[UniquePiece]], board: Board): Boolean = {
    true
  }
  // In this variant, a player cant move a piece twice in a row
  override def validMoves(situation: Situation): Map[Pos, List[Move]]  = {
    val allMoves       = super.validMoves(situation)
    situation.board.crazyData.get.recentTurns(situation.history.halfMoveClock, situation.color) match {
      case recentTurns: Seq[UniquePiece] =>{
        println("RECENT TURNS" + recentTurns.toSeq.mkString(","))

        allMoves.collect{
          case (k : Pos, moves) if
            !situation.board.crazyData.get. thispiecewasrecentlymoved3(recentTurns, k) => {
            println(s"colkecting $k")
            (k, moves)
          }

          }
      }

      }
  }
  override def valid(board: Board, strict: Boolean) = {
    val pieces = board.pieces.values
   val crazyHouseValid = (Color.all forall validSide(board, false) _) &&
      (!strict || (pieces.count(_ is Pawn) <= 16 && pieces.size <= 32))
  //  val kageMushaValid = board.crazyData.map(_.listOfTurnsAndUniquPiecesMoved).map(getInvalidTurnsTooSoon(_, board))
    crazyHouseValid // && kageMushaValid.getOrElse(true)


  }

  private def canDropPawnOn(pos: Pos) = pos.y != 1 && pos.y != 8

  override def drop(situation: Situation, role: Role, pos: Pos): Valid[Drop] =
    for {
      d1 <- situation.board.crazyData toValid "Board has no crazyhouse data"
      _  <- d1.validIf(role != Pawn || canDropPawnOn(pos), s"Can't drop $role on $pos")
      piece = Piece(situation.color, role)
      d2     <- d1.drop(piece) toValid s"No $piece to drop on $pos"
      board1 <- situation.board.place(piece, pos) toValid s"Can't drop $role on $pos, it's occupied"
      _      <- board1.validIf(!board1.check(situation.color), s"Dropping $role on $pos doesn't uncheck the king")
    } yield Drop(
      piece = piece,
      pos = pos,
      situationBefore = situation,
      after = board1 withCrazyData d2
    )

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

          val (d3: Data, piece: Option[UniquePiece]) = d2 withUniquePieceMapUpdated(orig, dest)

          val d4 = d3 withOutedImpersonatorsUpdated(orig, dest, piece, board)
          val d5 = d4 withListOFRecentPiecesMoved(board.history.halfMoveClock, piece)
          board withCrazyData d5
        }
      case _ => //was a drop
        board
    }

  private def canDropStuff(situation: Situation) =
    situation.board.crazyData.fold(false) { (data: Data) =>
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

  case class Data(
                   pockets: Pockets,
                   // in crazyhouse, a promoted piece becomes a pawn
                   // when captured and put in the pocket.
                   // there we need to remember which pieces are issued from promotions.
                   // we do that by tracking their positions on the board.
                   promoted: Set[Pos],
                   pieceMap: UniquePieceMap,
                   listOfOuts: Set[UniquePiece],
                   listOfTurnsAndUniquPiecesMoved: Map[Int, Option[UniquePiece]],
                   listOfOutPos: Seq[Pos] = Seq()
                 ) {
    def thispiecewasrecentlymoved3(recentTurns: Seq[UniquePiece], k: Pos): Boolean = {
      println(s"recent turns: $recentTurns.... $k")
      recentTurns.exists(thispiecewasrecentlymoved(_,k))
    }

    def recentTurns(halfMoveClock: Int, color: Color):Seq[UniquePiece] = {
      val ♜ = Piece.pieceFromUnicode("♜")
   Seq(listOfTurnsAndUniquPiecesMoved.getOrElse(halfMoveClock, UniquePiece(10002, ♜)),
      listOfTurnsAndUniquPiecesMoved.getOrElse(halfMoveClock-1, UniquePiece(10003,♜ )),
      listOfTurnsAndUniquPiecesMoved.getOrElse(halfMoveClock-2, UniquePiece(10004, ♜)),
      listOfTurnsAndUniquPiecesMoved.getOrElse(halfMoveClock-3, UniquePiece(10005, ♜)),
      listOfTurnsAndUniquPiecesMoved.getOrElse(halfMoveClock-4, UniquePiece(10006, ♜)))
        .collect {
          case u: UniquePiece if u.is(color) && u.id < 1000 => u
        }


    }


    def thispiecewasrecentlymoved(lastThreeMovesThisColor: UniquePiece, k: Pos): Boolean = {
     pieceMap(lastThreeMovesThisColor) == k
    }

    //    def recentlyMoved(numMoves: Int): Set[UniquePiece] = {
//Set.empty
//    }
    def withListOFRecentPiecesMoved(halfMoveClock: Int, piece: Option[UniquePiece]) = {
      val map = listOfTurnsAndUniquPiecesMoved.++(Map(halfMoveClock -> piece))
      Data(pockets, promoted, pieceMap, listOfOuts, map)
    }

    def withOutedImpersonatorsUpdated(orig: Pos, dest: Pos, somePiece: Option[UniquePiece], board: Board): Data = {
      println(s"withOutedImpersonatorsUpdated: somePiece: $somePiece")
      somePiece match {
        case Some(piece) =>  //if !isOuted(piece) =>
         // if (piece.positionsBetween(orig, dest).toSeq.forall(p => pieceThreatened(board, !piece.genericPiece.color, p))) {
            Data(pockets, promoted, this.pieceMap, listOfOuts + piece, this.listOfTurnsAndUniquPiecesMoved)
         // } else {
        //    this
        //  }

        case None => this
      }
    }

    def isOuted(piece: UniquePiece): Boolean = {
      listOfOuts.contains(piece)
    }

    def withUniquePieceMapUpdated(orig: Pos, dest: Pos): (Data, Option[UniquePiece]) = {

      val onThatSquare = pieceMap.filter(_._2 == orig)
      val pieceThatMoved = onThatSquare.headOption.map(_._1)

      pieceThatMoved match {
        case Some(uniquePiece) => {
          val mapWithoutPieceThatMoved = pieceMap.removed(uniquePiece)
          val mapWithPieceThatMovedAtNewLocation = mapWithoutPieceThatMoved.+((uniquePiece, dest))

          (Data(pockets, promoted, mapWithPieceThatMovedAtNewLocation, listOfOuts, this.listOfTurnsAndUniquPiecesMoved), Some(uniquePiece))
  }
  case None => {
    println("ERROR: withUniquePieceMapUpdated is failing")
    (this, None)
  }

}

    }


    def drop(piece: Piece): Option[Data] =
      pockets take piece map { nps =>
        copy(pockets = nps)
      }

    def store(piece: Piece, from: Pos) =
      copy(
        pockets = pockets store promoted(from).fold(piece.color.pawn, piece),
        promoted = promoted - from
      )

    def promote(pos: Pos) = copy(promoted = promoted + pos)

    def move(orig: Pos, dest: Pos) =
      copy(
        promoted = if (promoted(orig)) promoted - orig + dest else promoted
      )
  }

  object Data {
def     pieceMapToUnique(standardPieceMap: PieceMap) = {
  val uniquePieceMap = standardPieceMap.map{
    case (pos,piece) => {
      val uidFromStartingPosition = pos.x + 8 * pos.y - 9
      println(s"${piece.unicode} ${pos.x} + 8 * ${pos.y} -9 = ${uidFromStartingPosition}")
      (UniquePiece(uidFromStartingPosition,piece), pos)
    }
  }
  uniquePieceMap
}
    def init(standardPieceMap: PieceMap) = {

      Data(Pockets(Pocket(Nil), Pocket(Nil)), Set.empty, pieceMapToUnique(standardPieceMap), Set.empty[UniquePiece], Map.empty)
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
}
