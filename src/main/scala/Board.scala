package chess

import Pos.posAt
import scalaz.Validation.FlatMap._
import scalaz.Validation.{failureNel, success}
import variant.Variant
import variant.crazy.Crazyhouse
import variant.crazy.CrazyhouseData

case class Board(
                  pieces: PieceMap,
                  history: History,
                  variant: Variant,
                  crazyData: Option[CrazyhouseData] = None
                ) {

  def withMannRevieled(position: Pos) = {
//   crazyData.map(_.pieceMap.collect{
//        case  (p,pos: Pos) if  pos.piotr == position.piotr =>
//          p
//      }).getOrElse(Seq.empty).toSeq.headOption match {
//        case Some(theRevealedMann) => Board(pieces.updated(position, theRevealedMann.genericPiece), history, variant, crazyData)
//        case None => this
//      }
    this



  }


  import implicitFailures._

  def apply(at: Pos): Option[Piece] = pieces get at

  def apply(x: Int, y: Int): Option[Piece] = posAt(x, y) flatMap pieces.get

  lazy val actors: Map[Pos, Actor] = pieces map {
    case (pos, piece) => (pos, Actor(piece, pos, this))
  }

  lazy val actorsOf: Color.Map[Seq[Actor]] = {
    val (w, b) = actors.values.toSeq.partition { _.color.white }
    Color.Map(w, b)
  }

  def rolesOf(c: Color): List[Role] =
    pieces.values
      .collect {
        case piece if piece.color == c => piece.role
      }
      .to(List)

  def actorAt(at: Pos): Option[Actor] = actors get at

  def piecesOf(c: Color): Map[Pos, Piece] = pieces filter (_._2 is c)

  lazy val kingPos: Map[Color, Pos] = pieces.collect {
    case (pos, Piece(color, King)) => color -> pos
  }

  def kingPosOf(c: Color): Option[Pos] = kingPos get c

  def check(c: Color): Boolean = c.white.fold(checkWhite, checkBlack)

  lazy val checkWhite = checkOf(White)
  lazy val checkBlack = checkOf(Black)

  private def checkOf(c: Color): Boolean =
    kingPosOf(c) exists { kingPos =>
      variant.kingThreatened(this, !c, kingPos)
    }

  def destsFrom(from: Pos): Option[List[Pos]] = actorAt(from) map (_.destinations)

  def seq(actions: Board => Valid[Board]*): Valid[Board] =
    actions.foldLeft(success(this): Valid[Board])(_ flatMap _)

  def place(piece: Piece) =
    new {
      def at(at: Pos): Valid[Board] =
        if (pieces contains at) failureNel("Cannot place at occupied " + at)
        else success(copy(pieces = pieces + ((at, piece))))
    }

  def place(piece: Piece, at: Pos): Option[Board] =
    if (pieces contains at) None
    else Some(copy(pieces = pieces + ((at, piece))))

  def take(at: Pos): Option[Board] =
    if (pieces contains at) Some(copy(pieces = pieces - at))
    else None

  def move(orig: Pos, dest: Pos): Option[Board] =
    if (pieces contains dest) None
    else
      pieces get orig map { piece =>
        copy(pieces = pieces - orig + ((dest, piece)))
      }

  def taking(orig: Pos, dest: Pos, taking: Option[Pos] = None): Option[Board] =
    for {
      piece <- pieces get orig
      takenPos = taking getOrElse dest
      if pieces contains takenPos
    } yield copy(pieces = pieces - takenPos - orig + (dest -> piece))

  def move(orig: Pos) =
    new {
      def to(dest: Pos): Valid[Board] = {
        if (pieces contains dest) failureNel("Cannot move to occupied " + dest)
        else
          pieces get orig map { piece =>
            copy(pieces = pieces - orig + (dest -> piece))
          } toSuccess ("No piece at " + orig + " to move")
      }
    }

  lazy val occupation: Color.Map[Set[Pos]] = Color.Map { color =>
    pieces.collect { case (pos, piece) if piece is color => pos }.to(Set)
  }

  def hasPiece(p: Piece) = pieces.values exists (p ==)

  def promote(pos: Pos): Option[Board] =
    for {
      pawn <- apply(pos)
      if pawn is Pawn
      b2 <- take(pos)
      b3 <- b2.place(pawn.color.queen, pos)
    } yield b3

  def castles: Castles = history.castles

  def withHistory(h: History): Board = copy(history = h)

  def withCastles(c: Castles) = withHistory(history withCastles c)

  def withPieces(newPieces: PieceMap) = copy(pieces = newPieces)

  def withVariant(v: Variant): Board = {
    if (v == Crazyhouse)
      copy(variant = v).ensureCrazyData
    else
      copy(variant = v)
  }

  def withCrazyData(data: CrazyhouseData)         = copy(crazyData = Some(data))
  def withCrazyData(data: Option[CrazyhouseData]) = copy(crazyData = data)
  def withCrazyData(f: CrazyhouseData => CrazyhouseData): Board =
    withCrazyData(f(crazyData | CrazyhouseData.init(this.pieces)))

  def ensureCrazyData = withCrazyData(crazyData | CrazyhouseData.init(this.pieces))

  def unmovedRooks =
    UnmovedRooks {
      history.unmovedRooks.pos.filter(pos =>
        apply(pos).exists(piece => piece.is(Rook) && piece.color.backrankY == pos.y)
      )
    }

  def fixCastles: Board =
    withCastles {
      if (variant.allowsCastling) {
        val wkPos   = kingPosOf(White)
        val bkPos   = kingPosOf(Black)
        val wkReady = wkPos.fold(false)(_.y == 1)
        val bkReady = bkPos.fold(false)(_.y == 8)
        def rookReady(color: Color, kPos: Option[Pos], left: Boolean) =
          kPos.fold(false) { kp =>
            actorsOf(color) exists { a =>
              a.piece.is(Rook) && a.pos.y == kp.y && (left ^ (a.pos.x > kp.x)) && history.unmovedRooks.pos(
                a.pos
              )
            }
          }
        Castles(
          whiteKingSide = castles.whiteKingSide && wkReady && rookReady(White, wkPos, false),
          whiteQueenSide = castles.whiteQueenSide && wkReady && rookReady(White, wkPos, true),
          blackKingSide = castles.blackKingSide && bkReady && rookReady(Black, bkPos, false),
          blackQueenSide = castles.blackQueenSide && bkReady && rookReady(Black, bkPos, true)
        )
      } else Castles.none
    }

  def updateHistory(f: History => History) = copy(history = f(history))

  def count(p: Piece): Int = pieces.values count (_ == p)
  def count(c: Color): Int = pieces.values count (_.color == c)

  def autoDraw: Boolean =
    variant.fiftyMoves(history) || variant.isInsufficientMaterial(this) || history.fivefoldRepetition

  def situationOf(color: Color) = Situation(this, color)

  def visual = {
    if(variant.crazyhouse) {

      format.Visual.>>| (this, crazyData.get.visualStuff(pieces)) + "\n\n\n" + format.Visual.>|(this, crazyData.get.visualStuff(pieces))

    }
    else
      format.Visual >> this
  }

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  override def toString = s"$variant Position after ${history.lastMove}\n$visual"
}

object Board {

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces.toMap, if (variant.allowsCastling) Castles.all else Castles.none, variant)

  def apply(pieces: Iterable[(Pos, Piece)], castles: Castles, variant: Variant): Board =
    Board(pieces.toMap, History(castles = castles), variant, variantCrazyData(variant,pieces.toMap))

  def init(variant: Variant): Board = Board(variant.pieces, variant.castles, variant)

  def empty(variant: Variant): Board = Board(Nil, variant)

  private def variantCrazyData(variant: Variant, pieces: PieceMap) = {

    (variant == Crazyhouse) option CrazyhouseData.init(pieces)
  }
}
