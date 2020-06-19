package chess

case class UniquePiece(id: Int, genericPiece: Piece){
  def positionsBetween(orig: Pos, dest: Pos): Set[Pos] = {


    genericPiece.role match {
      case King => Set(dest)
      case Queen => straightBetween(orig, dest) //(from onSameLine to) || (from onSameDiagonal to)
      case Rook => straightBetween(orig, dest) //from onSameLine to
      case Bishop => straightBetween(orig, dest) //from onSameDiagonal to
      case Knight => Set(dest) //todo
      case Pawn => straightBetween(orig, dest) //Piece.pawnEyes(color, from, to)
    }
  }

  def straightBetween(orig: Pos, dest: Pos) = {
    if (orig.onSameLine(dest)) {
      orig.<->(dest).toSet ++ orig.⬍(dest).toSet
    } else if (orig.onSameDiagonal(dest)) {
      orig.⬃⬂⬁⬀(dest).toSet
    }
    else {
      Set(dest)
    }
  }


  def is(c: Color) = genericPiece.is(c)

  def is(r: Role) = genericPiece.is(r)

  def isNot(r: Role) = genericPiece.isNot(r)

  def oneOf(rs: Set[Role]) = genericPiece.oneOf(rs)

  def isMinor = genericPiece.isMinor

  def isMajor = genericPiece.isMajor

  def forsyth: Char = genericPiece.forsyth

  // attackable positions assuming empty board
  def eyes(from: Pos, to: Pos): Boolean = genericPiece.eyes(from,to)


  // movable positions assuming empty board
  def eyesMovable(from: Pos, to: Pos): Boolean =
    genericPiece.eyesMovable(from,to)

  override def toString =s"(${id.toString},${genericPiece.unicode})"
}
case class Piece(color: Color, role: Role) {
  def unicode = {
    color match {
      case Color.White => role match {
        case King   => "♔"
        case Queen  => "♕"
        case Rook   =>  "♖"
        case Bishop =>  "♗"
        case Knight => "♘"
        case Pawn => "♙"
      }
      case Color.Black =>  role match {
        case King   =>   "♚"
        case Queen  =>"♛"
        case Rook   => "♜"
        case Bishop =>  "♝"
        case Knight => "♞"
        case Pawn => "♟︎"
      }
    }







  }

  def is(c: Color)   = c == color
  def is(r: Role)    = r == role
  def isNot(r: Role) = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def isMinor = oneOf(Set(Knight, Bishop))
  def isMajor = oneOf(Set(Queen, Rook))

  def forsyth: Char = if (color == White) role.forsythUpper else role.forsyth

  // attackable positions assuming empty board
  def eyes(from: Pos, to: Pos): Boolean =
    role match {
      case King   => from touches to
      case Queen  => (from onSameLine to) || (from onSameDiagonal to)
      case Rook   => from onSameLine to
      case Bishop => from onSameDiagonal to
      case Knight =>
        from.color != to.color && {
          val xd = from xDist to
          val yd = from yDist to
          (xd == 1 && yd == 2) || (xd == 2 && yd == 1)
        }
      case Pawn => Piece.pawnEyes(color, from, to)
    }

  // movable positions assuming empty board
  def eyesMovable(from: Pos, to: Pos): Boolean =
    if (role == Pawn) Piece.pawnEyes(color, from, to) || {
      (from ?| to) && {
        val dy = to.y - from.y
        if (color.white) (dy == 1 || (from.y <= 2 && dy == 2))
        else (dy == -1 || (from.y >= 7 && dy == -2))
      }
    }
    else eyes(from, to)

  override def toString = s"$color-$role".toLowerCase
}

object Piece {

  def fromChar(c: Char): Option[Piece] =
    Role.allByPgn get c.toUpper map {
      Piece(Color(c.isUpper), _)
    }

  private def pawnEyes(color: Color, from: Pos, to: Pos) =
    (from xDist to) == 1 && (to.y - from.y) == {
      if (color.white) 1 else -1
    }
}
