package chess

import chess.format.pgn.SanStr
import chess.format.{ Fen, FullFen }
import chess.variant.Variant

import java.util.{ Optional, OptionalInt }
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

class LichessGame(private var game: Game):
  def getCurrentColor: String =
    game.situation.color.name

  def getFullMoveNumber: Integer =
    game.fullMoveNumber.asInstanceOf[Integer]

  def getHalfMoveNumber: Integer =
    game.ply.asInstanceOf[Integer]

  def getFen: String =
    Fen.write(game).asInstanceOf[String]

  def getSans: java.util.List[String] =
    game.sans
      .map: s =>
        s.toString()
      .asJava

  def getPly: Integer =
    game.ply.asInstanceOf[Integer]

  def getStartedAtPly: Integer =
    game.startedAtPly.asInstanceOf[Integer]

  def getPieceAt(at: Integer): Optional[Character] =
    game.situation
      .board(at.asInstanceOf[Square])
      .map: p =>
        p.forsyth.asInstanceOf[Character]
      .toJava

  def getLegalMoves: java.util.List[String] =
    game.situation.legalMoves
      .map: m =>
        m.toUci.uci
      .asJava

  def getLastMoveUci: Optional[String] =
    game.situation.board.history.lastMove
      .map: m =>
        m.uci
      .toJava

  def getLastMoveSan: Optional[String] =
    game.sans.lastOption
      .map: s =>
        s.asInstanceOf[String]
      .toJava

  def isMoveLegal(from: Integer, to: Integer): java.lang.Boolean =
    game.situation.legalMoves
      .find(move => move.orig == from.asInstanceOf[Square] && move.dest == to.asInstanceOf[Square])
      .isDefined

  def isMoveLegal(from: Integer, to: Integer, promotion: Character): java.lang.Boolean =
    val role = Role.promotable(promotion)

    game.situation.legalMoves
      .find(move =>
        move.orig == from.asInstanceOf[Square] && move.dest == to
          .asInstanceOf[Square] && ((move.promotion, role) match
          case (Some(promotion), Some(role)) => promotion.name == role.name
          case _                             => false
        )
      )
      .isDefined

  def move(from: Integer, to: Integer) =
    game(from.asInstanceOf[Square], to.asInstanceOf[Square], None) match
      case Right((game, move)) =>
        this.game = game
      case _ => throw RuntimeException("Unable to make move")

  def move(from: Integer, to: Integer, promotion: Character) =
    val role = Role.promotable(promotion)

    game(from.asInstanceOf[Square], to.asInstanceOf[Square], role) match
      case Right((game, move)) =>
        this.game = game
      case _ => throw RuntimeException("Unable to make move")

  def drop(square: Integer, dropRole: Character) =
    val role = Role.forsyth(dropRole)

    game.drop(role.get, square.asInstanceOf[Square]) match
      case Right((game, drop)) =>
        this.game = game
      case _ => throw RuntimeException("Unable to make drop")

  def undoMove =
    if this.game.sans.length > 0 then
      val poppedSans = game.sans.init

      val (initialGame, steps, __) = Replay.gameMoveWhileValid(
        poppedSans,
        game.situation.variant.initialFen,
        game.situation.variant
      )

      game = steps.lastOption
        .map: step =>
          step._1
        .getOrElse(initialGame)

  def isAutoDraw: java.lang.Boolean =
    game.situation.autoDraw

  def isCheckMate: java.lang.Boolean =
    game.situation.checkMate

  def isEnd: java.lang.Boolean =
    game.situation.end

  def isInsufficientMaterial: java.lang.Boolean =
    game.situation.opponentHasInsufficientMaterial

  def isStaleMate: java.lang.Boolean =
    game.situation.staleMate

  def winnerColor: Optional[Color] =
    game.situation.winner.toJava

  def checkSquare: Optional[Integer] =
    game.situation.checkSquare
      .map: s =>
        s.asInstanceOf[Integer]
      .toJava

object LichessGame:
  def create(variant: String): LichessGame =
    val realVariant = Variant.byName(variant).getOrElse(throw RuntimeException("No such variant"))

    LichessGame(Game(realVariant))

  def create(variant: String, fen: FullFen): LichessGame =
    val realVariant = Variant.byName(variant).getOrElse(throw RuntimeException("No such variant"))

    val game = Fen
      .read(realVariant, fen)
      .map: sit =>
        sit.color -> sit.withVariant(realVariant).board
      .map: (color, board) =>
        Game(realVariant).copy(situation = Situation(board, color))
      .getOrElse(throw RuntimeException("Can't create game"))

    LichessGame(game)

  def create(
      variant: String,
      sans: java.util.List[String],
      ply: Integer = 0,
      startedAtPly: Integer = 0
  ): LichessGame =
    val realVariant = Variant.byName(variant).getOrElse(throw RuntimeException("No such variant"))

    val game = Game(
      Situation(realVariant),
      sans.asScala.toVector.map: s =>
        s.asInstanceOf[SanStr],
      None,
      ply.asInstanceOf[Ply],
      startedAtPly.asInstanceOf[Ply]
    )

    LichessGame(game)

  def create(
      variant: String,
      sans: java.util.List[String]
  ): LichessGame =
    val realVariant = Variant.byName(variant).getOrElse(throw RuntimeException("No such variant"))

    val (game, steps, __) = Replay.gameMoveWhileValid(
      sans.asScala.toVector.map: s =>
        s.asInstanceOf[SanStr],
      realVariant.initialFen,
      realVariant
    )

    LichessGame(
      steps.lastOption
        .map: step =>
          step._1
        .getOrElse(game)
    )

  def getSquareFromKey(key: String): OptionalInt =
    Square
      .fromKey(key)
      .map: s =>
        s.asInstanceOf[Int]
      .toJavaPrimitive
