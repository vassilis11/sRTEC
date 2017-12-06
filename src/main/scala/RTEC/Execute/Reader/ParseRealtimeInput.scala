package RTEC.Execute.Reader

import RTEC._

import scala.util.parsing.combinator.JavaTokenParsers

object ParseRealtimeInput extends JavaTokenParsers {

    private var _happensAt: Seq[Main.InputHappensAt] = List()
    private var _holdsAt: Seq[Main.InputHoldsAt] = List()
    private var _holdsFor: Seq[Main.InputHoldsFor] = List()

    private def alphanumeric = ident | decimalNumber

    private def instantEventEntity: Parser[(Data.InstantEventId, Seq[String])] = "[" ~> ident ~ rep(alphanumeric) <~ "]" ^^ {
        case (name ~ args) =>
            (Data.InstantEventId(name, args.length), args)
    }

    private def fluentEntity: Parser[(Data.FluentId, Seq[String])] = ("[" ~> ident) ~ rep(alphanumeric) ~ ("=" ~> alphanumeric) <~ "]" ^^ {
        case (name ~ args ~ value) =>
            (Data.FluentId(name, args.length, value), args)
    }

    private def interval: Parser[(Int, Int)] = decimalNumber ~ ("-" ~> decimalNumber) ^^ {
        case (n1 ~ n2) => (n1.toInt, n2.toInt)
    }
    private def intervals: Parser[Data.Intervals] = "(" ~> rep1(interval) <~ ")" ^^ {
        case lIntervals => Data.Intervals(lIntervals.toVector)
    }

    private def holdsFor: Parser[Unit] = "HoldsFor" ~> fluentEntity ~ intervals ^^ {
        case (entity ~ time) =>
            _holdsFor +:= (entity._1, entity._2, time)
    }

    private def holdsAt: Parser[Unit] = "HoldsAt" ~> fluentEntity ~ decimalNumber ^^ {
        case (entity ~ time) =>
            _holdsAt +:= (entity._1, entity._2, time.toInt)
    }

    private def happensAt: Parser[Unit] = "HappensAt" ~> instantEventEntity ~ decimalNumber ^^ {
        case (entity ~ time) =>
            _happensAt +:= (entity._1, entity._2, time.toInt)
    }

    private def all: Parser[(Seq[Main.InputHappensAt], Seq[Main.InputHoldsAt], Seq[Main.InputHoldsFor])] = rep(happensAt | holdsAt | holdsFor) ^^ {
        case _ =>
            (_happensAt.reverse, _holdsAt.reverse, _holdsFor.reverse)
    }

    def get(source: String): Option[(Seq[Main.InputHappensAt], Seq[Main.InputHoldsAt], Seq[Main.InputHoldsFor])] = {
        parseAll(all, source) match {
            case Success(decl, _) =>
                Some(decl)
            case NoSuccess(errorMessage, _) =>
                println(s"(RT input): Parsing failed: $errorMessage")
                None
            case _ =>
                None
        }
    }

}
