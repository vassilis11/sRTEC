package RTEC.Execute.Reader

import RTEC._

import scala.util.parsing.combinator.JavaTokenParsers

object ParseDeclarations extends JavaTokenParsers {

    private def alphanumeric = ident | decimalNumber


    private def instantEvent: Parser[Data.InstantEvent] = (("Input" | "Output" | "Internal") <~ ":") ~ ("[" ~> alphanumeric) ~ (decimalNumber <~ "]") ^^ {
        case ("Input" ~ name ~ arity) =>
            Data.InputInstantEvent(name, arity.toInt)
        case ("Internal" ~ name ~ arity) =>
            Data.ComplexInstantEvent(name, arity.toInt)
        case ("Output" ~ name ~ arity) =>
            Data.OutputInstantEvent(name, arity.toInt)
    }
    private def instantEvents: Parser[Set[Data.InstantEvent]] = ("InstantEvents" ~ "{") ~> rep(instantEvent) <~ "}" ^^ {
        _.toSet
    }

    private def fluent: Parser[Data.Fluent] = (("Simple" | "InputSD" | "OutputSD" | "OutputSimple") <~ ":") ~ ("[" ~> alphanumeric) ~ decimalNumber ~ ("=" ~> alphanumeric <~ "]") ^^ {
        case ("OutputSimple" ~ name ~ arity ~ value) =>
            Data.OutputSimpleFluent(name, arity.toInt, value)
        case ("Simple" ~ name ~ arity ~ value) =>
            Data.InternalSimpleFluent(name, arity.toInt, value)
        case ("InputSD" ~ name ~ arity ~ value) =>
            Data.InputSDFluent(name, arity.toInt, value)
        case ("SD" ~ name ~ arity ~ value) =>
            Data.ComplexSDFluent(name, arity.toInt, value)
        case ("OutputSD" ~ name ~ arity ~ value) =>
            Data.OutputSDFluent(name, arity.toInt, value)
    }
    private def fluents: Parser[Set[Data.Fluent]] = ("Fluents" ~ "{") ~> rep(fluent) <~ "}" ^^ {
        _.toSet
    }

    private def inputEntity: Parser[Data.InputEntity] = ((ident  ~ decimalNumber) <~ ":") ~ rep1("[" ~> ident ~ opt("=" ~> ident) <~ "]") ^^ {
        case (name ~ numOfArgs ~ sources) =>
            val arity = numOfArgs.toInt
            val s = sources map {
                case (n ~ None) =>
                    Data.InstantEventId(n, arity)
                case (n ~ Some(value)) =>
                    Data.FluentId(n, arity, value)
            }

            Data.InputEntity(name, arity, s)
    }
    private def inputEntities: Parser[Set[Data.InputEntity]] = ("InputEntities" ~ "{") ~> rep(inputEntity) <~ "}" ^^ {
        _.toSet
    }

    private def builtEntities: Parser[Seq[Data.BuiltEntity]] = ("BuiltEntities" ~ "{") ~> rep(builtEntity) <~ "}"

    private def builtEntity: Parser[Data.BuiltEntity] = ((ident ~ decimalNumber) <~ ":") ~ rep1("[" ~> entitySource <~ "]") ^^ {
        case (name ~ numOfArgs ~ sources) =>
            Data.BuiltEntity(name, numOfArgs.toInt, sources)
    }
    private def entitySourceArg: Parser[(Option[String], Seq[String])] = opt(alphanumeric) ~ ("(" ~> repsep(alphanumeric, ",") <~ ")") ^^ {
        case (name ~ args) =>
            (name, args)
    }
    private def entitySource: Parser[Seq[(Option[String], Seq[String])]] = rep1(entitySourceArg)
    private def entities: Parser[(Set[Data.InputEntity], Seq[Data.BuiltEntity])] = {
        inputEntities ~ builtEntities ^^ {
            case (ie ~ be) =>
                (ie, be)
        }
    }

    private def eventId: Parser[Data.EventId] = "[" ~> ident ~ decimalNumber ~ opt("=" ~> alphanumeric) <~ "]" ^^ {
        case (name ~ arity ~ None) =>
            Data.InstantEventId(name, arity.toInt)
        case (name ~ arity ~ Some(value)) =>
            Data.FluentId(name, arity.toInt, value)
    }
    private def cachingItem: Parser[(Data.EventId, String)] = eventId ~ ("->" ~> ident) ^^ {
        case (target ~ entity) => (target, entity)
    }
    private def cachingOrder: Parser[Seq[Main.CachingTarget]] = ("CachingOrder" ~ "{") ~> rep1(cachingItem) <~ "}"

    private def all: Parser[(Set[Data.InstantEvent], Set[Data.Fluent], (Set[Data.InputEntity], Seq[Data.BuiltEntity]), Seq[Main.CachingTarget])] =
        instantEvents ~ fluents ~ entities ~ cachingOrder ^^ {
            case (iEs ~ fl ~ ent ~ co) =>
                (iEs, fl, ent, co)
        }

    def get(source: String): Option[(Set[Data.InstantEvent], Set[Data.Fluent], (Set[Data.InputEntity], Seq[Data.BuiltEntity]), Seq[Main.CachingTarget])] = {
        parseAll(all, source) match {
            case Success((iEs, fluents, entities, caching), _) => Some((iEs, fluents, entities, caching))
            case NoSuccess(errorMessage, _) => println(s"(Declarations) Parsing failed: $errorMessage"); None
        }
    }

}
