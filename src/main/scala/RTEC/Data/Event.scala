package RTEC.Data


sealed trait EventId {
    def ==(x: EventId): Boolean
    def !=(x: EventId): Boolean
}

case class InstantEventId(name: String, arity: Int) extends EventId {
    def ==(x: EventId): Boolean = x match {
        case InstantEventId(n, a) =>
            (name == n) && (arity == a)
        case _ =>
            false
    }

    def !=(x: EventId): Boolean = ! ==(x)
}

case class FluentId(name: String, numOfArgs: Int, value: String) extends EventId {
    def ==(x: EventId): Boolean = x match {
        case FluentId(n, noa, v) =>
            (name == n) && (numOfArgs == noa) && (value == "_" || v == "_" || value == v)
        case _ =>
            false
    }

    def !=(x: EventId): Boolean = ! ==(x)
}


//sealed trait EventType
//
//sealed trait IEType extends EventType
//case object InputIE extends IEType
//case object OutputIE extends IEType
//
//sealed trait FluentType extends EventType
//case object SimpleFluent extends FluentType
//case object InputSDFluent extends FluentType
//case object OutputSDFluent extends FluentType

// Event root type
sealed abstract class Event(val name: String, val arity: Int) {
    def id: EventId
}

sealed abstract class InstantEvent(override val name: String,
                                        override val arity: Int)
    extends Event(name, arity) {
    override def id = InstantEventId(name, arity)
}

sealed abstract class Fluent(override val name: String,
                             override val arity: Int,
                             val value: String)
    extends Event(name, arity) {
    override def id = FluentId(name, arity, value)
}

sealed trait InputEvent
sealed trait ComplexEvent
sealed trait OutputEvent

case class InputInstantEvent(override val name: String,
                                   override val arity: Int)
    extends InstantEvent(name, arity)
    with InputEvent

case class ComplexInstantEvent(override val name: String,
                               override val arity: Int)
    extends InstantEvent(name, arity)
    with ComplexEvent

case class OutputInstantEvent(override val name: String,
                                    override val arity: Int)
    extends InstantEvent(name, arity)
    with ComplexEvent
    with OutputEvent

sealed abstract class SimpleFluent(override val name: String,
                                   override val arity: Int,
                                   override val value: String)
    extends Fluent(name, arity, value)
    with ComplexEvent

case class InternalSimpleFluent(override val name: String,
                                override val arity: Int,
                                override val value: String)
  extends SimpleFluent(name, arity, value)

case class OutputSimpleFluent(override val name: String,
                              override val arity: Int,
                              override val value: String)
    extends SimpleFluent(name, arity, value)
    with OutputEvent

sealed abstract class SDFluent(override val name: String,
                               override val arity: Int,
                               override val value: String)
    extends Fluent(name, arity, value)

case class InputSDFluent(override val name: String,
                               override val arity: Int,
                               override val value: String)
    extends SDFluent(name, arity, value)
    with InputEvent

case class ComplexSDFluent(override val name: String,
                           override val arity: Int,
                           override val value: String)
    extends SDFluent(name, arity, value)
    with ComplexEvent

case class OutputSDFluent(override val name: String,
                                override val arity: Int,
                                override val value: String)
    extends SDFluent(name, arity, value)
    with ComplexEvent
    with OutputEvent

//case class InstantEvent(override val name: String,
//                        override val arity: Int)
//    extends Event(name, arity) {
//
//    override val id = InstantEventId(name, arity)
//
//    override val toString = {
//        s"$name $arity"
//    }
//}
//
//case class Fluent(override val name: String,
//                  override val arity: Int,
//                  value: String)
//extends Event(name, arity) {
//
//    override val id = FluentId(name, arity, value)
//
//    override val toString = {
//        s"$name $arity = $value"
//    }
//}

