package RTEC.Data


sealed trait EventId {
    def ==(x: EventId): Boolean
    def !=(x: EventId): Boolean
}

case class InstantEventId(name: String, numOfArgs: Int) extends EventId {
    def ==(x: EventId): Boolean = x match {
        case InstantEventId(n, noa) =>
            (name == n) && (numOfArgs == noa)
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


sealed trait EventType

sealed trait IEType extends EventType
case object InputIE extends IEType
case object OutputIE extends IEType

sealed trait FluentType extends EventType
case object SimpleFluent extends FluentType
case object InputSDFluent extends FluentType
case object OutputSDFluent extends FluentType

// Event root type
sealed abstract class Event(val eventType: EventType, val name: String, val numOfArgs: Int) {
    def id: EventId
}
    
case class InstantEvent(override val eventType: IEType,
                        override val name: String,
                        override val numOfArgs: Int)
    extends Event(eventType, name, numOfArgs) {

    override val id = InstantEventId(name, numOfArgs)

    override val toString = {
        s"$name $numOfArgs"
    }
}

case class Fluent(override val eventType: FluentType,
                  override val name: String,
                  override val numOfArgs: Int,
                  value: String)
    extends Event(eventType, name, numOfArgs) {

    override val id = FluentId(name, numOfArgs, value)

    override val toString = {
        s"$name $numOfArgs = $value"
    }
}

