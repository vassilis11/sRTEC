package RTEC.Data

import RTEC._

object Clause {
    def isVariable(input: String): Boolean = {
        Character.isUpperCase(input.head)
    }

    def isWildCard(input: String): Boolean = {
        input.head == '_'
    }
}

trait EntityContainer {
    def id: Data.EventId
    def entity: Seq[String]
}

trait Clause {
    // Replace any occurrence of the target symbol with a new one
    def replaceLabel(target: String, newLabel: String): Clause
}
trait HeadClause extends Clause with EntityContainer {
    override def replaceLabel(target: String, newLabel: String): HeadClause
}

trait BodyClause extends Clause {
    override def replaceLabel(target: String, newLabel: String): BodyClause

    // Takes as input all runtime event variables and checks if they are valid through the given clause
    def resolve(data: Execute.EventDB, dict: Iterable[Predicate.GroundingDict]): Iterable[Predicate.GroundingDict]
}

case class UnionAll(input: Seq[String], result: String, strict: Boolean)
    extends BodyClause {
    override val toString = {
        s"Union_All ${if (strict) "!" else ""} [${input.mkString(", ")}] $result"
    }

    override def resolve(data: Execute.EventDB, dict: Iterable[Predicate.GroundingDict]): Iterable[Predicate.GroundingDict] = {
        if (strict) {
            dict
                .map {labels: Predicate.GroundingDict =>
                    val intervalsDict: Map[String, Intervals] = labels._3
                    val inputIntervals: Seq[Intervals] = input map {arg =>
                        if (intervalsDict contains arg)
                            intervalsDict(arg)
                        else
                            Intervals.fromString(arg)
                    }
                    val union = Intervals.union(inputIntervals)

                    if (union.isEmpty)
                        null
                    else
                        labels.copy(_3 = intervalsDict + (result -> union))
                }
                .filter(_ != null)

        } else {
            dict map {labels: Predicate.GroundingDict =>
                val intervalsDict: Map[String, Intervals] = labels._3
                val inputIntervals: Seq[Intervals] = input map {arg =>
                    if (intervalsDict contains arg)
                        intervalsDict(arg)
                    else
                        Intervals.fromString(arg)
                }
                val union = Intervals.union(inputIntervals)

                labels.copy(_3 = intervalsDict + (result -> union))
            }
        }
    }

    override def replaceLabel(target: String, newLabel: String): UnionAll = this
}

case class ComplementAll(input: Seq[String], result: String, strict: Boolean)
    extends BodyClause {
    override val toString = {
        s"Complement_All [${input.mkString(", ")}] $result"
    }

    override def resolve(data: Execute.EventDB, dict: Iterable[Predicate.GroundingDict]): Iterable[Predicate.GroundingDict] = {
        if (strict) {
            dict
                .map {labels: Predicate.GroundingDict =>
                    val intervalsDict: Map[String, Intervals] = labels._3
                    val inputIntervals: Seq[Intervals] = input map { arg =>
                        if (intervalsDict contains arg)
                            intervalsDict(arg)
                        else
                            Intervals.fromString(arg)
                    }
                    val union = Intervals.union(inputIntervals)
                    val complement = Intervals.complement(union)

                    if (complement.isEmpty)
                        null
                    else
                        labels.copy(_3 = intervalsDict + (result -> complement))
                }
                .filter(_ != null)

        } else {
            dict map {labels: Predicate.GroundingDict =>
                val intervalsDict: Map[String, Intervals] = labels._3
                val inputIntervals: Seq[Intervals] = input map { arg =>
                    if (intervalsDict contains arg)
                        intervalsDict(arg)
                    else
                        Intervals.fromString(arg)
                }
                val union = Intervals.union(inputIntervals)
                val complement = Intervals.complement(union)

                labels.copy(_3 = intervalsDict + (result -> complement))
            }
        }
    }

    override def replaceLabel(target: String, newLabel: String): ComplementAll = this
}

case class IntersectAll(input: Seq[String], result: String, strict: Boolean)
    extends BodyClause {
    override val toString = {
        s"Intersect_All ${if (strict) "!" else ""} [${input.mkString(", ")}] $result"
    }

    override def resolve(data: Execute.EventDB, dict: Iterable[Predicate.GroundingDict]): Iterable[Predicate.GroundingDict] = {
        if (strict) {
            dict
                .map {labels: Predicate.GroundingDict =>
                    val intervalsDict: Map[String, Intervals] = labels._3
                    val inputIntervals: Seq[Intervals] = input map {arg =>
                        if (intervalsDict contains arg)
                            intervalsDict(arg)
                        else
                            Intervals.fromString(arg)
                    }
                    val intersection = Intervals.intersect(inputIntervals)

                    if (intersection.isEmpty)
                        null
                    else
                        labels.copy(_3 = intervalsDict + (result -> intersection))
                }
                .filter(_ != null)

        } else {
            dict map {labels: Predicate.GroundingDict =>
                val intervalsDict: Map[String, Intervals] = labels._3
                val inputIntervals: Seq[Intervals] = input map {arg =>
                    if (intervalsDict contains arg)
                        intervalsDict(arg)
                    else
                        Intervals.fromString(arg)
                }
                val intersection = Intervals.intersect(inputIntervals)

                labels.copy(_3 = intervalsDict + (result -> intersection))
            }
        }
    }

    override def replaceLabel(target: String, newLabel: String): IntersectAll = this
}

case class RelativeComplementAll(baseInput: String, excludedInput: Seq[String], result: String, strict: Boolean)
    extends BodyClause {
    override val toString = {
        s"Relative_Complement_All ${if (strict) "!" else ""} $baseInput [${excludedInput.mkString(", ")}] $result"
    }

    override def resolve(data: Execute.EventDB, dict: Iterable[Predicate.GroundingDict]): Iterable[Predicate.GroundingDict] = {
        if (strict) {
            dict
                .map {labels: Predicate.GroundingDict =>
                    val intervalsDict: Map[String, Intervals] = labels._3
                    val baseInputIntervals: Intervals = intervalsDict.getOrElse(baseInput, Intervals.fromString(baseInput))
                    val excludedInputIntervals: Seq[Intervals] = excludedInput map {arg =>
                        if (intervalsDict contains arg)
                            intervalsDict(arg)
                        else
                            Intervals.fromString(arg)
                    }
                    val unionOfExcluded = Intervals.union(excludedInputIntervals)
                    val complementOfExcluded = Intervals.complement(unionOfExcluded)
                    val relativeComplement = complementOfExcluded & baseInputIntervals

                    if (relativeComplement.isEmpty)
                        null
                    else
                        labels.copy(_3 = intervalsDict + (result -> relativeComplement))
                }
                .filter(_ != null)

        } else {
            dict map {labels: Predicate.GroundingDict =>
                val intervalsDict: Map[String, Intervals] = labels._3
                val baseInputIntervals: Intervals = intervalsDict.getOrElse(baseInput, Intervals.fromString(baseInput))
                val excludedInputIntervals: Seq[Intervals] = excludedInput map {arg =>
                    if (intervalsDict contains arg)
                        intervalsDict(arg)
                    else
                        Intervals.fromString(arg)
                }
                val unionOfExcluded = Intervals.union(excludedInputIntervals)
                val complementOfExcluded = Intervals.complement(unionOfExcluded)
                val relativeComplement = complementOfExcluded & baseInputIntervals

                labels.copy(_3 = intervalsDict + (result -> relativeComplement))
            }
        }
    }

    override def replaceLabel(target: String, newLabel: String): RelativeComplementAll = this
}

// happensAt
case class HappensAtIE(id: InstantEventId, entity: Seq[String], time: String)
    extends HeadClause
    with BodyClause
    with EntityContainer {

    override val toString = {
        s"HappensAt [${id.name} ${entity.mkString(" ")}] $time"
    }

    override def resolve(data: Execute.EventDB, dict: Iterable[Predicate.GroundingDict]): Iterable[Predicate.GroundingDict] = {
        dict flatMap {labels =>
            val entityDict = labels._2
            val timePointsDict = labels._4
            val groundedEntity = entity.map(arg => entityDict.getOrElse(arg, arg))

            var response: Iterable[(Seq[String], Set[Int])] = data.getIETime(id, groundedEntity)

            if (timePointsDict contains time) {
                // Compare with grounded values
                val gtime = timePointsDict(time)
                response = response
                    .map(x => x.copy(_2 = gtime & x._2))
                    .filter(_._2.nonEmpty)
            }

            response.map{case (e, t) =>
                val additions = groundedEntity
                    .zip(e)
                    .filter(x => Clause.isVariable(x._1))

                labels.copy(_2 = entityDict ++ additions, _4 = timePointsDict + (time -> t))
            }
        }
    }

    override def replaceLabel(target: String, withLabel: String): HappensAtIE = {
        val argsIndex = entity indexOf target
        val newEntity =
            if (argsIndex != -1)
                entity updated(argsIndex, withLabel)
            else
                entity

        val newTime =
            if (time == target)
                withLabel
            else
                time

        HappensAtIE(id, newEntity, newTime)
    }
}

// initiatedAt
case class InitiatedAt(id: FluentId, entity: Seq[String], time: String)
    extends HeadClause
    with EntityContainer {

    override val toString = {
        s"InitiatedAt [${id.name} ${entity.mkString(" ")} = ${id.value}] $time"
    }

    override def replaceLabel(target: String, withLabel: String): InitiatedAt = {
        val newId =
            if (id.value == target)
                FluentId(id.name, id.numOfArgs, withLabel)
            else
                id

        val argsIndex = entity indexOf target
        val newEntity =
            if (argsIndex != -1)
                entity updated(argsIndex, withLabel)
            else
                entity

        val newTime =
            if (time == target)
                withLabel
            else
                time

        InitiatedAt(newId, newEntity, newTime)
    }
}

// terminatedAt
case class TerminatedAt(id: FluentId, entity: Seq[String], time: String)
    extends HeadClause
    with EntityContainer {

    override val toString = {
        s"TerminatedAt [${id.name} ${entity.mkString(" ")} = ${id.value}] $time"
    }

    override def replaceLabel(target: String, withLabel: String): TerminatedAt = {
        val newId =
            if (id.value == target)
                FluentId(id.name, id.numOfArgs, withLabel)
            else
                id

        val argsIndex = entity indexOf target
        val newEntity =
            if (argsIndex != -1)
                entity updated(argsIndex, withLabel)
            else
                entity

        val newTime =
            if (time == target)
                withLabel
            else
                time

        TerminatedAt(newId, newEntity, newTime)
    }
}

// holdsFor
case class HoldsFor(id: FluentId, entity: Seq[String], time: String, strict: Boolean)
    extends HeadClause
    with BodyClause
    with EntityContainer {

    override val toString = {
        s"HoldsFor ${if (strict) "!" else ""} [${id.name} ${entity.mkString(" ")} = ${id.value}] $time"
    }

    override def resolve(data: Execute.EventDB, dict: Iterable[Predicate.GroundingDict]): Iterable[Predicate.GroundingDict] = {
        if (strict) {
            dict flatMap {labels =>
                val entityDict = labels._2
                val intervalsDict = labels._3
                val groundedEntity = entity.map(arg => entityDict.getOrElse(arg, arg))

                val response = data.getFluentTime(id, groundedEntity)

                response.collect{case (e, t) if t.nonEmpty =>
                    val additions = groundedEntity
                        .zip(e)
                        .filter(x => Clause.isVariable(x._1))

                    labels.copy(_2 = entityDict ++ additions, _3 = intervalsDict + (time -> t))
                }
            }

        } else {
            dict flatMap {labels =>
                val entityDict = labels._2
                val intervalsDict = labels._3
                val groundedEntity = entity.map(arg => entityDict.getOrElse(arg, arg))

                val response = data.getFluentTime(id, groundedEntity)

                response.map{case (e, t) =>
                    val additions = groundedEntity
                        .zip(e)
                        .filter(x => Clause.isVariable(x._1))

                    labels.copy(_2 = entityDict ++ additions, _3 = intervalsDict + (time -> t))
                }
            }
        }
    }

    override def replaceLabel(target: String, withLabel: String): HoldsFor = {
        val newId =
            if (id.value == target)
                FluentId(id.name, id.numOfArgs, withLabel)
            else
                id

        val argsIndex = entity indexOf target
        val newEntity =
            if (argsIndex != -1)
                entity updated(argsIndex, withLabel)
            else
                entity

        val newTime =
            if (time == target)
                withLabel
            else
                time

        HoldsFor(newId, newEntity, newTime, strict)
    }
}

case class HoldsAt(id: FluentId, entity: Seq[String], time: String)
    extends BodyClause
    with EntityContainer {

    override val toString = {
        s"HoldsAt [${id.name} ${entity.mkString(" ")} = ${id.value}] $time"
    }

    override def resolve(data: Execute.EventDB, dict: Iterable[Predicate.GroundingDict]): Iterable[Predicate.GroundingDict] = {
        dict flatMap {labels =>
            val entityDict = labels._2
            val timePointsDict = labels._4
            val groundedEntity = entity.map(arg => entityDict.getOrElse(arg, arg))

            val response: Iterable[(Seq[String], Intervals)] = data.getFluentTime(id, groundedEntity)
            val gtime = timePointsDict(time)
            response
                .map(x => x.copy(_2 = gtime filter x._2.contains))
                .collect {case (e, t) if t.nonEmpty =>
                    val additions = groundedEntity
                        .zip(e)
                        .filter(x => Clause.isVariable(x._1))

                    labels.copy(_2 = entityDict ++ additions, _4 = timePointsDict + (time -> t))
                }
        }
    }

    override def replaceLabel(target: String, withLabel: String): HoldsAt = {
        val newId =
            if (id.value == target)
                FluentId(id.name, id.numOfArgs, withLabel)
            else
                id

        val argsIndex = entity indexOf target
        val newEntity =
            if (argsIndex != -1)
                entity updated(argsIndex, withLabel)
            else
                entity

        val newTime =
            if (time == target)
                withLabel
            else
                time

        HoldsAt(newId, newEntity, newTime)
    }

}

case class HappensAtFluentStart(id: FluentId, entity: Seq[String], time: String)
    extends BodyClause
    with EntityContainer {

    override val toString = {
        s"HappensAt Start [${id.name} ${entity.mkString(" ")} = ${id.value}] $time"
    }

    override def resolve(data: Execute.EventDB, dict: Iterable[Predicate.GroundingDict]): Iterable[Predicate.GroundingDict] = {
        dict flatMap {labels =>
            val entityDict = labels._2
            val timePointsDict = labels._4
            val groundedEntity = entity.map(arg => entityDict.getOrElse(arg, arg))

            val response: Iterable[(Seq[String], Intervals)] = data.getFluentTime(id, groundedEntity)
            val results =
                if (timePointsDict contains time) {
                    val gtime = timePointsDict(time)
                    response map (x => x.copy(_2 = gtime & x._2.startPoints))
                } else
                    response map (x => x.copy(_2 = x._2.startPoints))

            results.collect{case (e, t) if t.nonEmpty =>
                val additions = groundedEntity
                    .zip(e)
                    .filter(x => Clause.isVariable(x._1))

                labels.copy(_2 = entityDict ++ additions, _4 = timePointsDict + (time -> t))
            }
        }
    }

    override def replaceLabel(target: String, withLabel: String): HappensAtFluentStart = {
        val newId =
            if (id.value == target)
                FluentId(id.name, id.numOfArgs, withLabel)
            else
                id

        val argsIndex = entity indexOf target
        val newEntity =
            if (argsIndex != -1)
                entity updated(argsIndex, withLabel)
            else
                entity

        val newTime =
            if (time == target)
                withLabel
            else
                time

        HappensAtFluentStart(newId, newEntity, newTime)
    }
}

case class HappensAtFluentEnd(id: FluentId, entity: Seq[String], time: String)
    extends BodyClause
    with EntityContainer {

    override val toString = {
        s"HappensAt End [${id.name} ${entity.mkString(" ")} = ${id.value}] $time"
    }

    override def resolve(data: Execute.EventDB, dict: Iterable[Predicate.GroundingDict]): Iterable[Predicate.GroundingDict] = {
        dict flatMap {labels =>
            val entityDict = labels._2
            val timePointsDict = labels._4
            val groundedEntity = entity.map(arg => entityDict.getOrElse(arg, arg))

            val response: Iterable[(Seq[String], Intervals)] = data.getFluentTime(id, groundedEntity)
            val results =
                if (timePointsDict contains time) {
                    val gtime = timePointsDict(time)
                    response map (x => x.copy(_2 = gtime & x._2.endPoints))
                } else
                    response map (x => x.copy(_2 = x._2.endPoints))

            results.collect{case (e, t) if t.nonEmpty =>
                val additions = groundedEntity
                    .zip(e)
                    .filter(x => Clause.isVariable(x._1))

                labels.copy(_2 = entityDict ++ additions, _4 = timePointsDict + (time -> t))
            }
        }
    }

    override def replaceLabel(target: String, withLabel: String): HappensAtFluentEnd = {
        val newId =
            if (id.value == target)
                FluentId(id.name, id.numOfArgs, withLabel)
            else
                id

        val argsIndex = entity indexOf target
        val newEntity =
            if (argsIndex != -1)
                entity updated(argsIndex, withLabel)
            else
                entity

        val newTime =
            if (time == target)
                withLabel
            else
                time

        HappensAtFluentEnd(newId, newEntity, newTime)
    }
}

case class NotHappensAtIE(id: InstantEventId, entity: Seq[String], time: String)
    extends BodyClause
    with EntityContainer {

    override val toString = {
        s"Not HappensAt [${id.name} ${entity.mkString(" ")}] $time"
    }

    override def resolve(data: Execute.EventDB, dict: Iterable[Predicate.GroundingDict]): Iterable[Predicate.GroundingDict] = {
        dict flatMap {labels =>
            val entityDict = labels._2
            val timePointsDict = labels._4
            val groundedEntity = entity.map(arg => entityDict.getOrElse(arg, arg))

            val response: Iterable[(Seq[String], Set[Int])] = data.getIETime(id, groundedEntity)
            val gtime = timePointsDict(time)
            response
                .map(x => x.copy(_2 = gtime -- x._2))
                .collect {case (e, t) if t.nonEmpty =>
                    val additions = groundedEntity
                        .zip(e)
                        .filter(x => Clause.isVariable(x._1))

                    labels.copy(_2 = entityDict ++ additions, _4 = timePointsDict + (time -> t))
                }
        }
    }

    override def replaceLabel(target: String, withLabel: String): NotHappensAtIE = {
        val argsIndex = entity indexOf target
        val newEntity =
            if (argsIndex != -1)
                entity updated(argsIndex, withLabel)
            else
                entity

        val newTime =
            if (time == target)
                withLabel
            else
                time

        NotHappensAtIE(id, newEntity, newTime)
    }
}

case class NotHoldsAt(id: FluentId, entity: Seq[String], time: String)
    extends BodyClause
    with EntityContainer {

    override val toString = {
        s"Not HoldsAt [${id.name} ${entity.mkString(" ")} = ${id.value}] $time"
    }

    override def resolve(data: Execute.EventDB, dict: Iterable[Predicate.GroundingDict]): Iterable[Predicate.GroundingDict] = {
        dict flatMap {labels =>
            val entityDict = labels._2
            val timePointsDict = labels._4
            val groundedEntity = entity.map(arg => entityDict.getOrElse(arg, arg))

            val response: Iterable[(Seq[String], Intervals)] = data.getFluentTime(id, groundedEntity)

            val gtime = timePointsDict(time)
            response
                .map(x => x.copy(_2 = gtime filterNot x._2.contains))
                .collect {case (e, t) if t.nonEmpty =>
                    val additions = groundedEntity
                        .zip(e)
                        .filter(x => Clause.isVariable(x._1))

                    labels.copy(_2 = entityDict ++ additions, _4 = timePointsDict + (time -> t))
                }
        }
    }

    override def replaceLabel(target: String, withLabel: String): NotHoldsAt = {
        val newId =
            if (id.value == target)
                FluentId(id.name, id.numOfArgs, withLabel)
            else
                id

        val argsIndex = entity indexOf target
        val newEntity =
            if (argsIndex != -1)
                entity updated(argsIndex, withLabel)
            else
                entity

        val newTime =
            if (time == target)
                withLabel
            else
                time

        NotHoldsAt(newId, newEntity, newTime)
    }

}

