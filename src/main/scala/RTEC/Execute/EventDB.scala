package RTEC.Execute

import RTEC._

import scala.collection.mutable

class EventDB(val iEs: Map[Data.InstantEventId, Data.IEType],
              val fluents: Map[Data.FluentId, Data.FluentType]) extends Serializable {

    private val _iETime: Map[Data.InstantEventId, mutable.Map[Seq[String], Set[Int]]] = iEs.keys
        .map {ie =>
            ie -> mutable.Map.empty[Seq[String], Set[Int]]
        }(collection.breakOut)

    private val _fluentTime: Map[Data.FluentId, mutable.Map[Seq[String], Data.Intervals]] = fluents.keys
        .map {fluent =>
            fluent -> mutable.Map.empty[Seq[String], Data.Intervals]
        }(collection.breakOut)


    override def toString: String = {
        val IEStr = {
            for {
                ie <- _iETime
                data <- ie._2
            }
                yield s"${ie._1.name}(${data._1.mkString(",")}),[${data._2.mkString(",")}]"

        }.mkString("\n")

        val fluentStr = {
            for {
                fluent <- _fluentTime
                data <- fluent._2
            }
                yield s"${fluent._1.name}(${data._1.mkString(",")})=${fluent._1.value},[${data._2}]"

        }.mkString("\n")

        IEStr + "\n" + fluentStr + "\n"
    }

    def output: String = {
        val IEStr = {
            for {
                ie <- _iETime if iEs(ie._1) == Data.OutputIE
                data <- ie._2
            }
                yield s"${ie._1.name}(${data._1.mkString(",")}),[${data._2.mkString(",")}]"

        }.mkString("\n")

        val fluentStr = {
            for {
                fluent <- _fluentTime if fluents(fluent._1) == Data.SimpleFluent || fluents(fluent._1) == Data.OutputSDFluent
                data <- fluent._2
            }
                yield s"${fluent._1.name}(${data._1.mkString(",")})=${fluent._1.value},[${data._2}]"

        }.mkString("\n")

        IEStr + "\n" + fluentStr + "\n"
    }

    // Queries the database for the given pattern and returns any possible entity that matches
    def getEntities(id: Data.EventId, pattern: Seq[String] = null): Iterable[Seq[String]] = {
        id match {
            case iEId: Data.InstantEventId =>
                if (pattern != null) {
                    val allEntities: Iterable[Seq[String]] = _iETime(iEId).keys
                    allEntities filter {_
                        .zipWithIndex
                        .forall {pair =>
                            Data.Clause.isWildCard(pair._1) || pair._1 == pattern(pair._2)
                        }
                    }

                } else
                    _iETime(iEId).keys

            case fluentId: Data.FluentId =>
                if (pattern != null) {
                    val allEntities: Iterable[Seq[String]] = _fluentTime(fluentId).keys
                    allEntities filter {_
                        .zipWithIndex
                        .forall {pair =>
                            val cmp = pattern(pair._2)
                            Data.Clause.isWildCard(cmp) || pair._1 == cmp
                        }
                    }

                } else
                    _fluentTime(fluentId).keys

        }
    }

    // Queries the database for the given IE instance and returns any correspondent time data
    def getIETime(id: Data.InstantEventId, entity: Seq[String]): Iterable[(Seq[String], Set[Int])] = {
        // If there is a variable on the entity query the filtering must be done by hand to match more than one result if needed
        if (entity exists (x => Data.Clause.isWildCard(x) || Data.Clause.isVariable(x))) {
            val allEntities: mutable.Map[Seq[String], Set[Int]] = _iETime(id)
            allEntities
                .filterKeys {_
                    .zip(entity)
                    .forall {pair =>
                        pair._1 == pair._2 || Data.Clause.isWildCard(pair._2) || Data.Clause.isVariable(pair._2)
                    }
                }
                .view

        } else {
            _iETime(id) get entity match {
                case Some(t) =>
                    Iterable((entity, t))

                case None =>
                    Iterable((entity, Set.empty[Int]))
            }
        }
    }

    // Queries the database for the given fluent instance and returns any correspondent time data
    def getFluentTime(id: Data.FluentId, entity: Seq[String]): Iterable[(Seq[String], Data.Intervals)] = {
        if (entity exists (x => Data.Clause.isWildCard(x) || Data.Clause.isVariable(x))) {
            val allEntities: mutable.Map[Seq[String], Data.Intervals] = _fluentTime(id)
            allEntities
                .filterKeys {_
                    .zip(entity)
                    .forall {pair =>
                        pair._1 == pair._2 || Data.Clause.isWildCard(pair._2) || Data.Clause.isVariable(pair._2)
                    }
                }
                .view

        } else
            Iterable((entity, _fluentTime(id).getOrElse(entity, Data.Intervals.empty)))

    }

    def updateIE(input: Iterable[((Data.InstantEventId, Seq[String]), Set[Int])]): Unit = {
        input foreach {ie =>
            val m: mutable.Map[Seq[String], Set[Int]] = _iETime(ie._1._1)
            val oldTime: Set[Int] = m.getOrElse(ie._1._2, Set.empty)
            m += (ie._1._2 -> (oldTime ++ ie._2))
        }
    }

    def updateFluent(input: Iterable[((Data.FluentId, Seq[String]), Data.Intervals)]): Unit = {
        input foreach {fluent =>
            val m: mutable.Map[Seq[String], Data.Intervals] = _fluentTime(fluent._1._1)
            val oldIntervals = m.getOrElse(fluent._1._2, Data.Intervals.empty)
            m += (fluent._1._2 -> (oldIntervals | fluent._2))
        }
    }

    /* Takes a "threshold" timepoint and returns a map with any possible intervals that take place during that time.
       This is useful for the event discard step
     */
    def cut(threshold: Int, delay: Int): (Map[Data.FluentId, Map[Seq[String], Int]], Map[Data.FluentId, Iterable[((Data.FluentId, Seq[String]), Set[Int])]]) = {
        val sd: Map[Data.FluentId, Map[Seq[String], Int]] =
            _fluentTime.collect {case (id, data) if fluents(id) != Data.SimpleFluent =>
                val d: Map[Seq[String], Int] = data
                    .mapValues(_.cut(threshold))
                    .collect{case (key, Some(value)) => key -> value}(collection.breakOut)

                (id, d)
            }

        val simple: Map[Data.FluentId, Iterable[((Data.FluentId, Seq[String]), Set[Int])]] =
            _fluentTime collect {case (id, data) if fluents(id) == Data.SimpleFluent =>
                val d = data.view
                    .map {case (k, v) =>
                        val unfinished = v.cut(threshold)
                        ((id, k), unfinished)
                    }
                    .filter(_._2.isDefined)
                    .collect{case (k, Some(i)) =>
                        (k, Set(i - delay))
                    }
                    .force

                (id, d)
            }

        (sd, simple)
    }

    def clear(): Unit = {
        _iETime.values foreach (_.clear())
        _fluentTime.values foreach (_.clear())
    }

    def countEvents: Int = {
        _iETime.values.map(_.size).sum + _fluentTime.values.map(_.size).sum
    }
}