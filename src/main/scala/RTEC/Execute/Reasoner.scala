package RTEC.Execute

import java.io

import RTEC._

object Reasoner {

    // static parameters
    private var _cachingOrder: Seq[Reader.Main.CachingTarget] = _
    private var _predicates: Map[Data.EventId, Seq[Data.Predicate]] = _
    private var _inputEntities: Set[Data.InputEntity] = _
    private var _builtEntities: Seq[Data.BuiltEntity] = _
    private var _clock: Int = _

    // dynamic parameters
    private var _windowDB: EventDB = _
    private var _windowStart: Int = _
    private var _windowEnd: Int = _
    private var _entities: Map[String, Iterable[Seq[String]]] = _
    private var _input: (Seq[Reader.Main.InputHappensAt], Seq[Reader.Main.InputHoldsAt], Seq[Reader.Main.InputHoldsFor]) = _
    private var _sdPreviousPoints: Map[Data.FluentId, Map[Seq[String], Int]] = _
    private var _sfPreviousPoints: Map[Data.FluentId, Iterable[((Data.FluentId, Seq[String]), Set[Int])]] = _

    def run(staticData: (Set[Data.InstantEvent], Set[Data.Fluent], Map[Data.EventId, Seq[Data.Predicate]], (Set[Data.InputEntity], Seq[Data.BuiltEntity]), Seq[Reader.Main.CachingTarget]),
            input: (Seq[Reader.Main.InputHappensAt], Seq[Reader.Main.InputHoldsAt], Seq[Reader.Main.InputHoldsFor]),
            outputFile: String,
            start: Int, end: Int,
            windowSize: Int, windowStep: Int,
            clock: Int): Unit = {

        // Set shared parameters
        val iEMapping: Map[Data.InstantEventId, Data.InstantEvent] = staticData._1
            .map(iE => iE.id -> iE)(collection.breakOut)

        val fluentMapping: Map[Data.FluentId, Data.Fluent] = staticData._2
            .map(fluent => fluent.id -> fluent)(collection.breakOut)

        _windowDB = new EventDB(iEMapping, fluentMapping)
        _predicates = staticData._3
        _inputEntities = staticData._4._1
        _builtEntities = staticData._4._2
        _cachingOrder = staticData._5
        _input = input
        _clock = clock
        _sdPreviousPoints = Map.empty
        _sfPreviousPoints = Map.empty
        // Start event recognition loop
        val fd = new io.FileWriter(outputFile)
        _windowStart = start
        _windowEnd = start + windowSize
        var wcount = 0

        var inputParsingTIme = List.empty[Double]
        var entityCalculationTime = List.empty[Double]
        var CEReasoningTime = List.empty[Double]
        var outputWritingTime = List.empty[Double]
        var eventDiscardTIme = List.empty[Double]
        var recognitionTime = List.empty[Double]
        var inputEvents = List.empty[Int]
        var outputEvents = List.empty[Int]
        var outputIntervals = List.empty[Int]

        while (_windowEnd < end) {
            wcount += 1
            println(s"ER: ${_windowEnd} $windowSize")
            //fd.append(s"ER: ${_windowEnd} $windowSize")

            val t0 = System.nanoTime() / 1000000.0

            // Update Input data
            processInput()
            val inpEvents = _windowDB.countEvents
            //println(s"Input Events: $inpEvents")
            inputEvents +:= inpEvents

            val t1 = System.nanoTime() / 1000000.0
            inputParsingTIme +:= (t1 - t0)
            //println(s"Input parsing time: ${t1 - t0} ms")

            // Calculate all entities given the user defined patterns
            produceEntities()

            val t2 = System.nanoTime() / 1000000.0
            entityCalculationTime +:= (t2 - t1)
            //println(s"Entity production time: ${t2 - t1} ms")

            // Produce and update complex events using the user defined patterns
            processCE()

            val t3 = System.nanoTime() / 1000000.0
            CEReasoningTime +:= (t3 - t2)
            //println(s"Recognition time: ${t3 - t2} ms")

            // Write results for this window
            //println(header + _windowDB.output)
            //fd.append(_windowDB.output)
            _windowDB.output

            //val t4 = System.nanoTime() / 1000000.0
            //outputWritingTime +:= (t4 - t3)
            //println(s"Writing time: ${t4 - t3} ms")

            // Keep previous points
            _windowDB.cut(_windowStart + windowStep, _clock) match {
                case (sdp, sfp) => _sdPreviousPoints = sdp; _sfPreviousPoints = sfp
            }

            val outpEvents = _windowDB.countEvents - inpEvents
            //println(s"Output Events: $outpEvents")
            outputEvents +:= outpEvents

            val outpIntervals = _windowDB.countOutputIntervals
            //println(s"Output Intervals: $outpIntervals")
            outputIntervals +:= outpIntervals

            // Clear database
            _windowDB.clear()

            val t5 = System.nanoTime() / 1000000.0

            //eventDiscardTIme +:= (t5 - t4)
            eventDiscardTIme +:= (t5 - t3)
            //println(s"Window cut time: ${t5 - t4} ms")
            //println(s"Window cut time: ${t5 - t3} ms")

            val windowRecognitionTime = t5 - t1
            recognitionTime +:= windowRecognitionTime
            //println(s"Window recognition time: $windowRecognitionTime ms\n")

            // Increment window start point
            _windowStart += windowStep
            _windowEnd += windowStep

        }

        fd.close()

        println(s"\nAverage input events: ${inputEvents.sum / wcount}")
        println(s"\nAverage output events: ${outputEvents.sum / wcount}")
        println(s"\nAverage output intervals: ${outputIntervals.sum / wcount}")
        println(s"\nAverage input parsing time: ${inputParsingTIme.init.sum / wcount}")
        println(s"Average entity calculation time: ${entityCalculationTime.init.sum / wcount}")
        println(s"Average CE reasoning time: ${CEReasoningTime.init.sum / wcount}")
        println(s"Average event discard time: ${eventDiscardTIme.init.sum / wcount}")
        println(s"Average recognition time: ${recognitionTime.init.sum / wcount}")
    }

    private def processInput(): Unit = {
        // Process HappensAt input
        val inputHappensAt: Seq[(Data.InstantEventId, Seq[String], Int)] = _input._1
        val inMemoryHappensAt = inputHappensAt.filter {case (_, _, timepoint) =>
            timepoint > _windowStart && timepoint <= _windowEnd
        }
        val formattedHappensAt: Iterable[((Data.InstantEventId, Seq[String]), Set[Int])] = inMemoryHappensAt
            .groupBy {group =>
                (group._1, group._2)
            }
            .mapValues(_.map(_._3)(collection.breakOut))

        // Process HoldsAt input
        val inputHoldsAt: Seq[(Data.FluentId, Seq[String], Int)] = _input._2
        val inMemoryHoldsAt = inputHoldsAt.filter {case (_, _, timepoint) =>
            timepoint > _windowStart && timepoint <= _windowEnd
        }
        val formattedHoldsAt: Iterable[((Data.FluentId, Seq[String]), Data.Intervals)] = inMemoryHoldsAt
            .groupBy {group =>
                (group._1, group._2)
            }
            .mapValues(v => Data.Intervals.fromPoints(v.map(_._3)(collection.breakOut), _windowStart, _windowEnd, _clock))

        // Process HoldsFor input
        val inputHoldsFor: Seq[(Data.FluentId, Seq[String], Data.Intervals)] = _input._3
        val inMemoryHoldsFor = inputHoldsFor collect {
            case hf @(_, _, intervals) if intervals.head > _windowStart && intervals.last <= _windowEnd =>
                hf
            case hf @(_, _, intervals) if intervals.last > _windowStart && intervals.last <= _windowEnd =>
                hf.copy(_3 = intervals.restrictOn(_windowStart, _clock))
        }
        val formattedHoldsFor: Iterable[((Data.FluentId, Seq[String]), Data.Intervals)] = inMemoryHoldsFor
            .groupBy {group =>
                (group._1, group._2)
            }
            .mapValues(v => Data.Intervals.union(v.map(_._3)))

        // Update database
        _windowDB.updateIE(formattedHappensAt)
        _windowDB.updateFluent(formattedHoldsAt)
        _windowDB.updateFluent(formattedHoldsFor)
    }

    private def produceEntities(): Unit = {
        // First produce input entities
        _entities = _inputEntities.map(_.instances(_windowDB))(collection.breakOut)

        // Now produce built entities that depend on the input ones
        _entities = _builtEntities.foldLeft(_entities) {(acc, entity) =>
            acc + entity.instances(acc)
        }
    }

    private def processCE(): Unit = {
        var initiations: Iterable[((Data.FluentId, Seq[String]), Set[Int])] = Vector.empty
        var terminations: Iterable[((Data.FluentId, Seq[String]), Set[Int])] = Vector.empty
        var previousId: Option[Data.FluentId] = None
        _cachingOrder foreach {case (id, entityId) =>
            if (previousId.isDefined && id != previousId.get) {
                val combined = combineSF(initiations, terminations)
                _windowDB.updateFluent(combined)

                initiations = Vector.empty
                terminations = Vector.empty
            }
            val predicates = _predicates(id)
            val entities = _entities(entityId)
            id match {
                case _: Data.InstantEventId =>
                    // Case: Instant Event
                    predicates foreach {
                        case p: Data.IEPredicate =>
                            val results: Iterable[((Data.InstantEventId, Seq[String]), Set[Int])] = p.validate(_windowDB, entities)
                            _windowDB.updateIE(results)

                    }

                case fluentId: Data.FluentId =>
                    // Case: Fluent
                    val sm: Iterable[((Data.FluentId, Seq[String]), Set[Int])] = _sfPreviousPoints.getOrElse(fluentId, Map.empty)
                    val sdm: Map[Seq[String], Int] = _sdPreviousPoints.getOrElse(fluentId, Map.empty)

                    initiations ++= sm
                    predicates foreach {
                        case p: Data.InitiatedAtPredicate =>
                            initiations ++= p.validate(_windowDB, entities)

                        case p: Data.TerminatedAtPredicate =>
                            terminations ++= p.validate(_windowDB, entities)

                        case p: Data.SDFPredicate =>
                            val result = p.validate(_windowDB, (entities ++ sdm.keys).toSet)
                            val corrected = result.map{
                                case r @(_, intervals) =>
                                    r.copy(_2 = intervals.applyFrame(_windowStart))
                            }.collect{
                                case r @((f,entity), intervals) if sdm.contains(entity) && intervals.nonEmpty =>
                                    //println(s"$f $entity Window ${_windowStart} - ${_windowEnd} with pending point ${sdm(entity)}: $intervals")
                                    r.copy(_2 = intervals.amalgamate(sdm(entity), _windowStart + _clock))

                                case r @((f,e), intervals) if intervals.nonEmpty =>
                                    //println(s"$f $e Window ${_windowStart} - ${_windowEnd}: $intervals")
                                    //r.copy(_2 = intervals.discard(_windowStart + _windowStep))
                                    r
                            }
                            _windowDB.updateFluent(corrected)

                    }

                    previousId = Some(Data.FluentId(fluentId.name, fluentId.numOfArgs, "_"))
            }

        }

        val combined = combineSF(initiations, terminations)
        _windowDB.updateFluent(combined)
    }

    private def combineSF(initiations: Iterable[((Data.FluentId, Seq[String]), Set[Int])], terminations: Iterable[((Data.FluentId, Seq[String]), Set[Int])]): Iterable[((Data.FluentId, Seq[String]), Data.Intervals)] = {
        if (initiations.isEmpty)
            return Vector.empty

        val init: Map[(Data.FluentId, Seq[String]), Set[Int]] = initiations
            .groupBy(_._1)
            .mapValues (_ .flatMap(_._2)(collection.breakOut))

        val term: Map[(Data.FluentId, Seq[String]), Set[Int]] = terminations
            .groupBy(_._1)
            .mapValues (_ .flatMap(_._2)(collection.breakOut))

        val ids: Set[Data.FluentId] = init.keySet map (_._1)

        // All possible holdsFor results can only be as much as the number of all the different initiations
        init map {i: ((Data.FluentId, Seq[String]), Set[Int]) =>
            val entityInitiations: Set[Int] = i._2

            val otherIds: Set[Data.FluentId] = ids - i._1._1
            val entityTerminations: Set[Int] = term.getOrElse(i._1, Set.empty) ++ {
                otherIds flatMap {id =>
                    init.getOrElse((id, i._1._2), Set.empty)
                }
            }

            i._1 -> Data.Intervals.combine(entityInitiations, entityTerminations, _clock)
        }

    }

}
