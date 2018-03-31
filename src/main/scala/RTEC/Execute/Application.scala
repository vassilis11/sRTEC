package RTEC.Execute

object Application extends App {

    // Set program parameters
    //Caviar
    val inputDir = "./input/caviar"
    val outputFile = s"$inputDir/results.txt"
    val windowSize = 1007000
    val windowStep =  1007000
    val start = 0
    val end = 2014001
    val clock = 40

    // CTM
  /*
    val inputDir = "./input/ctm"
    val outputFile = s"$inputDir/results.txt"
    val windowSize = 50000
    val windowStep =  50000
    val start = 0
    val end = 100001
    val clock = 1
    */

    //Simple Example
    /*
    val inputDir = "./input/Simple Example"
    val outputFile = s"$inputDir/results.txt"
    val windowSize = 24
    val windowStep = 24
    val start = 0
    val end = 73
    val clock = 1
    */

    //Caviar long
    /*
    val inputDir = "./input/caviar_long"
    val outputFile = s"$inputDir/results.txt"
    val windowSize = 1007000
    val windowStep = 1007000
    val start = 0
    val end = 2014001
    val clock = 40
    */

    // CTM long
  /*
    val inputDir = "./input/ctm_long"
    val outputFile = s"$inputDir/results.txt"
    val windowSize = 50000
    val windowStep =  50000
    val start = 0
    val end = 100001
    val clock = 1
    */

    //val executions = 5
    execute()

    private def execute(): Unit = {
        // Read input files
        Reader.Main.readDeclarations(s"$inputDir/declarations.txt")
        Reader.Main.readDefinitions(s"$inputDir/definitions.txt")
        Reader.Main.readDataset(s"$inputDir/dataset.txt")

        // Parse static data to the internal structures
        val staticData = Reader.Main.staticData match {
            case Some(sd) => sd
            case _ => println("Error while reading static data"); null
        }

        /*

        println(collectionOutputFormat("Instant Events", staticData._1))
        println(collectionOutputFormat("Fluents", staticData._2))
        println(collectionOutputFormat("cachingOrder", staticData._5))
        println(collectionOutputFormat("Predicates",
            staticData._3.values.flatten map {p =>
                collectionOutputFormat(p.head.toString, p.body, 2)
            }))
        */
        // Initialize database
        val input = Reader.Main.realTime match {
            case Some(d) => d
            case _ => println("Error while reading dataset"); null
        }

        // Start event recognition
        Reasoner.run(staticData, input, outputFile, start, end, windowSize, windowStep, clock)

    }


    def collectionOutputFormat(name: String, data: Iterable[Any], indent: Int = 1): String = {
        if (data.nonEmpty) s"$name:\n${data.foldLeft("")((str, token) => str + s"${"\t" * indent}$token\n")}"
        else ""
    }

}

