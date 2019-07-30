import java.io.{BufferedWriter, FileWriter}


case class CSVWriter(constructs: Constructs) {

  def write(res: Array[(Int, (Int, Int))]): Unit = {
    try {
      lazy val outputFile = new BufferedWriter(new FileWriter(s"output_E${constructs.epsilon}_$constructs.netSize.csv", true))
      outputFile.newLine()
      outputFile.write("Exits, Agents_Correct, Agents_Errors")
      res.foreach(s => {
        outputFile.write(s._1.toString + ", " + s._2._1.toString + ", " + s._2._2.toString)
        outputFile.newLine()
      })
      outputFile.close()
    } catch {
      case e: Exception => print(e.getCause)
    }
  }


}
