import java.io._
import scala.io._

object Main{
  var output_def = List[String]()
  var flag: Int = 0
  var flag_2: Int = 0
  var line_memory: String = ""
  var count: Int = 0

  def main(args: Array[String]): Unit = {
    var path = args(0)
    var path_output = args(1)
    val output = new PrintWriter(path_output)
    if(new File(path).isDirectory){
      if(!(path endsWith "/")) path = path + "/"
      val files = new File(path).list()
      files.foreach { e =>
        findDirectorys(e, path)
      }
    }
    else{
      println("Please set directory path.")
    }
    println("complete..")
    System.exit(0)
  
    def findDirectorys(file: String, sub_path: String):Unit = {
      if(new File(sub_path + file).isDirectory){
        new File(sub_path + file).list().foreach { e =>
          findDirectorys(e, sub_path + "/" + file + "/")
        }
      }
      else{
        var source = Source.fromFile(sub_path + file, "ISO-8859-1")
        try{
          source.getLines().foreach { line: String =>
            if(flag == 0){
              line_memory = line
            }
            if(line.contains("def")){
              line_memory = line
              flag = 1
              flag_2 = 1
            }

            var r = "def\\s+(.+?)\\((.+?)\\)\\s*:\\s*(.*?)\\s*\\=\\s*\\{".r
            for(m <- r.findAllIn(line_memory).matchData){
              m match {
                case r(a, b, c) =>
                  //println(c + "=" + a + "(" + b + ")")
                  var output_def_str = c + "=" + a + "(" + b + ")\n"
                  output_def = output_def :+ output_def_str
              }
              flag = 2
            }
            if(flag == 1){
              line_memory.replaceAll("\\n", "")
              line_memory.replaceAll("\\r", "")
              line_memory.replaceAll("\\s", "")
              line_memory.replaceAll("\\t", "")
              line_memory.replaceAll(" ", "")
              if(flag_2 == 1){
                line_memory = line_memory
                flag_2 = 2
              }
              else{
                line_memory = line_memory + line
                flag_2 = 2
              }
            }
            else if(flag == 2){
              flag = 0
            }
          }
        }
        finally{
          println("file:" + file + " is end")
          output_def.foreach{line: String => output.write(line)}
          flag = 0
          flag_2 = 0
          line_memory = ""
          output_def = List.empty[String]
          source.close()
        }
      }
    }
  }
}
